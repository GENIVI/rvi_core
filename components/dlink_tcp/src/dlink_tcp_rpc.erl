%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(dlink_tcp_rpc).
-behavior(gen_server).

-export([handle_rpc/2]).
-export([handle_notification/2]).
-export([handle_socket/6]).
-export([handle_socket/5]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_json_server/0]).
-export([start_connection_manager/0]).

%% Invoked by service discovery
%% FIXME: Should be rvi_service_discovery behavior
-export([service_available/4,
	 service_unavailable/4]).

-export([setup_data_link/3,
	 disconnect_data_link/2,
	 send_data/3]).


-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").

-define(PERSISTENT_CONNECTIONS, persistent_connections).
-define(DEFAULT_BERT_RPC_PORT, 9999).
-define(DEFAULT_RECONNECT_INTERVAL, 5000).
-define(DEFAULT_BERT_RPC_ADDRESS, "0.0.0.0").
-define(DEFAULT_PING_INTERVAL, 300000).  %% Five minutes
-define(SERVER, ?MODULE). 

-define(CONNECTION_TABLE, rvi_dlink_tcp_connections).
-define(SERVICE_TABLE, rvi_dlink_tcp_services).

-record(service_entry, {
	  service = [],           %% Name of service
	  connection = undefined  %% PID of connection that can reach this service
	 }).

-record(st, { 
	  cs = #component_spec{}
	 }).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?info("dlink_tcp:init(): Called"),
    %% Dig out the bert rpc server setup

    ets:new(?SERVICE_TABLE, [ duplicate_bag,  public, named_table, 
			     { keypos, #service_entry.service }]),

    ets:new(?CONNECTION_TABLE, [ duplicate_bag,  public, named_table, 
				 { keypos, #service_entry.connection }]),

    {ok, #st { 
	    cs = rvi_common:get_component_specification()
	   }
    }.

start_json_server() ->
    rvi_common:start_json_rpc_server(data_link, ?MODULE, dlink_tcp_sup).


start_connection_manager() ->
    CompSpec = rvi_common:get_component_specification(),
    {ok, BertOpts } = rvi_common:get_module_config(data_link, 
						   ?MODULE, 
						   bert_rpc_server, 
						   [], 
						   CompSpec),
    IP = proplists:get_value(ip, BertOpts, ?DEFAULT_BERT_RPC_ADDRESS),
    Port = proplists:get_value(port, BertOpts, ?DEFAULT_BERT_RPC_PORT),
    
    ?info("dlink_tcp:init_rvi_component(~p): Starting listener.", [self()]),

    %% Fire up listener
    connection_manager:start_link(), 
    {ok,Pid} = listener:start_link(), 
    ?info("dlink_tcp:init_rvi_component(): Adding listener ~p:~p", [ IP, Port ]),
    
    %% Add listener port.
    case listener:add_listener(Pid, IP, Port, CompSpec) of
	ok ->
	    ?notice("---- RVI Node External Address: ~s", 
		    [ application:get_env(rvi, node_address, undefined)]);

	Err -> 	
	    ?error("dlink_tcp:init_rvi_component(): Failed to launch listener: ~p", [ Err ]),
	    ok
    end,
    ?info("dlink_tcp:init_rvi_component(): Setting up persistent connections."),
    
    {ok, PersistentConnections } = rvi_common:get_module_config(data_link, 
								?MODULE, 
								?PERSISTENT_CONNECTIONS, 
								[], 
								CompSpec),


    setup_persistent_connections_(PersistentConnections, CompSpec),
    ok.

setup_persistent_connections_([ ], _CompSpec) ->
     ok;


setup_persistent_connections_([ NetworkAddress | T], CompSpec) ->
    ?debug("~p: Will persistently connect connect : ~p", [self(), NetworkAddress]),
    [ IP, Port] =  string:tokens(NetworkAddress, ":"),
    connect_and_retry_remote(IP, Port, CompSpec), 
    setup_persistent_connections_(T, CompSpec),
    ok.

service_available(CompSpec, SvcName, DataLinkModule, Address) ->
    rvi_common:notification(data_link, ?MODULE, 
			    service_available, 
			    [{ service, SvcName },
			     { data_link_module, DataLinkModule },
			     { address, Address }],
			    CompSpec).

service_unavailable(CompSpec, SvcName, DataLinkModule, Address) ->
    rvi_common:notification(data_link, ?MODULE, 
			    service_unavailable, 
			    [{ service, SvcName },
			     { data_link_module, DataLinkModule },
			     { address, Address }],
			    CompSpec).


setup_data_link(CompSpec, Service, Opts) ->
    rvi_common:request(data_link, ?MODULE, setup_data_link,
		       [ { service, Service },
			 { opts, Opts }],
		       [status, timeout], CompSpec).

disconnect_data_link(CompSpec, NetworkAddress) ->
    rvi_common:request(data_link, ?MODULE, disconnect_data_link,
		       [ {network_address, NetworkAddress} ],
		       [status], CompSpec).


send_data(CompSpec, Service, Data) ->
    rvi_common:request(data_link, ?MODULE, send_data,
			    [ { service, Service }, 
			      { data, Data } ], 
		       [status], CompSpec).


%% End of behavior

%%
%% Connect to a remote RVI node.
%%
connect_remote(IP, Port, CompSpec) ->
    case connection_manager:find_connection_by_address(IP, Port) of
	{ ok, _Pid } ->
	    already_connected;

	not_found ->
	    %% Setup a new outbound connection
	    ?info("dlink_tcp:connect_remote(): Connecting ~p:~p",
		  [IP, Port]),

	    case gen_tcp:connect(IP, Port, [binary, {packet, 4}]) of
		{ ok, Sock } -> 
		    ?info("dlink_tcp:connect_remote(): Connected ~p:~p", 
			   [IP, Port]),

		    %% Setup a genserver around the new connection.
		    {ok, Pid } = connection:setup(IP, Port, Sock, 
						  ?MODULE, handle_socket, [CompSpec] ),

		    %% Send authorize
		    { LocalIP, LocalPort} = rvi_common:node_address_tuple(),
		    connection:send(Pid, 
				    { authorize, 
				      1, LocalIP, LocalPort, rvi_binary, 
				      { certificate, {}}, { signature, {}} }),
		    ok;
		
		{error, Err } -> 
		    ?info("dlink_tcp:connect_remote(): Failed ~p:~p: ~p",
			   [IP, Port, Err]),
		    not_available
	    end
    end.
		    

connect_and_retry_remote( IP, Port, CompSpec) ->
    ?info("dlink_tcp:connect_and_retry_remote(): ~p:~p", 
	  [ IP, Port]),

    case connect_remote(IP, list_to_integer(Port), CompSpec) of
	ok  -> ok;

	Err -> %% Failed to connect. Sleep and try again
	    ?notice("dlink_tcp:connect_and_retry_remote(~p:~p): Failed: ~p", 
			   [IP, Port, Err]),

	    ?notice("dlink_tcp:connect_and_retry_remote(~p:~p): Will try again in ~p sec", 
			   [IP, Port, ?DEFAULT_RECONNECT_INTERVAL]),

	    setup_reconnect_timer(?DEFAULT_RECONNECT_INTERVAL, IP, Port, CompSpec),

	    not_available
    end.


announce_local_service_(CompSpec, Service, Availability) ->
    ?debug("dlink_tcp:announce_local_service(~p): Service: ~p",  [Availability, Service]),
    %% Grab our local address.
    { LocalAddress, LocalPort } = rvi_common:node_address_tuple(),

    %% Grab all remote addresses we are currently connected to.
    %% We will get the data link address of all remote nodes that
    %% we currently have a conneciton to.
    [ ok, Addresses ] = service_discovery_rpc:get_remote_network_addresses(CompSpec),

    %% Grab our local address.
    { LocalAddress, LocalPort } = rvi_common:node_address_tuple(),

    %% Loop over all returned addresses
    lists:map(
      fun(Address) ->
	      ?info("dlink_tcp:announce_local_service(~p): Announcing ~p to ~p", 
		    [ Availability, Service, Address]),

	      %% Split the address into host and port
	      [ RemoteAddress, RemotePort] =  string:tokens(Address, ":"),

	      %% Announce the new service to the remote 
	      %% RVI node
	      Res = connection:send(RemoteAddress, list_to_integer(RemotePort), 
				    {service_announce, 3, Availability, 
				     [Service], { signature, {}}}),
	      ?debug("dlink_tcp:announce_local_service(~p): Res      ~p", 
		     [ Availability, Res])
      end, Addresses),
    ok.

handle_socket(_FromPid, PeerIP, PeerPort, data, ping, [_CompSpec]) ->
    ?info("dlink_tcp:ping(): Pinged from: ~p:~p", [ PeerIP, PeerPort]),
    ok;

handle_socket(FromPid, PeerIP, PeerPort, data, 
	      { authorize, 
		TransactionID, 
		RemoteAddress, 
		RemotePort, 
		Protocol, 
		Certificate,
		Signature}, [CompSpec]) ->

    ?info("dlink_tcp:authorize(): Peer Address:   ~p:~p", [PeerIP, PeerPort ]),
    ?info("dlink_tcp:authorize(): Remote Address: ~p~p", [ RemoteAddress, RemotePort ]),
    ?info("dlink_tcp:authorize(): Protocol:       ~p", [ Protocol ]),
    ?debug("dlink_tcp:authorize(): TransactionID:  ~p", [ TransactionID ]),
    ?debug("dlink_tcp:authorize(): Certificate:    ~p", [ Certificate ]),
    ?debug("dlink_tcp:authorize(): Signature:      ~p", [ Signature ]),


    { LocalAddress, LocalPort } = rvi_common:node_address_tuple(),

    %% If the remote address and port are both reported as "0.0.0.0" and 0,
    %% then the client connects from behind a firewall and cannot
    %% accept return connections. In these cases, we will tie the
    %% gonnection to the peer address provided in PeerIP and PeerPort
    { NRemoteAddress, NRemotePort} =
	case { RemoteAddress, RemotePort } of
	    { "0.0.0.0", 0 } ->
		
		?info("dlink_tcp:authorize(): Remote is behind firewall. Will use ~p:~p", 
		      [ PeerIP, PeerPort]),
		{ PeerIP, PeerPort };

	    _ -> { RemoteAddress, RemotePort}
	end,

    %% If FromPid (the genserver managing the socket) is not yet registered
    %% with the conneciton manager, this is an incoming connection
    %% from the client. We should respond with our own authorize followed by
    %% a service announce
    
    %% FIXME: Validate certificate and signature before continuing.
    case connection_manager:find_connection_by_pid(FromPid) of
	not_found ->
	    ?info("dlink_tcp:authorize(): New connection!"),
	    connection_manager:add_connection(NRemoteAddress, NRemotePort, FromPid),
	    ?debug("dlink_tcp:authorize(): Sending authorize."),
	    Res = connection:send(FromPid, 
			    { authorize, 
			      1, LocalAddress, LocalPort, rvi_binary, 
			      {certificate, {}}, { signature, {}}}),
	    ?debug("dlink_tcp:authorize(): Sending authorize: ~p", [ Res]),
	    ok;
	_ -> ok
    end,

    %% Send our own servide announcement to the remote server
    %% that just authorized to us.
    [ ok, LocalServices ] = service_discovery_rpc:get_services_by_module(CompSpec, local),
	 

    %% Send an authorize back to the remote node
    ?info("dlink_tcp:authorize(): Announcing local services: ~p to remote ~p:~p",
	  [LocalServices, NRemoteAddress, NRemotePort]),

    connection:send(FromPid, 
		    { service_announce, 2, available,
		      LocalServices, { signature, {}}}),

    %% Setup ping interval
    gen_server:call(?SERVER, { setup_initial_ping, NRemoteAddress, NRemotePort, FromPid }),
    ok;

handle_socket(FromPid, RemoteIP, RemotePort, data, 
	      { service_announce, 
		TransactionID,
		available,
		Services,
		Signature }, [CompSpec]) ->
    ?debug("dlink_tcp:service_announce(available): Address:       ~p:~p", [ RemoteIP, RemotePort ]),
    ?debug("dlink_tcp:service_announce(available): Remote Port:   ~p", [ RemotePort ]),
    ?debug("dlink_tcp:service_announce(available): TransactionID: ~p", [ TransactionID ]),
    ?debug("dlink_tcp:service_announce(available): Signature:     ~p", [ Signature ]),
    ?debug("dlink_tcp:service_announce(available): Service:       ~p", [ Services ]),

    
    %% Insert into our own tables
    [ ets:insert(?SERVICE_TABLE, 
		 #service_entry { 
		    service = SvcName,
		    connection = FromPid })  || SvcName <- Services ],

    [ ets:insert(?CONNECTION_TABLE, 
		 #service_entry { 
		    service = SvcName,
		    connection = FromPid }) || SvcName <- Services ],
    
    service_discovery_rpc:register_services(CompSpec, Services, ?MODULE),
    ok;


handle_socket(FromPid, RemoteIP, RemotePort, data, 
	      { service_announce, 
		TransactionID, 
		unavailable,
		Services, 
		Signature}, [CompSpec]) ->
    ?debug("dlink_tcp:service_announce(unavailable): Address:       ~p:~p", [ RemoteIP, RemotePort ]),
    ?debug("dlink_tcp:service_announce(unavailable): Remote Port:   ~p", [ RemotePort ]),
    ?debug("dlink_tcp:service_announce(unavailable): TransactionID: ~p", [ TransactionID ]),
    ?debug("dlink_tcp:service_announce(unavailable): Signature:     ~p", [ Signature ]),
    ?debug("dlink_tcp:service_announce(unavailable): Service:       ~p", [ Services ]),

    %% Register the received services with all relevant components

    
    %% Delete from our own tables.
    
    [ ets:delete(?SERVICE_TABLE, SvcName ) || SvcName <- Services ],
    
    [ ets:match_delete(?CONNECTION_TABLE, 
		       #service_entry { 
			  service = SvcName,
			  connection = FromPid }) || SvcName <- Services ],

    service_discovery_rpc:unregister_services(CompSpec, Services, ?MODULE),
    ok;


handle_socket(_FromPid, SetupIP, SetupPort, data, 
	      { receive_data, Data}, [CompSpec]) ->
%%    ?info("dlink_tcp:receive_data(): ~p", [ Data ]),
    ?debug("dlink_tcp:receive_data(): SetupAddress:  {~p, ~p}", [ SetupIP, SetupPort ]),
    protocol_rpc:receive_message(CompSpec, Data),
    ok;


handle_socket(_FromPid, SetupIP, SetupPort, data, Data, [_CompSpec]) ->
    ?warning("dlink_tcp:unknown_data(): SetupAddress:  {~p, ~p}", [ SetupIP, SetupPort ]),
    ?warning("dlink_tcp:unknown_data(): Unknown data:  ~p",  [ Data]),
    ok.


%% We lost the socket connection.
%% Unregister all services that were routed to the remote end that just died.
handle_socket(FromPid, SetupIP, SetupPort, closed, [CompSpec]) ->
    ?info("dlink_tcp:socket_closed(): SetupAddress:  {~p, ~p}", [ SetupIP, SetupPort ]),

    NetworkAddress = SetupIP  ++ ":" ++ integer_to_list(SetupPort),

    %% Get all service records associated with the given connection and
    %% extract the service name from them
    %


    Services = [ SvcName || #service_entry { service = SvcName } <- 
				ets:lookup(?CONNECTION_TABLE, FromPid) ],
    ?debug("dlink_tcp:close(): Deleting ~p", [ Services]),

    %% Step through all services and delete their corresponding record
    %% from the service table.
    %% We do this instead of match_delete because it is much faster since
    %% service is the key in ?SERVICE_TABLE. No linear search and delete
    %% needed.
    [ ets:delete(?SERVICE_TABLE, SvcName) || SvcName <- Services ],

    %% Delete all entries in the connection table that matches the closed
    %% connection.
    ets:delete(?CONNECTION_TABLE, FromPid),


    {ok, PersistentConnections } = rvi_common:get_module_config(data_link, 
								?MODULE, 
								persistent_connections, 
								[], 
								CompSpec),
    %% Check if this is a static node. If so, setup a timer for a reconnect
    case lists:member(NetworkAddress, PersistentConnections) of
	true ->
	    ?info("dlink_tcp:socket_closed(): Reconnect address:  ~p", [ NetworkAddress ]),
	    ?info("dlink_tcp:socket_closed(): Reconnect interval: ~p", [ ?DEFAULT_RECONNECT_INTERVAL ]),
	    [ IP, Port] = string:tokens(NetworkAddress, ":"),

	    setup_reconnect_timer(?DEFAULT_RECONNECT_INTERVAL, 
				  IP, Port, CompSpec);
	false -> ok
    end,
    ok;

handle_socket(_FromPid, SetupIP, SetupPort, error, _ExtraArgs) ->
    ?info("dlink_tcp:socket_error(): SetupAddress:  {~p, ~p}", [ SetupIP, SetupPort ]),
    ok.


%% JSON-RPC entry point
%% CAlled by local exo http server
handle_notification("service_available", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),
    {ok, Address} = rvi_common:get_json_element(["address"], Args),

    gen_server:cast(?SERVER, { rvi, service_available, 
				      [ SvcName,
					DataLinkModule,
					Address ]}),

    ok;
handle_notification("service_unavailable", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),
    {ok, Address} = rvi_common:get_json_element(["address"], Args),

    gen_server:cast(?SERVER, { rvi, service_unavailable, 
				      [ SvcName,
					DataLinkModule,
					Address ]}),

    ok;

handle_notification(Other, _Args) ->
    ?info("dlink_tcp:handle_notification(~p): unknown", [ Other ]),
    ok.

handle_rpc("setup_data_link", Args) ->
    { ok, Service } = rvi_common:get_json_element(["service"], Args),

    { ok, Opts } = rvi_common:get_json_element(["opts"], Args),

    [ Res, Timeout ] = gen_server:call(?SERVER, { rvi, setup_data_link, 
						  [ Service, Opts ] }),

    {ok, [ {status, rvi_common:json_rpc_status(Res)} , { timeout, Timeout }]};

handle_rpc("disconenct_data_link", Args) ->
    { ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    [Res] = gen_server:call(?SERVER, { rvi, disconnect_data_link, [NetworkAddress]}),
    {ok, [ {status, rvi_common:json_rpc_status(Res)} ]};

handle_rpc("send_data", Args) ->
    { ok, Service } = rvi_common:get_json_element(["service"], Args),
    { ok,  Data } = rvi_common:get_json_element(["data"], Args),
    [ Res ]  = gen_server:call(?SERVER, { rvi, send_data, [Service, Data]}),
    {ok, [ {status, rvi_common:json_rpc_status(Res)} ]};
    

handle_rpc(Other, _Args) ->
    ?info("dlink_tcp:handle_rpc(~p): unknown", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


handle_cast( {rvi, service_available, [SvcName, local, _Address]}, St) ->
    announce_local_service_(St#st.cs, SvcName, available),
    {noreply, St};


handle_cast( {rvi, service_unavailable, [SvcName, local, _Address]}, St) ->
    announce_local_service_(St#st.cs, SvcName, unavailable),
    {noreply, St};


handle_cast(Other, St) ->
    ?warning("dlink_tcp:handle_cast(~p): unknown", [ Other ]),
    {noreply, St}.


handle_call({rvi, setup_data_link, [ Service, Opts ]}, _From, St) ->
    case proplists:get_value(target, Opts, undefined) of
	undefined ->
	    ?info("dlink_tcp:setup_data_link(~p) Failed: no target given in options.",
		  [Service]),
	     { reply, [ok, -1 ], St };

	Addr -> 
	    [ Address, Port] =  string:tokens(Addr, ":"),

	    case connect_remote(Address, list_to_integer(Port), St#st.cs) of
		ok  ->
		    { reply, [ok, 2000], St };  %% 2 second timeout

		Err ->
		    { reply, [Err, 0], St }
	    end
    end;


handle_call({rvi, disconnect_data_link, [NetworkAddress] }, _From, St) ->
    [ Address, Port] = string:tokens(NetworkAddress, ":"),
    Res = connection:terminate_connection(Address,Port),
    { reply, [ Res ], St };


handle_call({rvi, send_data, [Service, Data]}, _From, St) ->

    %% Resolve connection pid from service
    case ets:lookup(?SERVICE_TABLE, Service) of
	[ #service_entry { connection = ConnPid } ] ->
	    ?debug("dlink_tcp:send_data(): ~p -> ~p", [ Service, ConnPid]),
	    Res = connection:send(ConnPid, {receive_data, Data}),
	    { reply, [ Res ], St};

	[] -> %% Service disappeared during send.
	    { reply, [ no_route ], St}
    end;
	    



handle_call({setup_initial_ping, Address, Port, Pid}, _From, St) ->
    %% Create a timer to handle periodic pings.
    {ok, ServerOpts } = rvi_common:get_module_config(data_link, 
						     ?MODULE,
						     bert_rpc_server, [], 
						     St#st.cs),
    Timeout = proplists:get_value(ping_interval, ServerOpts, ?DEFAULT_PING_INTERVAL),

    ?info("dlink_tcp:setup_ping(): ~p:~p will be pinged every ~p msec", 
	  [ Address, Port, Timeout] ),
										      
    erlang:send_after(Timeout, self(), { rvi_ping, Pid, Address, Port, Timeout }),

    {reply, ok, St};

handle_call(Other, _From, St) ->
    ?warning("dlink_tcp:handle_rpc(~p): unknown", [ Other ]),
    { reply, { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ]}, St}.



%% Ping time
handle_info({ rvi_ping, Pid, Address, Port, Timeout},  St) ->

    %% Check that connection is up
    case connection:is_connection_up(Pid) of
	true ->
	    ?info("dlink_tcp:ping(): Pinging: ~p:~p", [Address, Port]),
	    connection:send(Pid, ping),
	    erlang:send_after(Timeout, self(), 
			      { rvi_ping, Pid, Address, Port, Timeout });

	false ->
	    ok
    end,
    {noreply, St};

%% Setup static nodes
handle_info({ rvi_setup_persitent_connection, IP, Port, CompSpec }, St) ->
    connect_and_retry_remote(IP, Port, CompSpec),
    { noreply, St };


handle_info(Info, St) ->
    ?notice("dlink_tcp(): Unkown message: ~p", [ Info]),
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

setup_reconnect_timer(MSec, IP, Port, CompSpec) ->
    erlang:send_after(MSec, ?MODULE, 
		      { rvi_setup_persitent_connection, 
			IP, Port, CompSpec }),
    ok.


    
