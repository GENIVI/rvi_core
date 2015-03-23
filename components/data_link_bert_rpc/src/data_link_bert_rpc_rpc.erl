%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(data_link_bert_rpc_rpc).
-behavior(gen_server).

-export([handle_rpc/2]).
-export([handle_socket/6]).
-export([handle_socket/5]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_json_server/0]).
-export([start_connection_manager/0]).

-export([announce_available_local_service/2,
	 announce_unavailable_local_service/2,
	 setup_data_link/2,
	 disconnect_data_link/2,
	 send_data/3]).


-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").

-define(DEFAULT_BERT_RPC_PORT, 9999).
-define(DEFAULT_RECONNECT_INTERVAL, 5000).
-define(DEFAULT_BERT_RPC_ADDRESS, "0.0.0.0").
-define(DEFAULT_PING_INTERVAL, 300000).  %% Five minutes
-define(SERVER, ?MODULE). 
-record(st, { 
	  cs = #component_spec{}
	 }).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?info("data_link_bert:init(): Called"),
    %% Dig out the bert rpc server setup

    {ok, #st { 
	    cs = rvi_common:get_component_specification()
	   }
    }.

start_json_server() ->
    rvi_common:start_json_rpc_server(data_link, ?MODULE, data_link_bert_rpc_sup).


start_connection_manager() ->
    CompSpec = rvi_common:get_component_specification(),
    {ok, BertOpts } = rvi_common:get_module_config(data_link, 
						   ?MODULE, 
						   bert_rpc_server, 
						   [], 
						   CompSpec),
    IP = proplists:get_value(ip, BertOpts, ?DEFAULT_BERT_RPC_ADDRESS),
    Port = proplists:get_value(port, BertOpts, ?DEFAULT_BERT_RPC_PORT),
    
    ?info("data_link_bert:init_rvi_component(~p): Starting listener.", [self()]),

    %% Fire up listener
    connection_manager:start_link(), 
    {ok,Pid} = listener:start_link(), 
    ?info("data_link_bert:init_rvi_component(): Adding listener ~p:~p", [ IP, Port ]),
    
    %% Add listener port.
    case listener:add_listener(Pid, IP, Port, CompSpec) of
	ok ->
	    ?notice("---- RVI Node External Address: ~s", 
		    [ application:get_env(rvi, node_address, undefined)]);

	Err -> 	
	    ?error("data_link_bert:init_rvi_component(): Failed to launch listener: ~p", [ Err ]),
	    ok
    end,
    ?info("data_link_bert:init_rvi_component(): Setting up static nodes."),
    setup_static_node_data_links_(rvi_common:static_nodes(), CompSpec),
    ok.

setup_static_node_data_links_([ ], _CompSpec) ->
    ok;


setup_static_node_data_links_([ { Prefix, NetworkAddress} | T], CompSpec) ->
    ?debug("~p: Will connect static node ~p -> ~p", [self(), Prefix, NetworkAddress]),
    [ IP, Port] =  string:tokens(NetworkAddress, ":"),
    connect_and_retry_remote(Prefix, IP, Port, CompSpec), 
    setup_static_node_data_links_(T, CompSpec),
    ok.

%% Behavior implementation
announce_available_local_service(CompSpec, Service) ->
    rvi_common:request(data_link, ?MODULE, announce_available_local_service,
		       [ Service ], [service],[status], CompSpec).


announce_unavailable_local_service(CompSpec, Service) ->
    rvi_common:request(data_link, ?MODULE, announce_available_local_service,
		       [ Service ], [service],[status], CompSpec).


setup_data_link(CompSpec, NetworkAddress) ->
    rvi_common:request(data_link, ?MODULE, setup_data_link,
		       [ NetworkAddress ], [ network_address ], 
		       [status], CompSpec).
disconnect_data_link(CompSpec, NetworkAddress) ->
    rvi_common:request(data_link, ?MODULE, disconnect_data_link,
		       [ NetworkAddress ], [ network_address ], 
		       [status], CompSpec).


send_data(CompSpec, NetworkAddress, Data) ->
    rvi_common:request(data_link, ?MODULE, send_data,
		       [ NetworkAddress, Data ], [ network_address, data ], 
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
	    ?info("data_link_bert:connect_remote(): Connecting ~p:~p",
		  [IP, Port]),

	    case gen_tcp:connect(IP, Port, [binary, {packet, 4}]) of
		{ ok, Sock } -> 
		    ?info("data_link_bert:connect_remote(): Connected ~p:~p", 
			   [IP, Port]),

		    %% Setup a genserver around the new connection.
		    {ok, Pid } = connection:setup(IP, Port, Sock, 
						  ?MODULE, handle_socket, [CompSpec] ),

		    %% Send authorize
		    connection:send(Pid, 
				    { authorize, 
				      1, IP, Port, rvi_binary, 
				      { certificate, {}}, { signature, {}} }),
		    ok;
		
		Err -> 
		    ?info("data_link_bert:connect_remote(): Failed ~p:~p: ~p",
			   [IP, Port, Err]),
		    Err
	    end
    end.
		    

connect_and_retry_remote(Prefix, IP, Port, CompSpec) ->
    ?info("data_link_bert:setup_static(): Connecting ~p -> ~p:~p", 
	  [Prefix, IP, Port]),

    case connect_remote(IP, list_to_integer(Port), CompSpec) of
	ok  -> ok;

	Err -> %% Failed to connect. Sleep and try again
	    ?notice("data_link_bert:setup_static_node_data_link(~p:~p): Failed: ~p", 
			   [IP, Port, Err]),

	    ?notice("data_link_bert:setup_static_node_data_link(~p:~p): Will try again in ~p sec", 
			   [IP, Port, ?DEFAULT_RECONNECT_INTERVAL]),

	    setup_static_node_reconnect_timer(?DEFAULT_RECONNECT_INTERVAL,
					      Prefix, IP, Port, CompSpec),

	    not_available
    end.


announce_local_service_(CompSpec, Service, Availability) ->
    ?debug("data_link_bert:announce_local_service(~p): Service: ~p",  [Availability, Service]),
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
	      ?info("data_link_bert:announce_local_service(~p): Announcing ~p to ~p", 
		    [ Availability, Service, Address]),

	      %% Split the address into host and port
	      [ RemoteAddress, RemotePort] =  string:tokens(Address, ":"),

	      %% Announce the new service to the remote 
	      %% RVI node
	      Res = connection:send(RemoteAddress, list_to_integer(RemotePort), 
				    {service_announce, 3, Availability, 
				     [Service], { signature, {}}}),
	      ?debug("data_link_bert:announce_local_service(~p): Res      ~p", 
		     [ Availability, Res])
      end, Addresses),
    ok.

handle_socket(_FromPid, PeerIP, PeerPort, data, ping, [_CompSpec]) ->
    ?info("data_link_bert:ping(): Pinged from: ~p:~p", [ PeerIP, PeerPort]),
    ok;

handle_socket(FromPid, PeerIP, PeerPort, data, 
	      { authorize, 
		TransactionID, 
		RemoteAddress, 
		RemotePort, 
		Protocol, 
		Certificate,
		Signature}, [CompSpec]) ->

    ?info("data_link_bert:authorize(): Peer Address:   ~p:~p", [PeerIP, PeerPort ]),
    ?info("data_link_bert:authorize(): Remote Address: ~p~p", [ RemoteAddress, RemotePort ]),
    ?info("data_link_bert:authorize(): Protocol:       ~p", [ Protocol ]),
    ?debug("data_link_bert:authorize(): TransactionID:  ~p", [ TransactionID ]),
    ?debug("data_link_bert:authorize(): Certificate:    ~p", [ Certificate ]),
    ?debug("data_link_bert:authorize(): Signature:      ~p", [ Signature ]),


    { LocalAddress, LocalPort } = rvi_common:node_address_tuple(),

    %% If the remote address and port are both reported as "0.0.0.0" and 0,
    %% then the client connects from behind a firewall and cannot
    %% accept return connections. In these cases, we will tie the
    %% gonnection to the peer address provided in PeerIP and PeerPort
    { NRemoteAddress, NRemotePort} =
	case { RemoteAddress, RemotePort } of
	    { "0.0.0.0", 0 } ->
		
		?info("data_link_bert:authorize(): Remote is behind firewall. Will use ~p:~p", 
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
	    ?info("data_link_bert:authorize(): New connection!"),
	    connection_manager:add_connection(NRemoteAddress, NRemotePort, FromPid),
	    ?debug("data_link_bert:authorize(): Sending authorize."),
	    Res = connection:send(FromPid, 
			    { authorize, 
			      1, LocalAddress, LocalPort, rvi_binary, 
			      {certificate, {}}, { signature, {}}}),
	    ?debug("data_link_bert:authorize(): Sending authorize: ~p", [ Res]),
	    ok;
	_ -> ok
    end,

    %% Send our own servide announcement to the remote server
    %% that just authorized to us.
    %% First grab all our services.
    [ ok, Services ] = service_discovery_rpc:get_local_services(CompSpec),
	 
    %% Covnert to JSON structured typles.
    LocalServices = [ Service || { Service, _LocalAddress } <- Services ],
	%% lists:foldl(fun({struct, JSONElem}, Acc) -> 
	%% 		    [ proplists:get_value("service", JSONElem, undefined) | Acc];
	%% 	       ({Service, _LocalAddress}, Acc) -> 
	%% 			    [ Service | Acc ];
	%% 		       (Elem, Acc) -> 
	%% 			    [ Elem | Acc ]
	%% 		    end,
	%% 		    [], JSONSvc),

    %% Grab our local address.
    { LocalAddress, LocalPort } = rvi_common:node_address_tuple(),

    %% Send an authorize back to the remote node
    ?info("data_link_bert:authorize(): Announcing local services: ~p to remote ~p:~p",
	  [LocalServices, NRemoteAddress, NRemotePort]),

    connection:send(FromPid, 
		    { service_announce, 2, available,
		      LocalServices, { signature, {}}}),

    %% Setup ping interval
    gen_server:call(?SERVER, { setup_initial_ping, NRemoteAddress, NRemotePort, FromPid }),
    ok;

handle_socket(_FromPid, RemoteIP, RemotePort, data, 
	      { service_announce, 
		TransactionID, 
		available,
		Services, 
		Signature}, [CompSpec]) ->
    ?debug("data_link_bert:service_announce(available): Address:       ~p:~p", [ RemoteIP, RemotePort ]),
    ?debug("data_link_bert:service_announce(available): Remote Port:   ~p", [ RemotePort ]),
    ?debug("data_link_bert:service_announce(available): TransactionID: ~p", [ TransactionID ]),
    ?debug("data_link_bert:service_announce(available): Signature:     ~p", [ Signature ]),
    ?debug("data_link_bert:service_announce(available): Service:       ~p", [ Services ]),


    %% Register the received services with all relevant components
    
    NetworkAddress = RemoteIP  ++ ":" ++ integer_to_list(RemotePort),
    service_discovery_rpc:register_remote_services(CompSpec, Services, NetworkAddress),
    ok;


handle_socket(_FromPid, RemoteIP, RemotePort, data, 
	      { service_announce, 
		TransactionID, 
		unavailable,
		Services, 
		Signature}, [CompSpec]) ->
    ?debug("data_link_bert:service_announce(unavailable): Address:       ~p:~p", [ RemoteIP, RemotePort ]),
    ?debug("data_link_bert:service_announce(unavailable): Remote Port:   ~p", [ RemotePort ]),
    ?debug("data_link_bert:service_announce(unavailable): TransactionID: ~p", [ TransactionID ]),
    ?debug("data_link_bert:service_announce(unavailable): Signature:     ~p", [ Signature ]),
    ?debug("data_link_bert:service_announce(unavailable): Service:       ~p", [ Services ]),

    %% Register the received services with all relevant components

    service_discovery_rpc:unregister_remote_services(CompSpec, Services),
    ok;


handle_socket(_FromPid, SetupIP, SetupPort, data, 
	      { receive_data, Data}, [CompSpec]) ->
%%    ?info("data_link_bert:receive_data(): ~p", [ Data ]),
    ?debug("data_link_bert:receive_data(): SetupAddress:  {~p, ~p}", [ SetupIP, SetupPort ]),
    protocol_rpc:receive_message(CompSpec, Data),
    ok;


handle_socket(_FromPid, SetupIP, SetupPort, data, Data, [_CompSpec]) ->
    ?warning("data_link_bert:unknown_data(): SetupAddress:  {~p, ~p}", [ SetupIP, SetupPort ]),
    ?warning("data_link_bert:unknown_data(): Unknown data:  ~p",  [ Data]),
    ok.

%% We lost the socket connection.
%% Unregister all services that were routed to the remote end that just died.
handle_socket(_FromPid, SetupIP, SetupPort, closed, [CompSpec]) ->
    ?info("data_link_bert:socket_closed(): SetupAddress:  {~p, ~p}", [ SetupIP, SetupPort ]),
    NetworkAddress = SetupIP  ++ ":" ++ integer_to_list(SetupPort),
    service_discovery_rpc:
	unregister_remote_services_by_address(CompSpec, NetworkAddress),

    %% Check if this is a static node. If so, setup a timer for a reconnect
    case lists:keyfind(NetworkAddress, 2, rvi_common:static_nodes()) of
	false ->
	    true;

	{ StaticPrefix, StaticNetworkAddress } ->
	    ?info("data_link_bert:socket_closed(): Reconnect service:  ~p", [ StaticPrefix ]),
	    ?info("data_link_bert:socket_closed(): Reconnect address:  ~p", [ StaticNetworkAddress ]),
	    ?info("data_link_bert:socket_closed(): Reconnect interval: ~p", [ ?DEFAULT_RECONNECT_INTERVAL ]),
	    [ IP, Port] = string:tokens(StaticNetworkAddress, ":"),

	    setup_static_node_reconnect_timer(?DEFAULT_RECONNECT_INTERVAL, 
					      StaticPrefix, 
					      IP, Port, CompSpec)
	    
    end,
    ok;

handle_socket(_FromPid, SetupIP, SetupPort, error, _ExtraArgs) ->
    ?info("data_link_bert:socket_error(): SetupAddress:  {~p, ~p}", [ SetupIP, SetupPort ]),
    ok.


%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc("announce_available_local_service", Args) ->
    { ok,  Service } = rvi_common:get_json_element(["service"], Args),
    [ ok ] = gen_server:call(?SERVER, { rvi_call, announce_available_local_service, [Service]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};


handle_rpc("announce_unavailable_local_service", Args) ->
    { ok,  Service } = rvi_common:get_json_element(["service"], Args),

    [ ok ] = gen_server:call(?SERVER, { rvi_call, announce_unavailable_local_service, [Service]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};

handle_rpc("setup_data_link", Args) ->
    { ok, Address } = rvi_common:get_json_element(["network_address"], Args),
    [ ok ] = gen_server:call(?SERVER, { rvi_call, setup_data_link, 
					 [ Address]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};

handle_rpc("disconenct_data_link", Args) ->
    { ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    [ ok ] = gen_server:call(?SERVER, { rvi_call, disconnect_data_link, [NetworkAddress]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};


handle_rpc("send_data", Args) ->
    {ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    { ok,  Data} = rvi_common:get_json_element(["data"], Args),
    [ Res ]  = gen_server:call(?SERVER, { rvi_call, send_data, [NetworkAddress, Data]}),
    {ok, [ {status, rvi_common:json_rpc_status(Res)} ]};
    
    
handle_rpc(Other, _Args) ->
    ?info("data_link_bert:handle_rpc(~p): unknown", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


handle_call({rvi_call, announce_available_local_service, [Service]}, _From, St) ->
    announce_local_service_(St#st.cs, Service, available),
    {reply, [ok], St};

handle_call({rvi_call, announce_unavailable_local_service, [Service]}, _From, St) ->
    announce_local_service_(St#st.cs, Service, unavailable),
    {reply, [ok], St};

handle_call({rvi_call, setup_data_link, [ NetworkAddress ]}, _From, St) ->
    [ RemoteAddress, RemotePort] =  string:tokens(NetworkAddress, ":"),
    connect_remote(RemoteAddress, list_to_integer(RemotePort), St#st.cs),
    { reply, [ok], St };


handle_call({rvi_call, disconnect_data_link, [NetworkAddress] }, _From, St) ->
    [ Address, Port] = string:tokens(NetworkAddress, ":"),
    Res = connection:terminate_connection(Address,Port),
    { reply, [ Res ], St };


handle_call({rvi_call, send_data, [NetworkAddress, Data]}, _From, St) ->
    [ RemoteAddress, RemotePort] =  string:tokens(NetworkAddress, ":"),
    ?info("data_link_bert:send_data(): Remote: ~p:~p", [ RemoteAddress, RemotePort]),
    Res = connection:send(RemoteAddress, RemotePort, {receive_data, Data}),
    { reply, [ Res ], St};


handle_call({setup_initial_ping, Address, Port, Pid}, _From, St) ->
    %% Create a timer to handle periodic pings.
    {ok, ServerOpts } = rvi_common:get_module_config(data_link, 
						     data_link_bert_rpc,
						     bert_rpc_server, [], 
						     St#st.cs),
    Timeout = proplists:get_value(ping_interval, ServerOpts, ?DEFAULT_PING_INTERVAL),

    ?info("data_link_bert:setup_ping(): ~p:~p will be pinged every ~p msec", 
	  [ Address, Port, Timeout] ),
										      
    erlang:send_after(Timeout, self(), { rvi_ping, Pid, Address, Port, Timeout }),

    {reply, ok, St};

handle_call(Other, _From, St) ->
    ?warning("data_link_bert:handle_rpc(~p): unknown", [ Other ]),
    { reply, { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ]}, St}.


handle_cast(_Msg, St) ->
    {noreply, St}.

%% Ping time
handle_info({ rvi_ping, Pid, Address, Port, Timeout},  St) ->

    %% Check that connection is up
    case connection:is_connection_up(Pid) of
	true ->
	    ?info("data_link_bert:ping(): Pinging: ~p:~p", [Address, Port]),
	    connection:send(Pid, ping),
	    erlang:send_after(Timeout, self(), 
			      { rvi_ping, Pid, Address, Port, Timeout });

	false ->
	    ok
    end,
    {noreply, St};

%% Setup static nodes
handle_info({ rvi_setup_static_node_data_link, Prefix, IP, Port, CompSpec }, St) ->
    connect_and_retry_remote(Prefix, IP, Port, CompSpec),
    { noreply, St };


handle_info(Info, St) ->
    ?notice("data_link_bert(): Unkown message: ~p", [ Info]),
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

setup_static_node_reconnect_timer(MSec, Prefix, IP, Port, CompSpec) ->
    erlang:send_after(MSec, ?MODULE, 
		      { rvi_setup_static_node_data_link, 
			Prefix, IP, Port, CompSpec }),
    ok.

