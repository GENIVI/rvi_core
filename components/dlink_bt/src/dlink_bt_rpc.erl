%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(dlink_bt_rpc).
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
-export([service_available/3,
	 service_unavailable/3]).

-export([setup_data_link/3,
	 disconnect_data_link/2,
	 send_data/5]).


-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").
-include_lib("rvi_common/include/rvi_dlink.hrl").

-define(PERSISTENT_CONNECTIONS, persistent_connections).
-define(DEFAULT_BT_CHANNEL, 1).
-define(DEFAULT_RECONNECT_INTERVAL, 1000).
-define(DEFAULT_PING_INTERVAL, 300000).  %% Five minutes
-define(SERVER, ?MODULE). 

-define(CONNECTION_TABLE, rvi_dlink_bt_connections).
-define(SERVICE_TABLE, rvi_dlink_bt_services).
-define(DLINK_BT_VER, "1.0").

%% Multiple registrations of the same service, each with a different connection,
%% is possible.
-record(service_entry, {
	  service = [],           %% Name of service
	  connections = undefined  %% PID of connection that can reach this service
	 }).

-record(connection_entry, {
	  connection = undefined, %% PID of connection that has a set of services.
	  services = []     %% List of service names available through this connection
	 }).

-record(st, { 
	  cs = #component_spec{}
	 }).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

tohex(V) when V < 16 ->
    "0" ++ integer_to_list(V, 16);

tohex(V) ->
    integer_to_list(V, 16).
	  
bt_address_to_string({A1, A2, A3, A4, A5, A6}) ->
    tohex(A1) ++ ":" ++
    tohex(A2) ++ ":" ++
    tohex(A3) ++ ":" ++
    tohex(A4) ++ ":" ++ 
    tohex(A5) ++ ":" ++
    tohex(A6).
	

init([]) ->
    ?info("dlink_bt:init(): Called"),
    %% Dig out the bert rpc server setup

    ets:new(?SERVICE_TABLE, [ set, public, named_table, 
			      { keypos, #service_entry.service }]),

    ets:new(?CONNECTION_TABLE, [ set, public, named_table, 
				 { keypos, #connection_entry.connection }]),

    CS = rvi_common:get_component_specification(),
    service_discovery_rpc:subscribe(CS, ?MODULE),

    {ok, #st { 
	    cs = CS
	   }
    }.

start_json_server() ->
    rvi_common:start_json_rpc_server(data_link, ?MODULE, dlink_bt_sup).


start_connection_manager() ->
    CompSpec = rvi_common:get_component_specification(),
    {ok, BertOpts } = rvi_common:get_module_config(data_link, 
						   ?MODULE, 
						   server_opts, 
						   [], 
						   CompSpec),
    %% Retrieve the channel we should use
    Channel = proplists:get_value(channel, BertOpts, ?DEFAULT_BT_CHANNEL),
    
    ?info("dlink_bt:init_rvi_component(~p): Starting listener.", [self()]),

    %% Fire up listener
 
    bt:start(),
    bt:debug(debug),
    bt_listener:start_link(), 
    bt_connection_manager:start_link(), 
    ?info("dlink_bt:start_connection_manager(): Adding listener on bluetooth channel ~p", [Channel ]),
    
    %% Add listener channel.
    case bt_listener:add_listener(Channel) of
	ok ->
	    ok;

	Err -> 	
	    ?error("dlink_bt:init_rvi_component(): Failed to launch listener: ~p", [ Err ]),
	    ok
    end,

    {ok, PersistentConnections } = rvi_common:get_module_config(data_link, 
								?MODULE, 
								?PERSISTENT_CONNECTIONS, 
								[], 
								CompSpec),

    setup_persistent_connections_(PersistentConnections, CompSpec),
    ok.


setup_persistent_connections_([ ], _CompSpec) ->
     ok;


setup_persistent_connections_([ BTAddress | T], CompSpec) ->
    ?debug("~p: Will persistently connect connect : ~p", [self(), BTAddress]),
    [ BTAddr, Channel] =  string:tokens(BTAddress, "-"),
    connect_and_retry_remote(BTAddr, Channel, CompSpec), 
    setup_persistent_connections_(T, CompSpec),
    ok.


service_available(CompSpec, SvcName, DataLinkModule) ->
    rvi_common:notification(data_link, ?MODULE, 
			    service_available, 
			    [{ service, SvcName },
			     { data_link_module, DataLinkModule }],
			    CompSpec).

service_unavailable(CompSpec, SvcName, DataLinkModule) ->
    rvi_common:notification(data_link, ?MODULE, 
			    service_unavailable, 
			    [{ service, SvcName },
			     { data_link_module, DataLinkModule }],
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


send_data(CompSpec, ProtoMod, Service, DataLinkOpts, Data) ->
    rvi_common:request(data_link, ?MODULE, send_data,
		       [ { proto_mod, ProtoMod }, 
			 { service, Service }, 
			 { data, Data },
			 { opts, DataLinkOpts }
		       ], 
		       [status], CompSpec).


%% End of behavior

%%
%% Connect to a remote RVI node.
%%
connect_remote(BTAddr, Channel, CompSpec) ->
    case bt_connection_manager:find_connection_by_address(BTAddr, Channel) of
	{ ok, _Pid } ->
	    already_connected;

	not_found ->
	    %% Setup a new outbound connection
	    ?info("dlink_bt:connect_remote(): Connecting ~p:~p",
		  [BTAddr, Channel]),

	    %%FIXME
	    %% Setup a genserver around the new connection.
	    case bt_connection:connect(BTAddr, Channel, 
				       ?MODULE, handle_socket, CompSpec ) of
		{ ok, Pid } -> 
		    ?info("dlink_bt:connect_remote(): Connection in progress ~p:~p - Proc ~p", 
			   [BTAddr, Channel, Pid]),
		    ok;
		
		{error, Err } -> 
		    ?info("dlink_bt:connect_remote(): Failed ~p:~p: ~p",
			   [BTAddr, Channel, Err]),
		    not_available
	    end
    end.
		    

connect_and_retry_remote( BTAddr, Channel, CompSpec) ->
    ?info("dlink_bt:connect_and_retry_remote(): ~p:~p", 
	  [ BTAddr, Channel]),

    case connect_remote(BTAddr, list_to_integer(Channel), CompSpec) of
	ok  -> ok;

	Err -> %% Failed to connect. Sleep and try again
	    ?notice("dlink_bt:connect_and_retry_remote(~p:~p): Failed: ~p", 
			   [BTAddr, Channel, Err]),

	    ?notice("dlink_bt:connect_and_retry_remote(~p:~p): Will try again in ~p sec", 
			   [BTAddr, Channel, ?DEFAULT_RECONNECT_INTERVAL]),

	    setup_reconnect_timer(?DEFAULT_RECONNECT_INTERVAL, BTAddr, Channel, CompSpec),

	    not_available
    end.



announce_local_service_(_CompSpec, [], _Service, _Availability) ->
    ok;

announce_local_service_(CompSpec, 
			[ConnPid | T],
			Service, Availability) ->
    [ ok, JWT ] = authorize_rpc:sign_message(
                    CompSpec, availability_msg(Availability, [Service])),
    Res = bt_connection:send(ConnPid, 
			     term_to_json(
			       { struct, 
				 [
				  { ?DLINK_ARG_CMD, ?DLINK_CMD_SERVICE_ANNOUNCE },
				  { ?DLINK_ARG_TRANSACTION_ID, 3},
				  { ?DLINK_ARG_SIGNATURE, JWT }
				 ]
			       })),

    ?debug("dlink_bt:announce_local_service(~p: ~p) -> ~p  Res: ~p", 
	   [ Availability, Service, ConnPid, Res]),

    %% Move on to next connection.
    announce_local_service_(CompSpec, 
			    T,
			    Service, Availability).

announce_local_service_(CompSpec, Service, Availability) ->
    ?debug("dlink_bt:announce_local_service(~p, ~p)", 
	   [ Service, Availability]),

    announce_local_service_(CompSpec, 
			    get_connections(),
			    Service, Availability).


process_data(_FromPid, RemoteBTAddr, RemoteChannel, ProtocolMod, Data, CompSpec) ->
    ?debug("dlink_bt:receive_data(): SetupAddress: {~p, ~p}", [ RemoteBTAddr, RemoteChannel ]),
    ?debug("dlink_bt:receive_data(): ~p:receive_message(~p)", [ ProtocolMod, Data ]),
    Proto = list_to_atom(ProtocolMod),
    Proto:receive_message(CompSpec, base64:decode_to_string(Data)),

    ok.

availability_msg(Availability, Services) ->
    {struct, [{?DLINK_ARG_STATUS, status_string(Availability)},
              {?DLINK_ARG_SERVICES, {array, Services}}]}.

status_string(available  ) -> ?DLINK_ARG_AVAILABLE;
status_string(unavailable) -> ?DLINK_ARG_UNAVAILABLE.
    

process_availability(Msg, FromPid, Addr, Channel, TID, CompSpec) ->
    {ok, Avail} = rvi_common:get_json_element([?DLINK_ARG_STATUS], Msg),
    {ok, Svcs} = rvi_common:get_json_element([?DLINK_ARG_SERVICES], Msg),
    ?debug("dlink_bt_rpc:service_announce(~p): Address:       ~p:~p", [Avail, Addr, Channel]),
    ?debug("dlink_bt_rpc:service_announce(~p): TransactionID: ~p", [Avail, TID]),
    ?debug("dlink_bt_rpc:service_announce(~p): Services:      ~p", [Avail, Svcs]),
    case Avail of
        ?DLINK_ARG_AVAILABLE ->
            add_services(Svcs, FromPid),
            service_discovery_rpc:register_services(CompSpec, Svcs, ?MODULE);
        ?DLINK_ARG_UNAVAILABLE ->
            delete_services(FromPid, Svcs),
            service_discovery_rpc:unregister_services(CompSpec, Svcs, ?MODULE)
    end.

process_authorize(FromPid,
		  PeerBTAddr, 
		  PeerBTChannel,
		  TransactionID, 
		  RemoteAddress, 
		  RemoteChannel, 
		  Protocol, 
		  Certificates, 
		  Signature,
		  CompSpec) ->

    ?info("dlink_bt:authorize(): Peer Address:   ~p:~p", [PeerBTAddr, PeerBTChannel ]),
    ?info("dlink_bt:authorize(): Remote Address: ~p~p", [ RemoteAddress, RemoteChannel ]),
    ?info("dlink_bt:authorize(): Protocol:       ~p",   [ Protocol ]),
    ?debug("dlink_bt:authorize(): TransactionID:  ~p",  [ TransactionID ]),
    ?debug("dlink_bt:authorize(): Certificates:   ~p",  [ Certificates ]),
    ?debug("dlink_bt:authorize(): Signature:      ~p",  [ Signature ]),

    %% If FromPid (the genserver managing the socket) is not yet registered
    %% with the conneciton manager, this is an incoming connection
    %% from the client. We should respond with our own authorize followed by
    %% a service announce
    
    Conn = {RemoteAddress, RemoteChannel},
    case validate_auth_jwt(Signature, Certificates, Conn, CompSpec) of
        true ->
            connection_authorized(FromPid, Conn, CompSpec);
        false ->
            %% close connection (how?)
            false
    end.

handle_socket(FromPid, PeerBTAddr, PeerChannel, data, 
	      Payload, CompSpec) ->

    {ok, {struct, Elems}} = exo_json:decode_string(binary_to_list(Payload)),
    ?debug("dlink_bt:data(): Got ~p", [ Elems ]),

    case opt(?DLINK_ARG_CMD, Elems, undefined) of
	?DLINK_CMD_AUTHORIZE ->
	    [ TransactionID, 
	      RemoteAddress, 
	      RemoteChannel, 
	      RVIProtocol,
	      CertificatesTmp,
	      Signature ] = 
		opts([?DLINK_ARG_TRANSACTION_ID, 
		      ?DLINK_ARG_ADDRESS, 
		      ?DLINK_ARG_PORT, 
		      ?DLINK_ARG_VERSION, 
		      ?DLINK_ARG_CERTIFICATES, 
		      ?DLINK_ARG_SIGNATURE], 
		     Elems, undefined),
	    
	    Certificates = 
		case CertificatesTmp of 
		    { array, C} -> C;
		    undefined -> []
	end,
	    process_authorize(FromPid, PeerBTAddr, RemoteChannel,
			     TransactionID, RemoteAddress, RemoteChannel, 
			     RVIProtocol,  Certificates, Signature, CompSpec);

			   
			   
	?DLINK_CMD_SERVICE_ANNOUNCE ->
            Conn = {PeerBTAddr, PeerChannel},
	    [ TransactionID, Signature ] = 
		opts([?DLINK_ARG_TRANSACTION_ID, ?DLINK_ARG_SIGNATURE],
		     Elems, undefined),
	    case authorize_rpc:validate_message(CompSpec, Signature, Conn) of
                [ok, Msg] ->
                    process_availability(
                      Msg, FromPid, PeerBTAddr, PeerChannel, TransactionID, CompSpec);
                _ ->
                    ?debug("Couldn't validate availability msg from ~p", [Conn])
            end;

	?DLINK_CMD_RECEIVE ->
	    [ _TransactionID, 
	      ProtocolMod, 
	      Data ] = 
		opts([?DLINK_ARG_TRANSACTION_ID, 
		      ?DLINK_ARG_MODULE, 
		      ?DLINK_ARG_DATA],
		     Elems, undefined),
	    process_data(FromPid, PeerBTAddr, PeerChannel,
			 ProtocolMod, Data, CompSpec);

	?DLINK_CMD_PING ->
	    ?info("dlink_bt:ping(): Pinged from: ~p:~p", [ PeerBTAddr, PeerChannel]),
	    ok;

	undefined -> 
	    ?warning("dlink_bt:data() cmd undefined., ~p", [ Elems ]),
	    ok
    end.


%% We lost the socket connection.
%% Unregister all services that were routed to the remote end that just died.
handle_socket(FromPid, SetupBTAddr, SetupChannel, closed, CompSpec) ->
    ?info("dlink_bt:closed(): SetupAddress:  {~p, ~p}", [ SetupBTAddr, SetupChannel ]),

    NetworkAddress = SetupBTAddr  ++ "-" ++ integer_to_list(SetupChannel),

    %% Get all service records associated with the given connection
    LostSvcNameList = get_services_by_connection(FromPid),

    delete_connection(FromPid),

    %% Check if this was our last connection supchanneling each given service.
    lists:map(
      fun(SvcName) ->
	      case get_connections_by_service(SvcName) of
		  [] ->
		      service_discovery_rpc:
			  unregister_services(CompSpec, 
					      [SvcName], 
					      ?MODULE);
		  _ -> ok
	      end
      end, LostSvcNameList),

    {ok, PersistentConnections } = rvi_common:get_module_config(data_link, 
								?MODULE, 
								persistent_connections, 
								[], 
								CompSpec),
    %% Check if this is a static node. If so, setup a timer for a reconnect
    case lists:member(NetworkAddress, PersistentConnections) of
	true ->
	    ?info("dlink_bt:closed(): Reconnect address:  ~p", [ NetworkAddress ]),
	    ?info("dlink_bt:closed(): Reconnect interval: ~p", [ ?DEFAULT_RECONNECT_INTERVAL ]),
	    [ BTAddr, Channel] = string:tokens(NetworkAddress, "-"),

	    setup_reconnect_timer(?DEFAULT_RECONNECT_INTERVAL, 
				  BTAddr, Channel, CompSpec);
	false -> ok
    end,
    ok;

handle_socket(FromPid, SetupBTAddr, SetupChannel, connected, CompSpec) ->
    ?info("dlink_bt:handle_socket(connected): {~p, ~p}", [ SetupBTAddr, SetupChannel ]),
    send_authorize(FromPid, SetupChannel, CompSpec),
    ok;


handle_socket(_FromPid, SetupBTAddr, SetupChannel, accepted, _ExtraArgs) ->
    ?info("dlink_bt:handle_socket(accepted): {~p, ~p}", [ SetupBTAddr, SetupChannel ]),
    ok;

handle_socket(_FromPid, SetupBTAddr, SetupChannel, error, _ExtraArgs) ->
    ?info("dlink_bt:socket_error(): SetupAddress:  {~p, ~p}", [ SetupBTAddr, SetupChannel ]),
    ok.

%% JSON-RPC entry point
%% CAlled by local exo http server
handle_notification("service_available", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),

    gen_server:cast(?SERVER, { rvi, service_available, 
				      [ SvcName,
					DataLinkModule ]}),

    ok;
handle_notification("service_unavailable", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),

    gen_server:cast(?SERVER, { rvi, service_unavailable, 
				      [ SvcName,
					DataLinkModule ]}),

    ok;

handle_notification(Other, _Args) ->
    ?info("dlink_bt:handle_notification(~p): unknown", [ Other ]),
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
    { ok, ProtoMod } = rvi_common:get_json_element(["proto_mod"], Args),
    { ok, Service } = rvi_common:get_json_element(["service"], Args),
    { ok,  Data } = rvi_common:get_json_element(["data"], Args),
    { ok,  DataLinkOpts } = rvi_common:get_json_element(["opts"], Args),
    [ Res ]  = gen_server:call(?SERVER, 
			       { rvi, send_data, 
				 [ProtoMod, Service, Data, DataLinkOpts]}),
    {ok, [ {status, rvi_common:json_rpc_status(Res)} ]};
    

handle_rpc(Other, _Args) ->
    ?info("dlink_bt:handle_rpc(~p): unknown", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


handle_cast( {rvi, service_available, [SvcName, local]}, St) ->
    ?debug("dlink_bt:service_available(): ~p (local)", [ SvcName ]),
    announce_local_service_(St#st.cs, SvcName, available),
    {noreply, St};


handle_cast( {rvi, service_available, [SvcName, Mod]}, St) ->
    ?debug("dlink_bt:service_available(): ~p (~p) ignored", [ SvcName, Mod ]),
    %% We don't care about remote services available through
    %% other data link modules
    {noreply, St};


handle_cast( {rvi, service_unavailable, [SvcName, local]}, St) ->
    announce_local_service_(St#st.cs, SvcName, unavailable),
    {noreply, St};

handle_cast( {rvi, service_unavailable, [_SvcName, _]}, St) ->
    %% We don't care about remote services available through 
    %% other data link modules
    {noreply, St};


handle_cast(Other, St) ->
    ?warning("dlink_bt:handle_cast(~p): unknown", [ Other ]),
    {noreply, St}.


handle_call({rvi, setup_data_link, [ Service, Opts ]}, _From, St) ->
    %% Do we already have a connection that supchannel service?
    case get_connections_by_service(Service) of
	[] -> %% Nope
	    case proplists:get_value(target, Opts, undefined) of
		undefined ->
		    ?info("dlink_bt:setup_data_link(~p) Failed: no target given in options.",
			  [Service]),
		    { reply, [ok, -1 ], St };

		Addr -> 
		    [ Address, Channel] =  string:tokens(Addr, "-"),

		    case connect_remote(Address, list_to_integer(Channel), St#st.cs) of
			ok  ->
			    { reply, [ok, 2000], St };  %% 2 second timeout

			already_connected ->  %% We are already connected
			    { reply, [already_connected, -1], St };  

			Err ->
			    { reply, [Err, 0], St }
		    end
	    end;

	_ ->  %% Yes - We do have a connection that knows of service
	    { reply, [already_connected, -1], St }
    end;


handle_call({rvi, disconnect_data_link, [NetworkAddress] }, _From, St) ->
    [ Address, Channel] = string:tokens(NetworkAddress, "-"),
    Res = bt_connection:terminate_connection(Address,Channel),
    { reply, [ Res ], St };


handle_call({rvi, send_data, [ProtoMod, Service, Data, _DataLinkOpts]}, _From, St) ->

    %% Resolve connection pid from service
    case get_connections_by_service(Service) of
	[] ->
	    { reply, [ no_route ], St};

	%% FIXME: What to do if we have multiple connections to the same service?
	[ConnPid | _T] -> 
	    ?debug("dlink_bt:send(~p): ~s", [ProtoMod, Data]),
	    Res = bt_connection:send(ConnPid, 
		       term_to_json(
			 {struct, 		     
			  [ { ?DLINK_ARG_TRANSACTION_ID, 1 },
			    { ?DLINK_ARG_CMD, ?DLINK_CMD_RECEIVE },
			    { ?DLINK_ARG_MODULE, atom_to_list(ProtoMod) },
			    { ?DLINK_ARG_DATA,  base64:encode_to_string(Data) }
			  ]})),
				     
	    { reply, [ Res ], St}
    end;
	    



handle_call({setup_initial_ping, Address, Channel, Pid}, _From, St) ->
    %% Create a timer to handle periodic pings.
    {ok, ServerOpts } = rvi_common:get_module_config(data_link, 
						     ?MODULE,
						     server_opts, [], 
						     St#st.cs),
    Timeout = proplists:get_value(ping_interval, ServerOpts, ?DEFAULT_PING_INTERVAL),

    ?info("dlink_bt:setup_ping(): ~p:~p will be pinged every ~p msec", 
	  [ Address, Channel, Timeout] ),
										      
    erlang:send_after(Timeout, self(), { rvi_ping, Pid, Address, Channel, Timeout }),

    {reply, ok, St};

handle_call(Other, _From, St) ->
    ?warning("dlink_bt:handle_rpc(~p): unknown", [ Other ]),
    { reply, { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ]}, St}.



%% Ping time
handle_info({ rvi_ping, Pid, Address, Channel, Timeout},  St) ->

    %% Check that connection is up
    case bt_connection:is_connection_up(Pid) of
	true ->
	    ?info("dlink_bt:ping(): Pinging: ~p:~p", [Address, Channel]),
	    bt_connection:send(Pid, term_to_json(
				      { struct, 
					[ { ?DLINK_ARG_CMD, 
					    ?DLINK_CMD_PING 
					  }]})),

	    erlang:send_after(Timeout, self(), 
			      { rvi_ping, Pid, Address, Channel, Timeout });

	false ->
	    ok
    end,
    {noreply, St};

%% Setup static nodes
handle_info({ rvi_setup_persistent_connection, BTAddr, Channel, CompSpec }, St) ->
    connect_and_retry_remote(BTAddr, Channel, CompSpec),
    { noreply, St };

handle_info(Info, St) ->
    ?notice("dlink_bt(): Unkown message: ~p", [ Info]),
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


send_authorize(Pid, SetupChannel, CompSpec) ->
    {ok,[{address, Address }]} = bt_drv:local_info([address]),
    bt_connection:send(Pid, 
		       term_to_json(
			 {struct, 		     
			  [ { ?DLINK_ARG_TRANSACTION_ID, 1 },
			    { ?DLINK_ARG_CMD, ?DLINK_CMD_AUTHORIZE },
			    { ?DLINK_ARG_ADDRESS, bt_address_to_string(Address) },
			    { ?DLINK_ARG_PORT,  SetupChannel },
                            { ?DLINK_ARG_VERSION, ?DLINK_BT_VER },
			    { ?DLINK_ARG_CERTIFICATES, {array, get_certificates(CompSpec)} },
			    { ?DLINK_ARG_SIGNATURE, get_authorize_jwt(CompSpec) } ]})).

connection_authorized(FromPid, {RemoteAddress, RemoteChannel} = Conn, CompSpec) ->
    case bt_connection_manager:find_connection_by_pid(FromPid) of
	not_found ->
	    ?info("dlink_bt:authorize(): New connection!"),
	    bt_connection_manager:add_connection(RemoteAddress, RemoteChannel, FromPid),
	    ?debug("dlink_bt:authorize(): Sending authorize."),
            Res = send_authorize(FromPid, RemoteChannel, CompSpec),
	    ?debug("dlink_bt:authorize(): Sending authorize: ~p", [ Res]),
	    ok;
	_ -> ok
    end,

    %% Send our own servide announcement to the remote server
    %% that just authorized to us.
    [ ok, LocalServices ] = service_discovery_rpc:get_services_by_module(CompSpec, local),
    
    [ ok, FilteredServices ] = authorize_rpc:filter_by_destination(
                                 CompSpec, LocalServices, Conn),

    %% Send an authorize back to the remote node
    ?info("dlink_bt:authorize(): Announcing local services: ~p to remote ~p:~p",
	  [FilteredServices, RemoteAddress, RemoteChannel]),

    [ ok, JWT ] = authorize_rpc:sign_message(
                    CompSpec, availability_msg(available, FilteredServices)),
    bt_connection:send(FromPid, 
		       term_to_json(
			 {struct, 		     
			  [ { ?DLINK_ARG_TRANSACTION_ID, 1 },
			    { ?DLINK_ARG_CMD, ?DLINK_CMD_SERVICE_ANNOUNCE },
			    { ?DLINK_ARG_SIGNATURE, JWT } ]})),

    %% Setup ping interval
    gen_server:call(?SERVER, { setup_initial_ping, RemoteAddress, RemoteChannel, FromPid }),
    ok.

setup_reconnect_timer(MSec, BTAddr, Channel, CompSpec) ->
    erlang:send_after(MSec, ?MODULE, 
		      { rvi_setup_persistent_connection, 
			BTAddr, Channel, CompSpec }),
    ok.


get_services_by_connection(ConnPid) ->
    case ets:lookup(?CONNECTION_TABLE, ConnPid) of
	[ #connection_entry { services = SvcNames } ] ->
	    SvcNames;
	[] -> []
    end.


get_connections_by_service(Service) ->
    case ets:lookup(?SERVICE_TABLE, Service) of
	[ #service_entry { connections = Connections } ] ->
	    Connections;
	[] -> []
    end.
		 

add_services(SvcNameList, ConnPid) ->
    %% Create or replace existing connection table entry
    %% with the sum of new and old services.
    ?debug("dlink_bt:add_services(~p, ~p)", [ ConnPid, SvcNameList]),
    ets:insert(?CONNECTION_TABLE, 
	       #connection_entry {
		  connection = ConnPid,
		  services = SvcNameList ++ get_services_by_connection(ConnPid)
	      }),

    %% Add the connection to the service entry for each service.
    [ ets:insert(?SERVICE_TABLE, 
	       #service_entry {
		  service = SvcName,
		  connections = [ConnPid | get_connections_by_service(SvcName)]
		 }) || SvcName <- SvcNameList ],

    ?debug("dlink_bt:_services(): CONN_TABLE: ~p", [ get_services_by_connection(ConnPid)]),
    [ ?debug("  dlink_bt:_services(): SVC_TABLE(~p) : ~p", [ Svc, get_connections_by_service(Svc)]) ||
	Svc <- SvcNameList ],
    ok.


delete_services(ConnPid, SvcNameList) ->
    ets:insert(?CONNECTION_TABLE, 
	       #connection_entry {
		  connection = ConnPid,
		  services = get_services_by_connection(ConnPid) -- SvcNameList
		 }),
    
    %% Loop through all services and update the conn table
    %% Update them with a new version where ConnPid has been removed
    [ ets:insert(?SERVICE_TABLE, 
		 #service_entry {
		  service = SvcName,
		  connections = get_connections_by_service(SvcName) -- [ConnPid]
		 }) || SvcName <- SvcNameList ],
    ok.

delete_connection(Conn) ->
    %% Create or replace existing connection table entry
    %% with the sum of new and old services.
    SvcNameList = get_services_by_connection(Conn),

    %% Replace each existing connection entry that has 
    %% SvcName with a new one where the SvcName is removed.
    lists:map(fun(SvcName) ->
		      Existing = get_connections_by_service(SvcName),
		      ets:insert(?SERVICE_TABLE, #
				     service_entry {
				       service = SvcName,
				       connections = Existing -- [ Conn ]
				      })
	      end, SvcNameList),
    
    %% Delete the connection
    ets:delete(?CONNECTION_TABLE, Conn),
    ok.

		 

get_connections('$end_of_table', Acc) ->
    Acc;

get_connections(Key, Acc) ->
    get_connections(ets:next(?CONNECTION_TABLE, Key), [ Key | Acc ]).

	    
get_connections() ->
    get_connections(ets:first(?CONNECTION_TABLE), []).


term_to_json(Term) ->
    binary_to_list(iolist_to_binary(exo_json:encode(Term))).

opt(K, L, Def) ->
    case lists:keyfind(K, 1, L) of
	{_, V} -> V;
	false  -> Def
    end.

opts(Keys, Elems, Def) ->
    [ opt(K, Elems, Def) || K <- Keys].

get_authorize_jwt(CompSpec) ->
    case authorize_rpc:get_authorize_jwt(CompSpec) of
	[ok, JWT] ->
	    JWT;
	[not_found] ->
	    ?error("No authorize JWT~n", []),
	    error(cannot_authorize)
    end.

get_certificates(CompSpec) ->
    case authorize_rpc:get_certificates(CompSpec) of
	[ok, Certs] ->
	    Certs;
	[not_found] ->
	    ?error("No certificate found~n", []),
	    error(no_certificate_found)
    end.

validate_auth_jwt(JWT, Certs, Conn, CompSpec) ->
    case authorize_rpc:validate_authorization(CompSpec, JWT, Certs, Conn) of
	[ok] ->
	    true;
	[not_found] ->
	    false
    end.
