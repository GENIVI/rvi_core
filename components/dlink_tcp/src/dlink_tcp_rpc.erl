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
-export([service_available/3,
	 service_unavailable/3]).
-export([connections/1]).

-export([setup_data_link/3,
	 disconnect_data_link/2,
	 send_data/5]).


-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").
-include_lib("rvi_common/include/rvi_dlink_bin.hrl").

-define(PERSISTENT_CONNECTIONS, persistent_connections).
-define(SERVER_OPTS, server_opts).
-define(DEFAULT_TCP_PORT, 9999).
-define(DEFAULT_RECONNECT_INTERVAL, 5000).
-define(DEFAULT_TCP_ADDRESS, "0.0.0.0").
-define(DEFAULT_PING_INTERVAL, 300000).  %% Five minutes
-define(SERVER, ?MODULE).
-define(DLINK_TCP_VERSION, "1.1").

-define(CONNECTION_TABLE, rvi_dlink_tcp_connections).
-define(SERVICE_TABLE, rvi_dlink_tcp_services).

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

init([]) ->
    ?info("dlink_tcp:init(): Called"),
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
    rvi_common:start_json_rpc_server(data_link, ?MODULE, dlink_tcp_sup).


start_connection_manager() ->
    %% Fire up listener
    CompSpec = rvi_common:get_component_specification(),
    %% connection_manager:start_link(),
    ?info("dlink_tcp:init_rvi_component(~p): Starting listener.", [self()]),
    %% {ok,Pid} = listener:start_link(),
    %%
    setup_initial_listeners(CompSpec),

    ?info("dlink_tcp:init_rvi_component(): Setting up persistent connections."),

    {ok, PersistentConnections } = rvi_common:get_module_config(data_link,
								?MODULE,
								?PERSISTENT_CONNECTIONS,
								[],
								CompSpec),

    setup_persistent_connections_(PersistentConnections, CompSpec),
    ok.


setup_initial_listeners(CompSpec) ->
    case rvi_common:get_module_config(data_link,
				      ?MODULE,
				      ?SERVER_OPTS,
				      CompSpec) of
	{ok, ServerOpts} ->
	    IP = proplists:get_value(ip, ServerOpts, ?DEFAULT_TCP_ADDRESS),
	    Port = proplists:get_value(port, ServerOpts, ?DEFAULT_TCP_PORT),
	    ?info("dlink_tcp:init_rvi_component(): Adding listener ~p:~p", [ IP, Port ]),
	    %%
	    %% Add listener port.
	    case listener:add_listener(IP, Port, CompSpec) of
		ok ->
		    ?notice("---- RVI Node External Address: ~s",
			    [ application:get_env(rvi_core, node_address, undefined)]);
		Err ->
		    ?warning(
		       "dlink_tcp:init_rvi_component(): Failed to launch listener (~p,~p): ~p",
		       [ IP, Port, Err ]),
		    ok
	    end;
	{error, _} ->
	    ?info("dlink_tcp: no initial listeners", [])
    end.

setup_persistent_connections_([ ], _CompSpec) ->
     ok;

setup_persistent_connections_([ NetworkAddress | T], CompSpec) ->
    ?debug("~p: Will persistently connect connect : ~p", [self(), NetworkAddress]),
    [ IP, Port] =  string:tokens(NetworkAddress, ":"),
    connect_and_retry_remote(IP, Port, CompSpec),
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

connections(_CompSpec) ->
    rvi_common:request(data_link, ?MODULE, connections, []).

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
connect_remote(IP, Port, CompSpec) ->
    ?info("connect_remote(~p, ~p)~n", [IP, Port]),
    case connection_manager:find_connection_by_address(IP, Port) of
	{ ok, _Pid } ->
	    log("already connected", [], CompSpec),
	    already_connected;

	not_found ->
	    %% Setup a new outbound connection
	    ?info("dlink_tcp:connect_remote(): Connecting ~p:~p",
		  [IP, Port]),
	    log("new connection", [], CompSpec),
	    case gen_tcp:connect(IP, Port, listener:sock_opts()) of
		{ ok, Sock } ->
		    ?info("dlink_tcp:connect_remote(): Connected ~p:~p",
			   [IP, Port]),

		    %% Setup a genserver around the new connection.
		    {ok, Pid } = connection:setup(IP, Port, Sock,
						  ?MODULE, handle_socket, CompSpec ),

		    %% Send authorize
		    send_authorize(Pid, CompSpec),
		    ok;

		{error, Err } ->
		    ?info("dlink_tcp:connect_remote(): Failed ~p:~p: ~p",
			   [IP, Port, Err]),
		    log("connect FAILED: ~w", [Err], CompSpec),
		    not_available
	    end
    end.

connect_and_retry_remote( IP, Port, CompSpec) ->
    ?info("dlink_tcp:connect_and_retry_remote(): ~p:~p",
	  [ IP, Port]),

    CS = start_log(<<"conn">>, "connect ~s:~s", [IP, Port], CompSpec),
    case connect_remote(IP, list_to_integer(Port), CS) of
	ok  ->
	    log(result, "connected", [], CS),
	    ok;

	Err -> %% Failed to connect. Sleep and try again
	    ?notice("dlink_tcp:connect_and_retry_remote(~p:~p): Failed: ~p",
			   [IP, Port, Err]),

	    ?notice("dlink_tcp:connect_and_retry_remote(~p:~p): Will try again in ~p sec",
			   [IP, Port, ?DEFAULT_RECONNECT_INTERVAL]),
	    log("start reconnect timer", [], CS),
	    setup_reconnect_timer(?DEFAULT_RECONNECT_INTERVAL, IP, Port, CS),

	    not_available
    end.


announce_local_service_(_CompSpec, [], _Service, _Availability) ->
    ok;

announce_local_service_(CompSpec,
			[ConnPid | T],
			Service, Availability) ->

    Msg = availability_msg(Availability, [Service], CompSpec),
    Res = connection:send(ConnPid, Msg),

    ?debug("dlink_tcp:announce_local_service(~p: ~p) -> ~p  Res: ~p",
	   [ Availability, Service, ConnPid, Res]),

    %% Move on to next connection.
    announce_local_service_(CompSpec,
			    T,
			    Service, Availability).

announce_local_service_(CompSpec, Service, Availability) ->
    announce_local_service_(CompSpec,
			    get_connections(),
			    Service, Availability).

handle_socket(FromPid, IP, Port, Event, Arg) ->
    gen_server:cast(?SERVER, {handle_socket, FromPid, IP, Port, Event, Arg}).

handle_socket(FromPid, IP, Port, Event, Payload, Arg) ->
    gen_server:cast(
      ?SERVER, {handle_socket, FromPid, IP, Port, Event, Payload, Arg}).

%% We lost the socket connection.
%% Unregister all services that were routed to the remote end that just died.
handle_socket_(FromPid, undefined, SetupPort, closed, Arg) ->
    handle_socket(FromPid, "0.0.0.0", SetupPort, closed, Arg);

handle_socket_(FromPid, SetupIP, SetupPort, closed, CompSpec) ->
    ?info("dlink_tcp:closed(): SetupAddress:  {~p, ~p}", [ SetupIP, SetupPort ]),

    NetworkAddress = SetupIP  ++ ":" ++ integer_to_list(SetupPort),

    %% Get all service records associated with the given connection
    LostSvcNameList = get_services_by_connection(FromPid),

    delete_connection(FromPid),

    %% Check if this was our last connection supporting each given service.
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
	    ?info("dlink_tcp:closed(): Reconnect address:  ~p", [ NetworkAddress ]),
	    ?info("dlink_tcp:closed(): Reconnect interval: ~p", [ ?DEFAULT_RECONNECT_INTERVAL ]),
	    [ IP, Port] = string:tokens(NetworkAddress, ":"),

	    setup_reconnect_timer(?DEFAULT_RECONNECT_INTERVAL,
				  IP, Port, CompSpec);
	false -> ok
    end,
    ok;

handle_socket_(_FromPid, SetupIP, SetupPort, error, _ExtraArgs) ->
    ?info("dlink_tcp:socket_error(): SetupAddress:  {~p, ~p}", [ SetupIP, SetupPort ]),
    log_orphan(<<"sock">>, "socket ERROR ~s:~w", [SetupIP, SetupPort]),
    ok.

handle_socket_(FromPid, PeerIP, PeerPort, data, Elems, CompSpec) ->

    ?debug("data(): Elems ~p", [authorize_keys:abbrev_payload(Elems)]),

    CS = rvi_common:pick_up_json_log_id(Elems, CompSpec),

    case opt(?DLINK_ARG_CMD, Elems, undefined) of
        ?DLINK_CMD_AUTHORIZE ->
	    ?debug("got authorize ~s:~w", [PeerIP, PeerPort]),
            [ RemoteAddress,
              RemotePort,
              ProtoVersion,
	      Credentials ] =
                opts([?DLINK_ARG_ADDRESS,
                      ?DLINK_ARG_PORT,
                      ?DLINK_ARG_VERSION,
		      ?DLINK_ARG_CREDENTIALS],
                     Elems, undefined),

            process_authorize(FromPid, PeerIP, PeerPort,
                              RemoteAddress, RemotePort,
                              ProtoVersion, Credentials, CS);

        ?DLINK_CMD_SERVICE_ANNOUNCE ->
	    ?debug("got service_announce ~s:~w", [PeerIP, PeerPort]),
            [ Status,
	      Services ] =
                opts([?DLINK_ARG_STATUS,
		      ?DLINK_ARG_SERVICES],
                     Elems, undefined),

	    log("sa from ~s:~w", [PeerIP, PeerPort], CS),
	    process_announce(Status, Services, FromPid, PeerIP, PeerPort, CompSpec);

        ?DLINK_CMD_RECEIVE ->
            [ _TransactionID,
              ProtoMod,
              Data ] =
                opts([?DLINK_ARG_TRANSACTION_ID,
                      ?DLINK_ARG_MODULE,
                      ?DLINK_ARG_DATA],
                     Elems, undefined),
            process_data(FromPid, PeerIP, PeerPort,
                         ProtoMod, Data, CompSpec);

        ?DLINK_CMD_PING ->
            ?info("dlink_tcp:ping(): Pinged from: ~p:~p", [ PeerIP, PeerPort ]),
            ok;

        undefined ->
            ?warning("dlink_tcp:data() cmd undefined, ~p", [ Elems ]),
            ok
    end.

%% JSON-RPC entry point
%% CAlled by local exo http server
handle_notification("service_available", Args) ->
    {ok, SvcName} = rvi_common:get_json_element([<<"service">>], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element([<<"data_link_module">>], Args),

    gen_server:cast(?SERVER, { rvi, service_available,
				      [ SvcName,
					DataLinkModule ]}),

    ok;
handle_notification("service_unavailable", Args) ->
    {ok, SvcName} = rvi_common:get_json_element([<<"service">>], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element([<<"data_link_module">>], Args),

    gen_server:cast(?SERVER, { rvi, service_unavailable,
				      [ SvcName,
					DataLinkModule ]}),

    ok;

handle_notification(Other, _Args) ->
    ?info("dlink_tcp:handle_notification(~p): unknown", [ Other ]),
    ok.

handle_rpc("setup_data_link", Args) ->
    { ok, Service } = rvi_common:get_json_element([<<"service">>], Args),

    { ok, Opts } = rvi_common:get_json_element([<<"opts">>], Args),

    [ Res, Timeout ] = gen_server:call(?SERVER, { rvi, setup_data_link,
						  [ Service, Opts ] }),

    {ok, [ {status, rvi_common:json_rpc_status(Res)} , { timeout, Timeout }]};

handle_rpc("disconnect_data_link", Args) ->
    { ok, NetworkAddress} = rvi_common:get_json_element([<<"network_address">>], Args),
    [Res] = gen_server:call(?SERVER, { rvi, disconnect_data_link, [NetworkAddress]}),
    {ok, [ {status, rvi_common:json_rpc_status(Res)} ]};

handle_rpc("send_data", Args) ->
    { ok, ProtoMod } = rvi_common:get_json_element([<<"proto_mod">>], Args),
    { ok, Service } = rvi_common:get_json_element([<<"service">>], Args),
    { ok,  Data } = rvi_common:get_json_element([<<"data">>], Args),
    { ok,  DataLinkOpts } = rvi_common:get_json_element([<<"opts">>], Args),
    [ Res ]  = gen_server:call(?SERVER, { rvi, send_data, [ProtoMod, Service, Data, DataLinkOpts]}),
    {ok, [ {status, rvi_common:json_rpc_status(Res)} ]};

handle_rpc("connections", []) ->
    Res = gen_server:call(?SERVER, connections),
    {ok, [ {status, ok} | {connections, Res} ]};

handle_rpc(Other, _Args) ->
    ?info("dlink_tcp:handle_rpc(~p): unknown", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


handle_cast( {rvi, service_available, [SvcName, local]}, St) ->
    ?debug("dlink_tcp:service_available(): ~p (local)", [ SvcName ]),
    announce_local_service_(St#st.cs, SvcName, available),
    {noreply, St};


handle_cast( {rvi, service_available, [SvcName, Mod]}, St) ->
    ?debug("dlink_tcp:service_available(): ~p (~p) ignored", [ SvcName, Mod ]),
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

handle_cast({handle_socket, FromPid, IP, Port, Event, Arg}, St) ->
    ?debug("handle_socket, Arg (CS) = ~p", [Arg]),
    try handle_socket_(FromPid, IP, Port, Event, Arg)
    catch
	C:E ->
	    ?debug("Caught ~p:~p; ~p", [C, E, erlang:get_stacktrace()]),
	    error("Caught ~p:~p", [C, E]),
	    ok
    end,
    {noreply, St};

handle_cast({handle_socket, FromPid, IP, Port, Event, Payload, Arg}, St) ->
    try handle_socket_(FromPid, IP, Port, Event, Payload, Arg)
    catch
	C:E ->
	    ?debug("Caught ~p:~p; ~p", [C, E, erlang:get_stacktrace()]),
	    ok
    end,
    {noreply, St};

handle_cast({setup_initial_ping, Address, Port, Pid} = Msg, St) ->
    %% Create a timer to handle periodic pings.
    ?debug("~p:handle_call(~p)", [?MODULE, Msg]),
    {ok, ServerOpts } = rvi_common:get_module_config(data_link,
						     ?MODULE,
						     ?SERVER_OPTS, [],
						     St#st.cs),
    Timeout = proplists:get_value(ping_interval, ServerOpts, ?DEFAULT_PING_INTERVAL),

    ?info("dlink_tcp:setup_ping(): ~p:~p will be pinged every ~p msec",
	  [ Address, Port, Timeout] ),

    erlang:send_after(Timeout, self(), { rvi_ping, Pid, Address, Port, Timeout }),

    {noreply, St};

handle_cast(Other, St) ->
    ?warning("dlink_tcp:handle_cast(~p): unknown", [ Other ]),
    {noreply, St}.


handle_call({rvi, setup_data_link, [ Service, Opts ]}, _From, St) ->
    %% Do we already have a connection that support service?
    ?info("dlink_tcp: setup_data_link (~p, ~p)~n", [Service, Opts]),
    case get_connections_by_service(Service) of
	[] -> %% Nop[e
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
    [ Address, Port] = string:tokens(NetworkAddress, ":"),
    Res = connection:terminate_connection(Address,Port),
    { reply, [ Res ], St };


handle_call({rvi, send_data, [ProtoMod, Service, Data, _DataLinkOpts]}, _From, St) ->

    %% Resolve connection pid from service
    case get_connections_by_service(Service) of
	[] ->
	    { reply, [ no_route ], St};

	%% FIXME: What to do if we have multiple connections to the same service?
	[ConnPid | _T] ->
 	    Res = connection:send(ConnPid,
				  [ { ?DLINK_ARG_TRANSACTION_ID, 1 },
				    { ?DLINK_ARG_CMD, ?DLINK_CMD_RECEIVE },
				    { ?DLINK_ARG_MODULE, atom_to_binary(ProtoMod, latin1) },
				    { ?DLINK_ARG_DATA, Data }
				  ]),
	    { reply, [ Res ], St}
    end;

handle_call(Other, _From, St) ->
    ?warning("dlink_tcp:handle_rpc(~p): unknown", [ Other ]),
    { reply, { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ]}, St}.



%% Ping time
handle_info({ rvi_ping, Pid, Address, Port, Timeout},  St) ->

    %% Check that connection is up
    case connection:is_connection_up(Pid) of
	true ->
	    ?info("dlink_tcp:ping(): Pinging: ~p:~p", [Address, Port]),
 	    connection:send(Pid, jsx:encode([{?DLINK_ARG_CMD, ?DLINK_CMD_PING}])),
	    erlang:send_after(Timeout, self(),
			      { rvi_ping, Pid, Address, Port, Timeout });

	false ->
	    ok
    end,
    {noreply, St};

%% Setup static nodes
handle_info({ rvi_setup_persistent_connection, IP, Port, CompSpec }, St) ->
    ?info("rvi_setup_persistent_connection, ~p, ~p~n", [IP, Port]),
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
		      { rvi_setup_persistent_connection,
			IP, Port, CompSpec }),
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
    ets:insert(?CONNECTION_TABLE,
	       #connection_entry {
		  connection = ConnPid,
		  services = SvcNameList ++ get_services_by_connection(ConnPid)
	      }),

    %% Add the connection to the service entry for each servic.
    [ ets:insert(?SERVICE_TABLE,
	       #service_entry {
		  service = SvcName,
		  connections = [ConnPid | get_connections_by_service(SvcName)]
		 }) || SvcName <- SvcNameList ],
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

availability_msg(Availability, Services, CompSpec) ->
    [{ ?DLINK_ARG_CMD, ?DLINK_CMD_SERVICE_ANNOUNCE },
     { ?DLINK_ARG_STATUS, status_string(Availability) },
     { ?DLINK_ARG_SERVICES, Services }
     | log_id_tail(CompSpec) ].

status_string(available  ) -> ?DLINK_ARG_AVAILABLE;
status_string(unavailable) -> ?DLINK_ARG_UNAVAILABLE.

process_authorize(FromPid, PeerIP, PeerPort, RemoteAddress,
		  RemotePort, ProtoVersion, Credentials, CompSpec) ->
    ?info("dlink_tcp:authorize(): Peer Address:   ~p:~p", [PeerIP, PeerPort ]),
    ?info("dlink_tcp:authorize(): Remote Address: ~p~p", [ RemoteAddress, RemotePort ]),
    ?info("dlink_tcp:authorize(): Protocol Ver:   ~p", [ ProtoVersion ]),
    ?debug("dlink_tcp:authorize(): Credentials:   ~p", [ [authorize_keys:abbrev_bin(C) || C <- Credentials] ]),
    {NRemoteAddress, NRemotePort} = Conn =
        case { RemoteAddress, RemotePort } of
            { <<"0.0.0.0">>, 0 } ->

                ?info("dlink_tcp:authorize(): Remote is behind firewall. Will use ~p:~p",
                      [ PeerIP, PeerPort]),
                { PeerIP, PeerPort };
            _ -> { RemoteAddress, RemotePort}
        end,

    log(result, "auth ~s:~w", [NRemoteAddress, NRemotePort], CompSpec),
    authorize_rpc:store_creds(CompSpec, Credentials, Conn),
    connection_authorized(FromPid, Conn, CompSpec).

send_authorize(Pid, CompSpec) ->
    {LocalIP, LocalPort} = rvi_common:node_address_tuple(),
    connection:send(Pid,
		    [{ ?DLINK_ARG_CMD, ?DLINK_CMD_AUTHORIZE },
		     { ?DLINK_ARG_ADDRESS, LocalIP },
		     { ?DLINK_ARG_PORT,  integer_to_binary(LocalPort) },
		     { ?DLINK_ARG_VERSION, ?DLINK_TCP_VERSION },
		     { ?DLINK_ARG_CREDENTIALS, get_credentials(CompSpec) }
		     | log_id_tail(CompSpec) ]).

connection_authorized(FromPid, {RemoteIP, RemotePort} = Conn, CompSpec) ->
    %% If FromPid (the genserver managing the socket) is not yet registered
    %% with the conneciton manager, this is an incoming connection
    %% from the client. We should respond with our own authorize followed by
    %% a service announce
    log("authorized: ~s:~p", [RemoteIP, RemotePort], CompSpec),
    case connection_manager:find_connection_by_pid(FromPid) of
	not_found ->
	    ?info("dlink_tcp:authorize(): New connection!"),
	    connection_manager:add_connection(RemoteIP, RemotePort, FromPid),
	    ?debug("dlink_tcp:authorize(): Sending authorize."),
            Res = send_authorize(FromPid, CompSpec),
	    ?debug("dlink_tcp:authorize(): Sending authorize: ~p", [ Res]),
	    ok;
	_ -> ok
    end,

    %% Send our own servide announcement to the remote server
    %% that just authorized to us.
    [ ok, LocalServices ] = service_discovery_rpc:get_services_by_module(CompSpec, local),

    [ ok, FilteredServices ] = authorize_rpc:filter_by_service(
                                 CompSpec, LocalServices, Conn),

    %% Send an authorize back to the remote node
    ?info("dlink_tcp:authorize(): Announcing local services: ~p to remote ~p:~p",
	  [FilteredServices, RemoteIP, RemotePort]),

    AvailabilityMsg = availability_msg(available, FilteredServices, CompSpec),
    log("sending sa: ~s:~w", [RemoteIP, RemotePort], CompSpec),
    connection:send(FromPid, AvailabilityMsg),
    %% Setup ping interval
    gen_server:cast(?SERVER, { setup_initial_ping, RemoteIP, RemotePort, FromPid }),
    ok.

process_data(_FromPid, RemoteIP, RemotePort, ProtocolMod, Data, CompSpec) ->
    ?debug("dlink_tcp:receive_data(): RemoteAddr: {~p, ~p}", [ RemoteIP, RemotePort ]),
    ?debug("dlink_tcp:receive_data(): ~p:receive_message(~p)", [ ProtocolMod, Data ]),
    Proto = binary_to_existing_atom(ProtocolMod, latin1),
    Proto:receive_message(CompSpec, {RemoteIP, RemotePort}, Data).

process_announce(Avail, Services, FromPid, IP, Port, CompSpec) ->
    ?debug("dlink_tcp:service_announce(~p): Address:       ~p:~p", [Avail,IP,Port]),
    ?debug("dlink_tcp:service_announce(~p): Services:      ~p",    [Avail,Services]),
    case Avail of
	?DLINK_ARG_AVAILABLE ->
	    add_services(Services, FromPid),
	    service_discovery_rpc:register_services(CompSpec, Services, ?MODULE);
	?DLINK_ARG_UNAVAILABLE ->
	    delete_services(FromPid, Services),
	    service_discovery_rpc:unregister_services(CompSpec, Services, ?MODULE)
    end,
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

get_credentials(CompSpec) ->
    case authorize_rpc:get_credentials(CompSpec) of
	[ok, Creds] ->
	    Creds;
	[not_found] ->
	    ?error("No credentials found~n", []),
	    error(no_credentials_found)
    end.

opt(K, L, Def) ->
    case lists:keyfind(K, 1, L) of
	{_, V} -> V;
	false  -> Def
    end.

opts(Keys, Elems, Def) ->
    [ opt(K, Elems, Def) || K <- Keys].


log_orphan(Pfx, Fmt, Args) ->
    start_log(Pfx, Fmt, Args, #component_spec{}).

start_log(Pfx, Fmt, Args, CS) ->
    LogId = rvi_log:new_id(Pfx),
    rvi_log:log(LogId, <<"dlink_tcp">>, rvi_log:format(Fmt, Args)),
    rvi_common:set_value(rvi_log_id, LogId, CS).

log(Fmt, Args, CS) ->
    log(info, Fmt, Args, CS).

log(Lvl, Fmt, Args, CS) ->
    rvi_log:flog(Lvl, Fmt, Args, <<"dlink_tcp">>, CS).

log_id_tail(CompSpec) ->
    rvi_common:log_id_json_tail(CompSpec).
