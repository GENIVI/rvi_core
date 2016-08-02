%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(service_edge_rpc).
-behaviour(gen_server).

-export([handle_rpc/2]).
-export([handle_notification/2]).
-export([handle_websocket/3]).


-export([start_link/0,
	 register_service/3]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([rpc/2,     %% (Service, Params)
	 rpc/3,     %% (Service, Timeout, Params)
	 msg/2,     %% (Service, Params)
	 msg/3      %% (Service, Timeout, Params)
	]).

-export([handle_remote_message/5,
	 handle_local_timeout/3]).

-export([start_json_server/0,
	 start_websocket/0]).

%% exo_socket authentication callbacks
-export([authenticate/3,
         incoming/2]).

%% Invoked by service discovery
%% FIXME: Should be rvi_service_discovery behavior
-export([service_available/3,
	 service_unavailable/3]).

-export([record_fields/1]).

%%-include_lib("lhttpc/include/lhttpc.hrl").
-include_lib("lager/include/log.hrl").

-include_lib("rvi_common/include/rvi_common.hrl").

-include_lib("trace_runner/include/trace_runner.hrl").

-define(SERVER, ?MODULE).
-define(LONG_TIMEOUT, 60000).

-record(st, {
	  %% Component specification
	  cs = #component_spec{},
	  pending = []
	 }).

-define(SERVICE_TABLE, rvi_local_services).

-record(service_entry, {
	  service = "",       %% Servie handled by this entry.
	  url = undefined,    %% URL where the service can be reached.
	  opts = []
	 }).

record_fields(service_entry)	-> record_info(fields, service_entry);
record_fields(st           )	-> record_info(fields, st);
record_fields(component_spec)	-> record_info(fields, component_spec);
record_fields(_)		-> no.

rpc(Service, Args) ->
    rpc(Service, 10000, Args).

rpc(Service, Timeout, Args0) ->
    Args = case is_synch(Args0) of
	       false    -> [{<<"rvi.synch">>, true}|Args0];
	       {true,_} -> Args0
	   end,
    rvi_common:request(service_edge, ?MODULE, message,
		       [{<<"service_name">>, service_name(Service)},
			{<<"timeout">>, Timeout},
			{<<"parameters">>, Args}],
		       [status, result],
		       rvi_common:get_component_specification()).

msg(Service, Args) ->
    msg(Service, 10000, Args).

msg(Service, Timeout, Args) ->
    rvi_common:request(service_edge, ?MODULE, message,
		       [{<<"service_name">>, service_name(Service)},
			{<<"timeout">>, Timeout},
			{<<"parameters">>, Args}],
		       [status, tid],
		       rvi_common:get_component_specification()).

service_name(<<"$PFX", Rest/binary>>) ->
    Pfx = rvi_common:local_service_prefix(),
    re:replace(<<Pfx/binary, Rest/binary>>, <<"//">>, <<"/">>,
	       [global, {return, binary}]);
service_name(Svc) ->
    Svc.


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% Called by service_edge_app:start_phase().
init([]) ->
    CompSpec = rvi_common:get_component_specification(),

    URL = rvi_common:get_module_json_rpc_url(service_edge,
					     ?MODULE,
					     CompSpec),

    ?notice("---- Service Edge URL:          ~p", [ URL ]),
    ?notice("---- Node Service Prefix:       ~s", [ rvi_common:local_service_prefix()]),

    ets:new(?SERVICE_TABLE, [ set, public, named_table,
			     { keypos, #service_entry.service }]),

    service_discovery_rpc:subscribe(CompSpec, ?MODULE),
    {ok, #st {
	    cs = CompSpec
	   }}.

register_service(CompSpec, SvcName, URL) ->
    ?debug("register_service()~n", []),
    rvi_common:request(service_edge, ?MODULE, register_service,
		       [{<<"service">>, SvcName},
			{<<"network_address">>, URL}],
		       [status, service, method], CompSpec).

start_json_server() ->
    Allowed = get_allowed(),
    ?debug("service_edge_rpc:start_json_server()"),
    Opts = [{exo_socket,
	     [{auth,
	       [{role, server},
		{server,
		 [{mod, ?MODULE},
		  {allowed, Allowed}
		 ]}
	       ]}
	     ]}
	   ],
    case rvi_common:start_json_rpc_server(service_edge,
					  ?MODULE,
					  service_edge_sup,
					  Opts) of
	ok ->
	    ok;

	Err ->
	    ?warning("service_edge_rpc:start_json_server(): Failed to start: ~p",
		    [Err]),
	    Err
    end.

get_allowed() ->
    Allowed = case rvi_common:get_module_config(
                     service_edge, service_edge_rpc, allowed) of
                  {ok, L}    -> L;
                  {error, _} -> [{127,0,0,1}]
        end,
    ?debug("get_allowed(); Allowed = ~p", [Allowed]),
    lists:flatmap(
      fun(Addr) ->
              case inet:ip(Addr) of
                  {ok, IP}   -> [IP];
                  {error, _} -> []
              end
      end, Allowed).

authenticate(X, Role, Opts) ->
    {ok, {PeerIP,_}} = exo_socket:peername(X),
    ?debug("authenticate(~p, ~p, ~p)~nPeer = ~p~n", [X, Role, Opts, PeerIP]),
    case lists:keyfind(allowed, 1, Opts) of
        {_, Allowed} ->
            case lists:member(PeerIP, Allowed) of
                true ->
                    {ok, Opts};
                false ->
                    error
            end;
        false ->
            {ok, Opts}
    end.

incoming(Data, _) ->
    Data.

start_websocket() ->
    %%
    %% Fire up the websocket subsystem, if configured
    %%
    case rvi_common:get_module_config(service_edge,
				      service_edge_rpc,
				      websocket,
				      not_found,
				      rvi_common:get_component_specification()) of
	{ok, not_found} ->
 	    ?notice("service_edge:init(): No websocket config specified. Will use JSON-RPC/HTTP only."),
	    ok;

	{ ok, WSOpts } ->
	    case proplists:get_value(port, WSOpts, undefined ) of
		undefined ->
		    ok;

		Port ->
		    %% FIXME: MONITOR AND RESTART
		    wse_server:start(Port,
				     ?MODULE, handle_websocket, undefined,
				     [{type, text} | proplists:delete(port, WSOpts)]),
		    ok
	    end
    end.




%% Invoked by service_discovery to announce service availability
%% Must be handled either as a JSON-RPC call or a gen_server call.
service_available(CompSpec, SvcName, DataLinkModule) ->
    ?event({service_available, SvcName}),
    rvi_common:notification(service_edge, ?MODULE,
			    service_available,
			    [{ service, SvcName },
			     { data_link_module, DataLinkModule }], CompSpec).


service_unavailable(CompSpec, SvcName, DataLinkModule) ->
    ?event({service_unavailable, SvcName}),
    rvi_common:notification(service_edge, ?MODULE,
			    service_unavailable,
			    [{ service, SvcName },
			     { data_link_module, DataLinkModule }], CompSpec).

handle_remote_message(CompSpec, Conn, SvcName, Timeout, Params) ->
    ?event({handle_remote_message, [Conn, SvcName, Timeout, Params]}),
    {IP, Port} = Conn,
    rvi_common:notification(service_edge, ?MODULE,
			    handle_remote_message,
			    [{ ip, IP },
			     { port, Port },
			     { service, SvcName },
			     { timeout, Timeout },
			     { parameters, Params }], CompSpec).


%% Invoked by schedule_rpc.
%% A message originated from a locally connected service
%% has timed out
handle_local_timeout(CompSpec, SvcName, TransID) ->
    ?event({handle_local_timeout, [SvcName, TransID]}),
    rvi_common:notification(service_edge, ?SERVER, handle_local_timeout,
			    [ { service, SvcName},
			      { transaction_id, TransID} ],
			    CompSpec).

handle_websocket(WSock, Mesg, Arg) ->
    Decoded = try jsx:decode(Mesg)
	      catch error:E0 ->
		      ?debug("Failed decode of ~p: ~p", [Mesg, E0]),
		      Mesg
	      end,
    ?event({handle_websocket, Decoded}),
    ?debug("Decoded Mesg = ~p", [Decoded]),
    { ok, Method } = rvi_common:get_json_element(["method"], Mesg),
    { ok, Params0 } = rvi_common:get_json_element(["params"], Mesg),
    { ok, ID } = rvi_common:get_json_element(["id"], Mesg),
    Params = try jsx:decode(Params0)
	     catch error:E ->
		     ?debug("Failed decode of ~p:~p", [Params0, E]),
		     Params0
	     end,
    ?debug("service_edge_rpc:handle_websocket(~p/~p) method:      ~p", [ WSock, ID,Method ]),

    case handle_ws_json_rpc(WSock, Method, Params, Arg) of
        ok -> ok;
	{ok, Reply} ->
	    EncReply = rvi_common:term_to_json([{id, ID} |Reply]),
	    ?debug("service_edge_rpc:handle_websocket(~p/~p) reply:      ~s", [ WSock, ID, EncReply]),
	    wse_server:send(WSock, iolist_to_binary(EncReply))
    end,
    ok.

%% Websocket interface
handle_ws_json_rpc(WSock, <<"message">>, Params, _Arg ) ->
    { ok, SvcName0 } = rvi_common:get_json_element(["service_name"], Params),
    { ok, Timeout } = rvi_common:get_json_element(["timeout"], Params),
    { ok, Parameters0 } = rvi_common:get_json_element(["parameters"], Params),
    Files = rvi_common:get_opt_json_element(["rvi.files"], [], Params),
    ?debug("Files = ~p", [Files]),
    SvcName = iolist_to_binary(SvcName0),
    Parameters = append_files_to_params(Files, Parameters0),
    ?event({message, ws, [SvcName, Timeout, Parameters]}),
    ?debug("WS Parameters: ~p", [Parameters]),
    %% Parameters = parse_ws_params(Parameters0),
    LogId = log_id_json_tail(Params ++ Parameters),
    ?debug("service_edge_rpc:handle_websocket(~p) params!:      ~p", [ WSock, Params ]),
    ?debug("service_edge_rpc:handle_websocket(~p) service:      ~p", [ WSock, SvcName ]),
    ?debug("service_edge_rpc:handle_websocket(~p) parameters:   ~p", [ WSock, Parameters ]),

    case gen_server:call(
	   ?SERVER,
	   {rvi, handle_local_message,
	    [ SvcName, Timeout, Parameters | LogId ]}, ?LONG_TIMEOUT) of
	[not_found] ->
	    {ok, [{status, rvi_common:json(not_found)}]};
	[Res, TID] ->
	    ?debug("service_edge_rpc:wse_message(~p) Res:      ~p", [ WSock, Res ]),
	    { ok, [ { status, rvi_common:json_rpc_status(Res) },
		    { transaction_id, TID},
		    { method, <<"message">>}] };
	{ok, _} = CompleteResult ->
	    CompleteResult
    end;

handle_ws_json_rpc(WSock, <<"register_service">>, Params,_Arg ) ->
    { ok, SvcName } = rvi_common:get_json_element(["service_name"], Params),
    ?event({register_service, ws, SvcName}),
    ?debug("service_edge_rpc:websocket_register(~p) service:     ~p", [ WSock, SvcName ]),
    Opts = rvi_common:get_opt_json_element(<<"opts">>, [], Params),
    LogId = log_id_json_tail(Params),
    [ok, FullSvcName ] = gen_server:call(?SERVER,
					 { rvi,
					   register_service,
					   [ SvcName,
					     "ws:" ++ pid_to_list(WSock),
					     Opts | LogId]}),

    { ok, [ { status, rvi_common:json_rpc_status(ok)},
	    { service, FullSvcName },
	    { method, <<"register_service">>}]};

handle_ws_json_rpc(WSock, <<"unregister_service">>, Params, _Arg ) ->
    { ok, SvcName } = rvi_common:get_json_element(["service_name"], Params),
    ?event({unregister_service, ws, SvcName}),
    ?debug("service_edge_rpc:websocket_unregister(~p) service:    ~p", [ WSock, SvcName ]),
    gen_server:call(?SERVER, { rvi, unregister_service, [ SvcName ]}),
    { ok, [ { status, rvi_common:json_rpc_status(ok)} ]};

handle_ws_json_rpc(WSock, <<"get_node_service_prefix">>, Params, _Arg) ->
    ?debug("websocket_get_node_service_prefix(~p)", [WSock]),
    get_node_service_prefix_(Params);

handle_ws_json_rpc(_Ws , <<"get_available_services">>, _Params, _Arg ) ->
    ?debug("service_edge_rpc:websocket_get_available()"),
    [ ok, Services ] =
	gen_server:call(?SERVER, { rvi, get_available_services, []}),
    { ok, [ { status, rvi_common:json_rpc_status(ok)},
	    { services, Services},
	    { method, <<"get_available_services">>}] }.

%% parse_ws_params([{K, V}|T]) ->
%%     K1 = iolist_to_binary(K),
%%     V1 = iolist_to_binary(V),
%%     ?debug("K1 = ~p, V1 = ~p", [K1, V1]),
%%     [{K1, jsx:decode(iolist_to_binary(V1))}
%%      | parse_ws_params(T)];
%% parse_ws_params([]) ->
%%     [].

%% Invoked by locally connected services.
%% Will always be routed as JSON-RPC since that, and websocket,
%% are the only access paths in.
%%
handle_rpc(<<"register_service">>, Args) ->
    {ok, SvcName} = rvi_common:get_json_element([<<"service">>], Args),
    ?event({register_service, json_rpc, SvcName}),
    {ok, URL} = rvi_common:get_json_element([<<"network_address">>], Args),
    Opts = rvi_common:get_opt_json_element([<<"opts">>], [], Args),
    LogId = log_id_json_tail(Args),
    [ok, FullSvcName ] = gen_server:call(?SERVER,
					 { rvi, register_service,
					   [SvcName, URL, Opts | LogId] }),
    {ok, [ {status, rvi_common:json_rpc_status(ok) },
	   { service, FullSvcName },
	   { method, <<"register_service">>}
	 ]};


handle_rpc(<<"unregister_service">>, Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    ?event({unregister_service, json_rpc, SvcName}),
    LogId = log_id_json_tail(Args),
    gen_server:call(?SERVER, { rvi, unregister_service,
			       [SvcName | LogId]}),
    {ok, [ { status, rvi_common:json_rpc_status(ok) },
	   { method, <<"unregister_service">>}
	 ]};

handle_rpc(<<"get_node_service_prefix">>, Params) ->
    ?debug("get_node_service_prefix", []),
    get_node_service_prefix_(Params);

handle_rpc(<<"get_available_services">>, _Args) ->
    [ Status, Services ] = gen_server:call(?SERVER, { rvi, get_available_services, []}),
    ?debug("get_available_services(): ~p ~p", [ Status, Services ]),
    {ok, [ { status, rvi_common:json_rpc_status(ok)},
	   { services, Services},
	   { method, <<"get_available_services">>}
	 ]};

handle_rpc(<<"message">>, Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service_name"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, Parameters0} = rvi_common:get_json_element(["parameters"], Args),
    Files = rvi_common:get_opt_json_element(["rvi.files"], [], Args),
    ?debug("Files = ~p~n", [Files]),
    Parameters = append_files_to_params(Files, Parameters0),
    ?event({message, json_rpc, [SvcName, Timeout, Parameters]}),
    LogId = log_id_json_tail(Args ++ Parameters),
    case gen_server:call(
	   ?SERVER, { rvi, message,
		      [ SvcName, Timeout, Parameters | LogId]},
	   ?LONG_TIMEOUT) of
	[Res, TID] ->
	    ?debug("'message' result: ~p", [[Res, TID]]),
	    {ok, [ { status, rvi_common:json_rpc_status(Res) },
		   { transaction_id, TID },
		   { method, <<"message">>}
		 ]};
	[Res] ->
	    {ok, [ { status, rvi_common:json_rpc_status(Res) } ]};
	{ok, _} = CompleteResult ->
	    CompleteResult
    end;

handle_rpc(Other, _Args) ->
    ?warning("service_edge_rpc:handle_rpc(~p): unknown command", [ Other ]),
    {ok,[ { status, rvi_common:json_rpc_status(invalid_command)} ]}.


get_node_service_prefix_(Params) ->
    Prefix = rvi_common:local_service_prefix(),
    [UUID | _ ] = re:split(Prefix, <<"/">>, [{return, binary}]),
    GoodRes = fun(R) ->
		      { ok, [ { status, rvi_common:json_rpc_status(ok) },
			      { node_service_prefix, R },
			      { method, <<"get_node_service_prefix">> } ]}
	      end,
    case rvi_common:get_json_element(["full"], Params) of
	{ok, Full} when Full == true; Full == 1 ->
	    GoodRes(Prefix);
	{error, _} ->
	    GoodRes(Prefix);
	{ok, Full} when Full == false; Full == 0 ->
	    GoodRes(UUID);
	_ ->
	    { ok, [ { status, rvi_common:json_rpc_status(invalid_command) },
		    { method, <<"get_node_service_prefix">> } ] }
    end.

handle_notification(<<"service_available">>, Args) ->
    {ok, SvcName} = rvi_common:get_json_element([<<"service">>], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element([<<"data_link_module">>], Args),
    ?debug("service_edge:service_available(): service:   ~p", [ SvcName]),
    ?debug("service_edge:service_available(): data_link: ~p", [ DataLinkModule]),

    gen_server:cast(?SERVER, { rvi, service_available,
			       [ SvcName, DataLinkModule ]}),

    ok;
handle_notification(<<"service_unavailable">>, Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),
    ?debug("service_edge:service_unavailable(): service:   ~p", [ SvcName]),
    ?debug("service_edge:service_unavailable(): data_link: ~p", [ DataLinkModule]),

    gen_server:cast(?SERVER, { rvi, service_unavailable,
			       [ SvcName, DataLinkModule ]}),

    ok;

handle_notification(<<"handle_remote_message">>, Args) ->
    { ok, IP } = rvi_common:get_json_element(["ip"], Args),
    { ok, Port } = rvi_common:get_json_element(["port"], Args),
    { ok, SvcName } = rvi_common:get_json_element(["service"], Args),
    { ok, Timeout } = rvi_common:get_json_element(["timeout"], Args),
    { ok, Parameters } = rvi_common:get_json_element(["parameters"], Args),
    Files = rvi_common:get_opt_json_element(["rvi.files"], Args),
    gen_server:cast(?SERVER, { rvi, handle_remote_message,
			       [
				IP,
				Port,
				SvcName,
				Timeout,
				Parameters,
				Files
			       ]}),

    ok;




%% JSON-RPC entry point
%% Called by local exo http server
handle_notification(<<"handle_local_timeout">>, Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, TransactionID} = rvi_common:get_json_element(["transaction_id"], Args),
    gen_server:cast(?SERVER, { rvi, handle_local_timeout,
			       [ SvcName, TransactionID]}),

    ok;

handle_notification(Other, _Args) ->
    ?warning("service_edge_rpc:handle_notification(~p): unknown command", [ Other ]),
    ok.





%% Handle calls received through regular gen_server calls, routed byh
%% rvi_common:request() We only need to implement
%% register_remote_serviecs() and handle_remote_message Since they are
%% the only calls invoked by other components, and not the locally
%% connected services that uses the same HTTP port to transmit their
%% register_service, and message calls.
handle_call({ rvi, register_service, [SvcName, URL, Opts | T] },
	    _From, St) ->
    ?debug("service_edge_rpc:register_service(): service:   ~p ",   [SvcName]),
    ?debug("service_edge_rpc:register_service(): address:   ~p ",   [URL]),

    FullSvcName = rvi_common:local_service_to_string(SvcName),
    try register_local_service_(FullSvcName, URL, Opts, T, St)
    catch
	throw:Reason ->
	    {reply, [Reason], St}
    end;

handle_call({ rvi, unregister_service, [SvcName | T] }, _From, St) ->
    ?debug("service_edge_rpc:unregister_service(): service: ~p ", [SvcName]),


    ets:delete(?SERVICE_TABLE, SvcName),

    %% Register with service discovery, will trigger callback to service_available()
    %% that forwards the registration to other connected services.
    CS = start_log(T, "unreg local service: ~s", [SvcName], St#st.cs),
    service_discovery_rpc:unregister_services(CS, [SvcName], local),

    %% Return ok.
    { reply, [ ok ], St };



handle_call({rvi, get_available_services, []}, _From, St) ->
    ?debug("service_edge_rpc:get_available_services()"),
    {reply, service_discovery_rpc:get_all_services(St#st.cs), St};


%%CRASH13:43:57.370 [debug] service_edge_rpc:local_msg: parameters:      [{struct,"{"value":"3"}"}]

%%13:43:57.370 [debug] service_edge_rpc:local_msg: parameters:      [{struct,"{"value":"3"}"}]
%% [{struct,[{"a","b"}]}]
%% 13:48:12.943 [debug] service_edge_rpc:local_msg: parameters:      [{struct,[{"a","b"}]}]

handle_call({ rvi, message,
	      [SvcName, TimeoutArg, Parameters | Tail] = Args }, From,
	    #st{pending = Pend} = St) ->
    ?debug("local_msg: service_name:    ~p", [SvcName]),
    ?debug("local_msg: timeout:         ~p", [TimeoutArg]),
    ?debug("local_msg: parameters:      ~p", [Parameters]),
    CS = start_log(Tail, "local_message: ~s", [SvcName], St#st.cs),
    %%
    %% Authorize local message and retrieve a certificate / signature
    %% that will be accepted by the receiving node that will deliver
    %% the messaage to its locally connected service_name service.
    %%
    {Pid, Ref} =
	spawn_monitor(
	  fun() ->
		  exit({deferred_reply, handle_local_message_(Args, CS)})
	  end),
    {noreply, St#st{pending = [{Pid, Ref, From}|Pend]}};

handle_call(Other, _From, St) ->
    ?warning("service_edge_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, [ invalid_command ], St}.



handle_cast({rvi, service_available, [SvcName, _DataLinkModule] }, St) ->
    ?debug("service_edge_rpc: Service available: ~p", [ SvcName]),
    start_log(<<"svc">>, "service_available: ~s", [SvcName], St#st.cs),
    announce_service_availability(available, SvcName),
    { noreply, St };


handle_cast({rvi, service_unavailable, [SvcName, _DataLinkModule] }, St) ->
    ?debug("service_edge_rpc: Service unavailable: ~p:", [ SvcName]),
    start_log(<<"rsvc">>, "service_unavailable: ~s", [SvcName], St#st.cs),
    announce_service_availability(unavailable, SvcName),
    { noreply, St };



handle_cast({rvi, handle_remote_message,
	     [
	      IP,
	      Port,
	      SvcName,
	      Timeout,
	      Parameters
	     ] }, #st{cs = CS} = St) ->
    ?event({handle_remote_message, [IP, Port, SvcName, Timeout]}, St),
    case SvcName of
	<<"rvi:", _/binary>> ->
	    dispatch_reply(IP, Port, SvcName, Timeout, Parameters, CS);
	_ ->
	    spawn(fun() ->
			  handle_remote_message_(
			    IP, Port, SvcName, Timeout, Parameters, CS)
		  end)
    end,
    {noreply, St};

handle_cast({ rvi, handle_local_timeout, [SvcName, TransactionID] }, St) ->
    %% FIXME: Should be forwarded to service.
    ?info("service_edge_rpc:handle_local_timeout(): service: ~p trans_id: ~p ",
	  [SvcName, TransactionID]),

    { noreply, St};

handle_cast(Other, St) ->
    ?warning("service_edge_rpc:handle_cast(~p): unknown", [ Other ]),
    {noreply, St}.

handle_info({'DOWN', Ref, _, _, {deferred_reply, Deferred}},
	     #st{pending = Pend} = St) ->
    ?debug("got deferred reply: ~p", [Deferred]),
    case lists:keyfind(Ref, 2, Pend) of
	{_Pid, Ref, From} = P ->
	    Reply =
		case Deferred of
		    [ok, {_, <<"{\"jsonrpc\"", _/binary>> = JSON}] ->
			Decoded = jsx:decode(JSON),
			?debug("Decoded = ~p", [Decoded]),
			{_, R} = lists:keyfind(<<"result">>, 1, Decoded),
			?debug("R = ~p", [R]),
			{ok, [convert_status(X) || X <- R]};
		    [ok, [{_,_}|_] = ReplyL] ->
			case lists:keyfind(<<"result">>, 1, ReplyL) of
			    {_, R} ->
				?debug("R = ~p", [R]),
				{ok, [convert_status(X) || X <- R]};
			    false ->
				?debug("Cannot find result: ~p", [Deferred]),
				Deferred
			end;
		    [ok, I] when is_integer(I) ->
			Deferred;
		    Other ->
			?debug("Strange deferred_reply: ~p", [Other]),
			Other
		end,
	    ?debug("Reply = ~p", [Reply]),
	    gen_server:reply(From, Reply),
	    {noreply, St#st{pending = Pend -- [P]}};
	false ->
	    {noreply, St}
    end;
handle_info({'DOWN', Ref, _, _, Reason}, #st{pending = Pend} = St) ->
    case lists:keyfind(Ref, 2, Pend) of
	{Pid, _, From} = P ->
	    ?error("~p died: ~p", [Pid, Reason]),
	    gen_server:reply(From, [internal]),
	    {noreply, St#st{pending = Pend -- [P]}};
	false ->
	    ?debug("got DOWN, but no corresponding pending", []),
	    {noreply, St}
    end;
handle_info(_Info, St) ->
    {noreply, St}.

convert_status({<<"status">> = K, St}) ->
    {K, rvi_common:json_rpc_status(St)};
convert_status(X) ->
    X.


terminate(_Reason, _St) ->
    ok.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

dispatch_reply(_IP, _Port, <<"rvi:", _/binary>> = ReplyId, _Timeout, Params, _CS) ->
    case gproc:where({n, l, {rvi, rpc, ReplyId}}) of
	undefined ->
	    ?debug("No process matching ~p", [ReplyId]),
	    ignore;
	Pid ->
	    Pid ! {rvi, rpc_return, ReplyId, Params},
	    ok
    end.

handle_remote_message_(IP, Port, SvcName, Timeout, Parameters, CS) ->
    ?debug("service_edge:remote_msg(): remote_ip:       ~p", [IP]),
    ?debug("service_edge:remote_msg(): remote_port:     ~p", [Port]),
    ?debug("service_edge:remote_msg(): service_name:    ~p", [SvcName]),
    ?debug("service_edge:remote_msg(): timeout:         ~p", [Timeout]),
    ?debug("service_edge:remote_msg(): parameters:      ~p", [Parameters]),

    %% Check if this is a local message.
    case ets:lookup(?SERVICE_TABLE, SvcName) of
	[ #service_entry { url = URL }] -> %% This is a local message
	    case authorize_rpc:authorize_remote_message(
		   CS,
		   SvcName,
		   [{remote_ip, IP},
		    {remote_port, Port},
		    {service_name, SvcName},
		    {timeout, Timeout},
		    {parameters, Parameters}]) of
		[ ok ] ->
		    forward_remote_msg_to_local_service(
		      URL, SvcName, Parameters, Timeout, CS);
		[ _Other ] ->
		    ?warning("service_entry:remote_msg(): "
			     "Failed to authenticate ~p (~p)",
			     [SvcName, _Other])
	    end;
	[] ->
	    case service_discovery_rpc:is_service_available(CS, SvcName) of
		[ok, false] ->
		    %% log error and discard
		    ?debug("Remote service disappeared (~p)", [SvcName]),
		    [not_found];
		[ok, true] ->
		    ?debug(
		       "Forward remote message to schedule (~p)", [SvcName]),
		    schedule_rpc:schedule_message(CS,
						  SvcName,
						  Timeout,
						  Parameters)
	    end
    end.

handle_local_message_([SvcName, TimeoutArg, Parameters|_] = Args, CS) ->
    ?debug("CS = ~p", [lager:pr(CS, rvi_common)]),
    case authorize_rpc:authorize_local_message(
	   CS, SvcName, [{service_name, SvcName},
			 {timeout, TimeoutArg},
			 %% {parameters, Parameters},
			 {parameters,  Parameters}
			]) of
	[ok] ->
	    do_handle_local_message_(Args, CS);
	[not_found] ->
	    [not_found]
    end.

do_handle_local_message_([SvcName, TimeoutArg, Parameters0 | _Tail], CS) ->
    %%
    %% Slick but ugly.
    %% If the timeout is more than 24 hrs old when parsed as unix time,
    %% then we are looking at a relative msec timeout. Convert accordingly
    %%
    { Mega, Sec, _Micro } = os:timestamp(),
    Now = Mega * 1000000 + Sec,

    Timeout =
	case TimeoutArg - Now < -86400 of
	    true -> %% Relative timeout arg. Convert to unix time msec
		?debug("service_edge_rpc:local_msg(): Timeout ~p is relative.",
		       [TimeoutArg]),
		(Now * 1000) + TimeoutArg;

	    false -> %% Absolute timeout. Convert to unix time msec
		TimeoutArg * 1000
	end,
    {Synch, Tag, Parameters} = check_if_synch(Parameters0),
    ?debug("check_if_synch(~p) -> ~p", [Parameters0, {Synch, Tag, Parameters}]),
    %%
    %% Check if this is a local service by trying to resolve its service name.
    %% If successful, just forward it to its service_name.
    %%
    LookupRes = ets:lookup(?SERVICE_TABLE, SvcName),
    ?debug("Service LookupRes = ~p", [LookupRes]),
    case LookupRes of
	[ #service_entry { url = URL } = E ]  -> %% SvcName is local. Forward message
	    ?debug("service_edge_rpc:local_msg(): Service is local. Forwarding."),
	    log("dispatch to ~s", [URL], CS),
	    ?event({matching_service_entry, E}),
	    Res = forward_message_to_local_service(URL,
						   SvcName,
						   Parameters,
						   Synch, Tag,
						   Timeout,
						   CS),
	    TimeoutMS = timeout_ms(Timeout),
	    ?debug("TimeoutMS = ~p", [TimeoutMS]),
	    rpc_return(Synch, Tag, TimeoutMS, Res);

	_ -> %% SvcName is remote
	    %% Ask Schedule the request to resolve the network address
	    ?debug("service_edge_rpc:local_msg(): Service is remote. Scheduling."),
	    log("schedule message (~s)", [SvcName], CS),
	    [ _, TID ] = schedule_rpc:schedule_message(CS,
						       SvcName,
						       Timeout,
						       Parameters),
	    TimeoutMS = timeout_ms(Timeout),
	    ?debug("TimeoutMS = ~p", [TimeoutMS]),
	    rpc_return(Synch, Tag, TimeoutMS, [ok, TID ])
    end.

timeout_ms(UnixT) ->
    {M,S,U} = os:timestamp(),
    Now = M * 1000000000 + (S * 1000) + (U div 1000),
    UnixT - Now.

check_if_synch(Params) ->
    IsSynch = is_synch(Params),
    case IsSynch of
	{true, Level} ->
	    {ReplyId, Params1} = prepare_rpc_wait(Params, Level),
	    ?debug("prepare_rpc_wait(~p, ~p) -> ~p",
		   [Params, Level, {ReplyId, Params1}]),
	    {true, ReplyId, Params1};
	false ->
	    {false, none, Params}
    end.

is_synch(Params) ->
    case lists:keyfind(<<"rvi.synch">>, 1, Params) of
	{_, T} when T==true; T == <<"true">> ->
	    {true, 1};
	{_, <<"rvi:", _/binary>>} ->
	    {true, 1};
	false ->
	    case rvi_common:get_json_element(
		   ["parameters","rvi.synch"], Params) of
		{ok, T} when T==true; T == <<"true">> ->
		    {true, 2};
		{ok, <<"rvi:", _/binary>>} ->
		    {true, 2};
		_ ->
		    false
	    end
    end.

prepare_rpc_wait(Params, Level) ->
    NodeId = rvi_common:node_id(),
    Seq = integer_to_binary(erlang:unique_integer([positive, monotonic])),
    ReplyId = <<"rvi:", NodeId/binary, "/", Seq/binary>>,
    gproc:reg({n,l,{rvi, rpc, ReplyId}}),
    {ReplyId, replace_synch(Level, Params, {<<"rvi.synch">>, ReplyId})}.

replace_synch(1, Params, New) ->
    lists:keyreplace(<<"rvi.synch">>, 1, Params, New);
replace_synch(2, Params, New) ->
    {_, Ps} = lists:keyfind(<<"parameters">>, 1, Params),
    Ps1 = lists:keyreplace(<<"rvi.synch">>, 1, Ps, New),
    lists:keyreplace(<<"parameters">>, 1, Params, {<<"parameters">>, Ps1}).

rpc_return(false, _, _, Res) ->
    ?debug("rpc_return(false,_,_,_) -> ~p", [Res]),
    Res;
rpc_return(true, Tag, Timeout, _OrigRes) ->
    ?debug("rpc_return(true,~p,~p,_)", [Tag, Timeout]),
    receive
	{rvi, rpc_return, Tag, Reply} ->
	    ?debug("received matching reply (Tag = ~p)~n~p", [Tag, Reply]),
	    [ok, Reply];
	Other ->
	    ?debug("Received Other = ~p", [Other]),
	    [ok, internal]
    after Timeout ->
	    [timeout]
    end.

register_local_service_(FullSvcName, URL, Opts, T, St) ->
    SvcOpts = parse_svc_opts(Opts),
    CS = start_log(T, "reg local service: ~s", [FullSvcName], St#st.cs),
    ?debug("register_local_service(): full name: ~p ", [FullSvcName]),
    ets:insert(?SERVICE_TABLE, #service_entry {
				  service = FullSvcName,
				  opts = SvcOpts,
				  url = URL }),

    %% Register with service discovery, will trigger callback to service_available()
    %% that forwards the registration to other connected services.
    service_discovery_rpc:register_services(CS, [FullSvcName], local),
    %% Return ok.
    { reply, [ ok, FullSvcName ], St }.


json_rpc_notification(Method, Parameters) ->
    jsx:encode(
      [{<<"json-rpc">>, <<"2.0">>},
       {<<"method">>, Method},
       {<<"params">>, Parameters}
      ]).

dispatch_to_local_service("ws:" ++ WSPidStr, services_available,
			  [{<<"services">>, Services}] ) ->
    ?info("service_edge:dispatch_to_local_service(service_available, websock, ~p): ~p",
	  [ WSPidStr,  Services]),
    wse_server:send(list_to_pid(WSPidStr),
		    json_rpc_notification(<<"services_available">>,
					  [{<<"services">>, Services}])),
    %% No reply
    ok;

dispatch_to_local_service("ws:" ++ WSPidStr, services_unavailable,
			  [{<<"services">>, Services}] ) ->
    ?info("service_edge:dispatch_to_local_service(service_unavailable, websock, ~p): ~p",
	  [ WSPidStr, Services]),

    wse_server:send(list_to_pid(WSPidStr),
		    json_rpc_notification(<<"services_unavailable">>,
					  [{<<"services">>, Services}])),
    ok;

%% dispatch_to_local_service([ $w, $s, $: | WSPidStr], message,
%% 			  [{service_name, SvcName}, {parameters, [Args]}]) ->
%%     ?info("service_edge:dispatch_to_local_service(message, websock): ~p", [Args]),
%%     wse_server:send(list_to_pid(WSPidStr),
%% 	     json_rpc_notification("message",
%% 				   [{ "service_name", SvcName}, {parameters, Args}])),
%%     %% No response expected.
%%     ?debug("service_edge:dispatch_to_local_service(message, websock): Done"),
%%     ok;

dispatch_to_local_service("ws:" ++ WSPidStr, message,
			 [{ <<"service_name">>, SvcName},
			  { <<"parameters">>, Args}]) ->
    ?info("service_edge:dispatch_to_local_service(message/alt, websock): ~p", [Args]),
    wse_server:send(list_to_pid(WSPidStr),
	     json_rpc_notification(<<"message">>,
				   [{<<"service_name">>, SvcName},
				    {<<"parameters">>, Args}])),
    %% No response expected.
    ?debug("service_edge:dispatch_to_local_service(message, websock): Done"),
    ok;

dispatch_to_local_service("ws:" ++ _WSPidStr, message, Other) ->
    ?warning("service_edge:dispatch_to_local_service(message/alt, websock): UNKNOWN: ~p", [Other]),
    ok;

%% Dispatch to regular JSON-RPC over HTTP.
dispatch_to_local_service(URL, Command, Args) ->
    CmdStr = atom_to_binary(Command, latin1),
    ?debug("dispatch_to_local_service():  Command:         ~p",[ Command]),
    ?debug("dispatch_to_local_service():  Args:            ~p",[ Args]),
    ?debug("dispatch_to_local_service():  URL:             ~p",[ URL]),
    Res = rvi_common:send_json_request(URL, CmdStr, Args),
    ?debug("dispatch_to_local_service():  Result:          ~p",[ Res]),
    Res.


%% Forward a message to a specific locally connected service.
%% Called by forward_message_to_local_service/2.
%%
forward_remote_msg_to_local_service(URL,SvcName, Parameters, Timeout, CS) ->
    {Synch, Tag} =
	case rvi_common:get_json_element([<<"rvi.synch">>], Parameters) of
	    {ok, <<"rvi:", _/binary>> = ID} ->
		{true, ID};
	    _ ->
		{false, none}
	end,
    forward_message_to_local_service(
      URL, SvcName, Parameters, Synch, Tag, Timeout, CS).

forward_message_to_local_service(URL, SvcName, Parameters,
				 Synch, Tag, Timeout, CS) ->
    ?debug("forward_to_local(): URL:         ~p", [URL]),
    ?debug("forward_to_local(): Synch:       ~p", [Synch]),
    ?debug("forward_to_local(): Parameters:  ~p", [Parameters]),

    %%
    %% Strip our node prefix from service_name so that
    %% the service receiving the JSON rpc call will have
    %% a service_name that is identical to the service name
    %% it registered with.
    %%
    Pfx = rvi_common:local_service_prefix(),
    Sz = byte_size(Pfx)-1,
    <<_:Sz/binary, LocalSvcName0/binary>> = SvcName,
    LocalSvcName = normalize_slash(LocalSvcName0),
    ?debug("Service name: ~p (Pfx = ~p)", [LocalSvcName, Pfx]),
    %% Deliver the message to the local service, which can
    %% be either a wse websocket, or a regular HTTP JSON-RPC call
    Me = self(),
    spawn(
      fun() ->
	      try
		  log_outcome(
		    maybe_reply(
		      Synch, Me, Tag,
		      rvi_common:get_request_result(
			dispatch_to_local_service(
			  URL,
			  message,
			  [{<<"service_name">>, LocalSvcName },
			   {<<"parameters">>, Parameters }])), Timeout, CS),
		    SvcName, CS)
	      catch
		  Tag:Err ->
		      ?error("Caught ~p:~p~n~p",
			     [Tag,Err,erlang:get_stacktrace()])
	      end
      end),
    timer:sleep(500),
    [ ok, -1 ].

normalize_slash(Svc) ->
    {match, [Stripped]} = re:run(Svc, "/*(.*)", [{capture,[1],binary}]),
    <<"/", Stripped/binary>>.

maybe_reply(false, _, _, Res, _Timeout, _CS) ->
    Res;
maybe_reply(true, _, <<"rvi:", _/binary>> = Tag, Res, Timeout, CS) ->
    ?debug("maybe_reply() Tag: ~p", [Tag]),
    ?debug("maybe_reply() Res: ~p", [Res]),
    case decode_reply(Res) of
	error ->
	    ignore;  %% but should log ...
	Params ->
	    log("schedule reply (~p)", [Tag], CS),
	    schedule_rpc:schedule_message(CS, Tag, Timeout, Params)
    end,
    Res;
maybe_reply(true, Pid, Tag, Res, _Timeout, _CS) ->
    Pid ! {rvi, rpc_return, Tag, Res},
    Res.

decode_reply({_, Res}) when is_binary(Res) ->
    try jsx:decode(Res) of
	Decoded ->
	    {ok, Result} =
		rvi_common:get_json_element([<<"result">>], Decoded),
	    [{<<"result">>, Result}]
    catch
	error:_ ->
	    error
    end;
decode_reply({_, [{_,_}|_] = Res}) ->
    try rvi_common:get_json_element([<<"result">>], Res) of
	{ok, Result} ->
	    [{<<"result">>, Result}];
	_ ->
	    error
    catch
	error:_ ->
	    error
    end.

log_outcome({Status, _}, _SvcName, CS) ->
    log("result: ~w", [Status], CS);
log_outcome(Other, _SvcName, CS) ->
    log("unexpected: ~w", [Other], CS).



announce_service_availability(Available, SvcName) ->
    Cmd = case Available of
	      available -> services_available;
	      unavailable -> services_unavailable
	  end,

    %% See if we the service is already registered as a local
    %% service. If so, make sure that we don't send a service
    %% available to the URL tha originated the newly registered service.
    %%
    %% We also want to make sure that we don't send the notification
    %% to a local service more than once.
    %% We will build up a list of blocked URLs not to resend to
    %% as we go along
    BlockURLs = case ets:lookup(?SERVICE_TABLE, SvcName)  of
		   [ #service_entry { url = URL } ]  -> [URL];
		   [] -> []
	       end,
    ets:foldl(fun(Term, _Acc) ->
		      ?debug("~p: ~p~n", [ ?SERVICE_TABLE, Term]),
		      ok
	      end, ok, ?SERVICE_TABLE),
    ?debug("announce: service: ~p", [ SvcName]),
    ?debug("announce: Block:   ~p", [ BlockURLs]),

    ets:foldl(
      %% Notify if this is not the originating service.
      fun(#service_entry { url = URL }, Acc) ->
	      %% If the URL is not on the blackout
	      %% list, send a notification
	      case lists:member(URL, Acc) of
		  false ->
		      ?debug("DISPATCH: ~p: ~p", [ URL, Cmd]),
		      dispatch_to_local_service(URL, Cmd, [{<<"services">>,
							    [SvcName]}]),
		      %% Add the current URL to the blackout list
		      [URL | Acc];

		  %% URL is on blackout list
		  true ->
		      Acc
	      end
      end, BlockURLs, ?SERVICE_TABLE).

start_log(Info, Fmt, Args, CS) ->
    ?debug("start_log(~p,~p,~p,CS)", [Info,Fmt,Args]),
    case rvi_common:get_json_element([<<"rvi_log_id">>], Info) of
	{ok, ID} ->
	    start_log_(ID, Fmt, Args, CS);
	{error, _} ->
	    start_log(Fmt, Args, CS)
    end.

start_log(Fmt, Args, CS) ->
    start_log_(rvi_log:new_id(pfx()), Fmt, Args, CS).

start_log_(ID, Fmt, Args, CS) ->
    CS1 = rvi_common:set_value(rvi_log_id, ID, CS),
    log(Fmt, Args, CS1),
    CS1.

pfx() ->
    <<"svc_edge">>.

log(Fmt, Args, CS) ->
    rvi_log:flog(Fmt, Args, pfx(), CS).

log_id_json_tail(Args) ->
    ?debug("Args = ~p", [Args]),
    case rvi_common:get_json_element([<<"rvi_log_id">>], Args) of
	{ok, ID} ->
	    [{<<"rvi_log_id">>, ID}];
	{error, _} ->
	    []
    end.

event(_, _, _) ->
    ok.

append_files_to_params([], Parameters) ->
    Parameters;
append_files_to_params(Files, [T|_] = Parameters) when is_tuple(T) -> % object
    Parameters ++ [{<<"rvi.files">>, Files}];
append_files_to_params(Files, [_|_] = Parameters) ->  % array
    Parameters ++ [[{<<"rvi.files">>, Files}]].


parse_svc_opts(Opts) ->
    Files = rvi_common:get_opt_json_element(<<"files">>, <<"inline">>, Opts),
    [{<<"files">>, files_option(Files)}].

files_option(O = <<"inline">>)    -> O;
files_option(O = <<"reject">>)    -> O;
files_option(O = <<"multipart">>) -> O;
files_option(_) ->
    throw(invalid_command).
