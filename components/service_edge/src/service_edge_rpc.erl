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


-export([start_link/0]).

-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([handle_remote_message/6,
	 handle_local_timeout/3]).

-export([start_json_server/0, 
	 start_websocket/0]).

%% Invoked by service discovery
%% FIXME: Should be rvi_service_discovery behavior
-export([service_available/3,
	 service_unavailable/3]).



%%-include_lib("lhttpc/include/lhttpc.hrl").
-include_lib("lager/include/log.hrl").

-include_lib("rvi_common/include/rvi_common.hrl").

-define(SERVER, ?MODULE). 


-record(st, { 
	  %% Component specification
	  cs = #component_spec{}
	 }).

-define(SERVICE_TABLE, rvi_local_services).

-record(service_entry, {
	  service = "",       %% Servie handled by this entry.
	  url = undefined     %% URL where the service can be reached.
	 }).



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


start_json_server() ->
    ?debug("service_edge_rpc:start_json_server()"),
    case rvi_common:start_json_rpc_server(service_edge, 
					  ?MODULE, 
					  service_edge_sup) of
	ok ->
	    ok;

	Err -> 
	    ?warning("service_edge_rpc:start_json_server(): Failed to start: ~p", 
		    [Err]),
	    Err
    end.

	
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
    rvi_common:notification(service_edge, ?MODULE, 
			    service_available, 
			    [{ service, SvcName }, 
			     { data_link_module, DataLinkModule }], CompSpec).


service_unavailable(CompSpec, SvcName, DataLinkModule) ->
    rvi_common:notification(service_edge, ?MODULE, 
			    service_unavailable, 
			    [{ service, SvcName }, 
			     { data_link_module, DataLinkModule }], CompSpec).

handle_remote_message(CompSpec, Conn, SvcName, Timeout, Params, Signature) ->
    {IP, Port} = Conn,
    rvi_common:notification(service_edge, ?MODULE, 
			    handle_remote_message, 
			    [{ ip, IP },
			     { port, Port },
			     { service, SvcName }, 
			     { timeout, Timeout },
			     { parameters, Params },
			     { signature, Signature }], CompSpec).


%% Invoked by schedule_rpc.
%% A message originated from a locally connected service
%% has timed out
handle_local_timeout(CompSpec, SvcName, TransID) ->
    rvi_common:notification(service_edge, ?SERVER, handle_local_timeout, 
			    [ { service, SvcName}, 
			      { transaction_id, TransID} ], 
			    CompSpec).



handle_websocket(WSock, Mesg, Arg) ->
    { ok, Method } = rvi_common:get_json_element(["method"], Mesg),
    { ok, Params } = rvi_common:get_json_element(["params"], Mesg),
    { ok, ID } = rvi_common:get_json_element(["id"], Mesg),

    ?debug("service_edge_rpc:handle_websocket(~p/~p) method:      ~p", [ WSock, ID,Method ]),

    case handle_ws_json_rpc(WSock, Method, {array,Params}, Arg) of 
        ok -> ok;
	{ok, Reply} -> 
	    EncReply = binary_to_list(iolist_to_binary(exo_json:encode({struct, [ { id, ID} |Reply]}))),
	    ?debug("service_edge_rpc:handle_websocket(~p/~p) reply:      ~s", [ WSock, ID, EncReply]),
	    wse_server:send(WSock, list_to_binary(EncReply))
    end,
    ok.


%% Websocket interface
handle_ws_json_rpc(WSock, "message", Params, _Arg ) ->
    { ok, SvcName } = rvi_common:get_json_element(["service_name"], Params),
    { ok, Timeout } = rvi_common:get_json_element(["timeout"], Params),
    { ok, Parameters } = rvi_common:get_json_element(["parameters"], Params),

    ?debug("service_edge_rpc:handle_websocket(~p) params!:      ~p", [ WSock, Params ]),
    ?debug("service_edge_rpc:handle_websocket(~p) service:      ~p", [ WSock, SvcName ]),
    ?debug("service_edge_rpc:handle_websocket(~p) parameters:   ~p", [ WSock, Parameters ]),

    [ Res, TID ] = gen_server:call(?SERVER, { rvi, handle_local_message, 
					      [ SvcName, Timeout, [{struct, Parameters}]]}),

    ?debug("service_edge_rpc:wse_message(~p) Res:      ~p", [ WSock, Res ]),
    { ok, [ { status, rvi_common:json_rpc_status(Res) }, 
	    { transaction_id, TID} ] };

handle_ws_json_rpc(WSock, "register_service", Params,_Arg ) ->
    { ok, SvcName } = rvi_common:get_json_element(["service_name"], Params),
    ?debug("service_edge_rpc:websocket_register(~p) service:     ~p", [ WSock, SvcName ]),
    [ok, FullSvcName ] = gen_server:call(?SERVER, 
					 { rvi, 
					   register_local_service, 
					   [ SvcName, 
					     "ws:" ++ pid_to_list(WSock)]}),
    
    { ok, [ { status, rvi_common:json_rpc_status(ok)}, 
	    { service, FullSvcName }]};

handle_ws_json_rpc(WSock, "unregister_service", Params, _Arg ) ->
    { ok, SvcName } = rvi_common:get_json_element(["service_name"], Params),
    ?debug("service_edge_rpc:websocket_unregister(~p) service:    ~p", [ WSock, SvcName ]),
    gen_server:call(?SERVER, { rvi, unregister_local_service, [ SvcName ]}),
    { ok, [ { status, rvi_common:json_rpc_status(ok)} ]};

handle_ws_json_rpc(_Ws , "get_available_services", _Params, _Arg ) ->
    ?debug("service_edge_rpc:websocket_get_available()"),
    [ Services ] = gen_server:call(?SERVER, { rvi, get_available_services, []}),
    { ok, [ { status, rvi_common:json_rpc_status(ok)},
	    { services, Services}] }.


%% Invoked by locally connected services.
%% Will always be routed as JSON-RPC since that, and websocket,
%% are the only access paths in.
%%
handle_rpc("register_service", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, URL} = rvi_common:get_json_element(["network_address"], Args),
    [ok, FullSvcName ] = gen_server:call(?SERVER, 
					 { rvi, register_local_service, 
					   [ SvcName, URL]}),

    {ok, [ {status, rvi_common:json_rpc_status(ok) }, { service, FullSvcName }]};


handle_rpc("unregister_service", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    gen_server:call(?SERVER, { rvi, unregister_local_service, [ SvcName]}),
    {ok, [ { status, rvi_common:json_rpc_status(ok) }]};


handle_rpc("get_available_services", _Args) ->
    [ Status, Services ] = gen_server:call(?SERVER, { rvi, get_available_services, []}),
    ?debug("get_available_services(): ~p ~p", [ Status, Services ]),
    {ok, [ { status, rvi_common:json_rpc_status(ok)},
	   { services, {array, Services}} ]};


handle_rpc("message", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service_name"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, Parameters} = rvi_common:get_json_element(["parameters"], Args),
    [ Res, TID ] = gen_server:call(?SERVER, { rvi, handle_local_message, 
					      [ SvcName, Timeout, Parameters]}),

    {ok, [ { status, rvi_common:json_rpc_status(Res) },
	   { transaction_id, TID } ]};

handle_rpc(Other, _Args) ->
    ?warning("service_edge_rpc:handle_rpc(~p): unknown command", [ Other ]),
    {ok,[ { status, rvi_common:json_rpc_status(invalid_command)} ]}.


handle_notification("service_available", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),
    ?debug("service_edge:service_available(): service:   ~p", [ SvcName]),
    ?debug("service_edge:service_available(): data_link: ~p", [ DataLinkModule]),

    gen_server:cast(?SERVER, { rvi, service_available, 
			       [ SvcName, DataLinkModule ]}),

    ok;
handle_notification("service_unavailable", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),
    ?debug("service_edge:service_unavailable(): service:   ~p", [ SvcName]),
    ?debug("service_edge:service_unavailable(): data_link: ~p", [ DataLinkModule]),

    gen_server:cast(?SERVER, { rvi, service_unavailable, 
			       [ SvcName, DataLinkModule ]}),

    ok;

handle_notification("handle_remote_message", Args) ->
    { ok, IP } = rvi_common:get_json_element(["ip"], Args),
    { ok, Port } = rvi_common:get_json_element(["port"], Args),
    { ok, SvcName } = rvi_common:get_json_element(["service"], Args),
    { ok, Timeout } = rvi_common:get_json_element(["timeout"], Args),
    { ok, Parameters } = rvi_common:get_json_element(["parameters"], Args),
    { ok, Signature } = rvi_common:get_json_element(["signature"], Args),
    gen_server:cast(?SERVER, { rvi, handle_remote_message, 
			       [ 
				 IP,
				 Port,
				 SvcName, 
				 Timeout, 
				 Parameters,
				 Signature
			       ]}),

    ok;




%% JSON-RPC entry point
%% Called by local exo http server
handle_notification("handle_local_timeout", Args) ->
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
handle_call({ rvi, register_local_service, [SvcName, URL] }, _From, St) ->
    ?debug("service_edge_rpc:register_local_service(): service:   ~p ",   [SvcName]),
    ?debug("service_edge_rpc:register_local_service(): address:   ~p ",   [URL]),

    FullSvcName = rvi_common:local_service_to_string(SvcName),
    ?debug("service_edge_rpc:register_local_service(): full name: ~p ", [FullSvcName]),

    ets:insert(?SERVICE_TABLE, #service_entry {
				  service = FullSvcName,
				  url = URL }),

    %% Register with service discovery, will trigger callback to service_available()
    %% that forwards the registration to other connected services.
    service_discovery_rpc:register_services(St#st.cs, [FullSvcName], local),
    

    %% Return ok.
    { reply, [ ok, FullSvcName ], St };

handle_call({ rvi, unregister_local_service, [SvcName] }, _From, St) ->
    ?debug("service_edge_rpc:unregister_local_service(): service: ~p ", [SvcName]),


    ets:delete(?SERVICE_TABLE, SvcName),

    %% Register with service discovery, will trigger callback to service_available()
    %% that forwards the registration to other connected services.
    service_discovery_rpc:unregister_services(St#st.cs, [SvcName], local),

    %% Return ok.
    { reply, [ ok ], St };



handle_call({rvi, get_available_services, []}, _From, St) ->
    ?debug("service_edge_rpc:get_available_services()"),
    {reply, service_discovery_rpc:get_all_services(St#st.cs), St};


%%CRASH13:43:57.370 [debug] service_edge_rpc:local_msg: parameters:      [{struct,"{"value":"3"}"}]

%%13:43:57.370 [debug] service_edge_rpc:local_msg: parameters:      [{struct,"{"value":"3"}"}]
%% [{struct,[{"a","b"}]}]
%% 13:48:12.943 [debug] service_edge_rpc:local_msg: parameters:      [{struct,[{"a","b"}]}]

handle_call({ rvi, handle_local_message, 
	      [SvcName, TimeoutArg, Parameters] }, _From, St) ->
    ?debug("service_edge_rpc:local_msg: service_name:    ~p", [SvcName]),
    ?debug("service_edge_rpc:local_msg: timeout:         ~p", [TimeoutArg]),
    ?debug("service_edge_rpc:local_msg: parameters:      ~p", [Parameters]),
    %%
    %% Authorize local message and retrieve a certificate / signature
    %% that will be accepted by the receiving node that will deliver
    %% the messaage to its locally connected service_name service.
    %%
    [ok, Signature ] = 
	authorize_rpc:authorize_local_message(
	  St#st.cs, SvcName, [{service_name, SvcName},
			      {timeout, TimeoutArg},
			      {parameters, Parameters}]),

    %%
    %% Slick but ugly.
    %% If the timeout is more than 24 hrs old when parsed as unix time,
    %% then we are looking at a relative msec timeout. Convert accordingly
    %%
    { Mega, Sec, _Micro } = now(),
    Now = Mega * 1000000 + Sec,

    Timeout = 
	case TimeoutArg - Now < -86400 of
	    true -> %% Relative timeout arg. Convert to unix time msec
		?debug("service_edge_rpc:local_msg(): Timeout ~p is relative.", 
		       [TimeoutArg]),
		(Now * 1000) + TimeoutArg;

	    false -> %% Absolute timoeut. Convert to unix time msec
		TimeoutArg * 1000
	end,
    %%
    %% Check if this is a local service by trying to resolve its service name. 
    %% If successful, just forward it to its service_name.
    %% 
    case ets:lookup(?SERVICE_TABLE, SvcName)  of
	[ #service_entry { url = URL } ]  -> %% SvcName is local. Forward message
	    ?debug("service_edge_rpc:local_msg(): Service is local. Forwarding."),
	    Res = forward_message_to_local_service(URL, 
						   SvcName, 
						   Parameters,
						   St#st.cs),
	    { reply, Res , St};

	_ -> %% SvcName is remote
	    %% Ask Schedule the request to resolve the network address
	    ?debug("service_edge_rpc:local_msg(): Service is remote. Scheduling."),
	    [ _, TID ] = schedule_rpc:schedule_message(St#st.cs, 
						       SvcName, 
						       Timeout, 
						       Parameters,
						       Signature),
	    { reply, [ok, TID ], St}
    end;


handle_call(Other, _From, St) ->
    ?warning("service_edge_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, [ invalid_command ], St}.


		      
handle_cast({rvi, service_available, [SvcName, _DataLinkModule] }, St) ->
    ?debug("service_edge_rpc: Service available: ~p:", [ SvcName]),
    announce_service_availability(available, SvcName),
    { noreply, St };

		      
handle_cast({rvi, service_unavailable, [SvcName, _DataLinkModule] }, St) ->
    ?debug("service_edge_rpc: Service unavailable: ~p:", [ SvcName]),
    announce_service_availability(unavailable, SvcName),
    { noreply, St };


		      
handle_cast({rvi, handle_remote_message, 
	     [
	      IP,
	      Port,
	      SvcName,
	      Timeout,
	      Parameters,
	      Signature
	     ] }, St) ->

    ?debug("service_edge:remote_msg(): remote_ip:       ~p", [IP]),
    ?debug("service_edge:remote_msg(): remote_port:     ~p", [Port]),
    ?debug("service_edge:remote_msg(): service_name:    ~p", [SvcName]),
    ?debug("service_edge:remote_msg(): timeout:         ~p", [Timeout]),
    ?debug("service_edge:remote_msg(): parameters:      ~p", [Parameters]),
    ?debug("service_edge:remote_msg(): signature:       ~p", [Signature]),

    %% Check if this is a local message.
    case ets:lookup(?SERVICE_TABLE, SvcName) of
	[ #service_entry { url = URL }] -> %% This is a local message
	    case authorize_rpc:authorize_remote_message(
		   St#st.cs, 
		   SvcName, 
		   [{remote_ip, IP},
		    {remote_port, Port},
		    {service, SvcName},
		    {timeout, Timeout},
		    {parameters, Parameters},
		    {signature, Signature}]) of
		[ ok ] -> 
		    forward_message_to_local_service(URL, SvcName, 
						     Parameters, St#st.cs),
		    { noreply, St};

		[ _ ] ->
		    ?warning("service_entry:remote_msg(): Failed to authenticate ~p",
			     [SvcName]),
		    { noreply, St}
		end;
	[] ->
	    ?notice("service_entry:remote_msg(): Service Disappeared ~p",
		     [SvcName]),
	    { noreply, St}
	
    end;


handle_cast({ rvi, handle_local_timeout, [SvcName, TransactionID] }, St) ->
    %% FIXME: Should be forwarded to service.
    ?info("service_edge_rpc:handle_local_timeout(): service: ~p trans_id: ~p ", 
	  [SvcName, TransactionID]),
    
    { noreply, St};

handle_cast(Other, St) ->
    ?warning("service_edge_rpc:handle_cast(~p): unknown", [ Other ]),
    {noreply, St}.

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.




%%
%% Depending on the format of NetworkAddress
%% Dispatch to websocket or JSON-RPC server
%% FIXME: Should be a pluggable setup where 
%%        different dispatchers are triggered depending
%%        on prefix in NetworkAddress
%%

flatten_ws_args([{ struct, List} | T], Acc )  when is_list(List) ->
    flatten_ws_args( List ++ T, Acc);


flatten_ws_args([{ Key, Val}| T], Acc ) ->
    NKey = case is_atom(Key) of
	       true -> atom_to_list(Key);
	       false -> Key
	   end,

    NVal = flatten_ws_args(Val),

    flatten_ws_args(T, [ NKey, NVal] ++ Acc);

flatten_ws_args([], Acc) -> 
    Acc;
    

flatten_ws_args(Other, []) ->
    Other;

flatten_ws_args(Other, Acc) ->
    [ Other | Acc ].


flatten_ws_args(Args) ->    
    flatten_ws_args(Args, []).


json_rpc_notification(Method, Parameters) ->
      iolist_to_binary(
	exo_json:encode(
	  {struct, 
	   [ { "json-rpc", "2.0"},
	     { "method", Method },
	     { "params", {struct, Parameters}}
	   ]})).

dispatch_to_local_service([ $w, $s, $: | WSPidStr], services_available, 
			  {struct, [{ services, { array, Services}}]} ) ->
    ?info("service_edge:dispatch_to_local_service(service_available, websock, ~p): ~p", 
	  [ WSPidStr,  Services]),
    wse_server:send(list_to_pid(WSPidStr), 
		    json_rpc_notification("services_available",
					  [{"services", {array, Services}}])),
    %% No reply
    ok;

dispatch_to_local_service([ $w, $s, $: | WSPidStr], services_unavailable, 
			  {struct, [{ services, { array, Services}}]} ) ->
    ?info("service_edge:dispatch_to_local_service(service_unavailable, websock, ~p): ~p", 
	  [ WSPidStr, Services]),

    wse_server:send(list_to_pid(WSPidStr), 
	     json_rpc_notification("services_unavailable",
				   [{"services", {array, Services}}])),
    ok;

dispatch_to_local_service([ $w, $s, $: | WSPidStr], message, 
			 {struct, [{ service_name, SvcName}, { parameters, [ { struct, Args} ]}]} ) ->
    ?info("service_edge:dispatch_to_local_service(message, websock): ~p", [Args]),
    wse_server:send(list_to_pid(WSPidStr), 
	     json_rpc_notification("message",
				   [{ "service_name", SvcName}, {parameters, { struct, Args}}])),
    %% No response expected.
    ?debug("service_edge:dispatch_to_local_service(message, websock): Done"),
    ok;

dispatch_to_local_service([ $w, $s, $: | WSPidStr], message, 
			 {struct, [{ service_name, SvcName}, { parameters,{array,[{struct, Args}]}}]}) ->
    ?info("service_edge:dispatch_to_local_service(message/alt, websock): ~p", [Args]),
    wse_server:send(list_to_pid(WSPidStr), 
	     json_rpc_notification("message",
				   [{ "service_name", SvcName}, {parameters, { struct, Args}}])),
    %% No response expected.
    ?debug("service_edge:dispatch_to_local_service(message, websock): Done"),
    ok;

dispatch_to_local_service([ $w, $s, $: | _WSPidStr], message, Other) ->
    ?warning("service_edge:dispatch_to_local_service(message/alt, websock): UNKNOWN: ~p", [Other]),
    ok;

%% Dispatch to regular JSON-RPC over HTTP.
dispatch_to_local_service(URL, Command, Args) ->
    CmdStr = atom_to_list(Command),
    ?debug("dispatch_to_local_service():  Command:         ~p",[ Command]),
    ?debug("dispatch_to_local_service():  Args:            ~p",[ Args]),
    ?debug("dispatch_to_local_service():  URL:             ~p",[ URL]),
    Res = rvi_common:send_json_request(URL, CmdStr, Args),
    ?debug("dispatch_to_local_service():  Result:          ~p",[ Res]),
    Res.


%% Forward a message to a specific locally connected service.
%% Called by forward_message_to_local_service/2.
%%
forward_message_to_local_service(URL,SvcName, Parameters, _CompSpec) ->
    ?debug("service_edge:forward_to_local(): URL:         ~p", [URL]),
    ?debug("service_edge:forward_to_local(): Parameters:  ~p", [Parameters]),

    %%
    %% Strip our node prefix from service_name so that
    %% the service receiving the JSON rpc call will have
    %% a service_name that is identical to the service name
    %% it registered with.
    %%
    LocalSvcName = string:substr(SvcName, 
				 length(rvi_common:local_service_prefix())),

    %% Deliver the message to the local service, which can
    %% be either a wse websocket, or a regular HTTP JSON-RPC call
    spawn(fun() ->
		  rvi_common:get_request_result(
		    dispatch_to_local_service(URL, 
					      message, 
					      {struct, [ { service_name, LocalSvcName },
							 { parameters, Parameters }]}))
	  end),
    [ ok, -1 ].
    

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
	      ?debug("  URL: ~p - Acc : ~p ", [ URL, Acc]),
	      case lists:member(URL, Acc) of 
		  false ->
		      ?debug("DISPATCH: ~p: ~p", [ URL, Cmd]),
		      dispatch_to_local_service(URL, Cmd, 
						{struct, [ { services, 
							     { array, [SvcName]}
							   }
							 ]}),
		      %% Add the current URL to the blackout list
		      [URL | Acc]; 

		  %% URL is on blackout list
		  true -> 
		      Acc
	      end
      end, BlockURLs, ?SERVICE_TABLE).
