%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(service_edge_rpc).
-behaviour(gen_server).

-export([handle_rpc/2]).
-export([handle_notification/2]).
-export([wse_register_service/2]).
-export([wse_unregister_service/2]).
-export([wse_get_available_services/1]).
-export([wse_message/5]).
-export([wse_message/4]).

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
		    wse_server:start(Port, proplists:delete(port, WSOpts)),
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

handle_remote_message(CompSpec, SvcName, Timeout, Parameters, Signature, Certificate) ->
    rvi_common:notification(service_edge, ?MODULE, 
			    handle_remote_message, 
			    [{ service, SvcName }, 
			     { timeout, Timeout },
			     { parameters, Parameters },
			     { signature, Signature },
			     { certificate, Certificate }], CompSpec).


%% Invoked by schedule_rpc.
%% A message originated from a locally connected service
%% has timed out
handle_local_timeout(CompSpec, SvcName, TransID) ->
    rvi_common:notification(service_edge, ?SERVER, handle_local_timeout, 
			    [ { service, SvcName}, 
			      { transaction_id, TransID} ], 
			    CompSpec).




%% Websocket interface
wse_register_service(Ws, SvcName ) ->
    ?debug("service_edge_rpc:wse_register_service(~p) service:     ~p", [ Ws, SvcName ]),
    gen_server:call(?SERVER, { rvi, register_local_service, [ SvcName, "ws:" ++ pid_to_list(Ws)]}),
    { ok, [ { status, rvi_common:json_rpc_status(ok)} ]}.

wse_unregister_service(Ws, SvcName ) ->
    ?debug("service_edge_rpc:wse_unregister_service(~p) service:    ~p", [ Ws, SvcName ]),
    gen_server:call(?SERVER, { rvi, unregister_local_service, [ SvcName ]}),
    { ok, [ { status, rvi_common:json_rpc_status(ok)} ]}.


wse_get_available_services(_Ws ) ->
    ?debug("service_edge_rpc:wse_get_available_services()"),
    [ Services ] = gen_server:call(?SERVER, { rvi, get_available_services, []}),
    { ok, [ { status, rvi_common:json_rpc_status(ok)},
	    { services, Services}] }.


wse_message(Ws, SvcName, Timeout, JSONParameters) ->
    %% Parameters are delivered as JSON. Decode into tuple
    { ok, Parameters } = exo_json:decode_string(JSONParameters),
    ?debug("service_edge_rpc:wse_message(~p) SvcName:          ~p", [ Ws, SvcName ]),
    ?debug("service_edge_rpc:wse_message(~p) Timeout:         ~p", [ Ws, Timeout]),
    ?debug("service_edge_rpc:wse_message(~p) Parameters:      ~p", [ Ws, Parameters ]),

    [ Res, TID ] = gen_server:call(?SERVER, { rvi, handle_local_message, 
					      [ SvcName, Timeout, Parameters]}),

    { ok, [ { status, rvi_common:json_rpc_status(Res) }, 
	    { transaction_id, TID} ] }.

%% Deprecated
wse_message(Ws, SvcName, Timeout, JSONParameters, _CallingService) ->
    wse_message(Ws, SvcName, Timeout, JSONParameters).



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
    { ok, SvcName } = rvi_common:get_json_element(["service"], Args),
    { ok, Timeout } = rvi_common:get_json_element(["timeout"], Args),
    { ok, Parameters } = rvi_common:get_json_element(["parameters"], Args),
    { ok, Certificate } = rvi_common:get_json_element(["certificate"], Args),
    { ok, Signature } = rvi_common:get_json_element(["signature"], Args),
    gen_server:cast(?SERVER, { rvi, handle_remote_message, 
			       [ 
				 SvcName, 
				 Timeout, 
				 Parameters,
				 Certificate, 
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
    ?debug("service_edge_rpc:register_local_service(): service: ~p ", [SvcName]),
    ?debug("service_edge_rpc:register_local_service(): address: ~p ", [URL]),

    FullSvcName = rvi_common:local_service_to_string(SvcName),

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

handle_call({ rvi, handle_local_message, 
	      [SvcName, Timeout, Parameters] }, _From, St) ->
    ?debug("service_edge_rpc:local_msg: service_name:    ~p", [SvcName]),
    ?debug("service_edge_rpc:local_msg: timeout:         ~p", [Timeout]),
    ?debug("service_edge_rpc:local_msg: parameters:      ~p", [Parameters]),

    %%
    %% Authorize local message and retrieve a certificate / signature
    %% that will be accepted by the receiving node that will deliver
    %% the messaage to its locally connected service_name service.
    %%
    [ok, Certificate, Signature ] = 
	authorize_rpc:authorize_local_message(St#st.cs, SvcName),
    
    
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
						       Certificate,
						       Signature),
	    { reply, [ok, TID ], St}
    end;


handle_call(Other, _From, St) ->
    ?warning("service_edge_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, [ invalid_command ], St}.


handle_cast({rvi, service_available, [SvcName, _DataLinkModule] }, St) ->
    announce_service_availability(available, SvcName),
    { noreply, St };

		      
handle_cast({rvi, service_unavailable, [SvcName, _DataLinkModule] }, St) ->
    announce_service_availability(unavailable, SvcName),
    { noreply, St };

		      
handle_cast({rvi, handle_remote_message, 
	     [
	      SvcName,
	      Timeout,
	      Parameters,
	      Certificate,
	      Signature
	     ] }, St) ->

    ?debug("service_edge:remote_msg(): service_name:    ~p", [SvcName]),
    ?debug("service_edge:remote_msg(): timeout:         ~p", [Timeout]),
    ?debug("service_edge:remote_msg(): parameters:      ~p", [Parameters]),
    ?debug("service_edge:remote_msg(): signature:       ~p", [Signature]),
    ?debug("service_edge:remote_msg(): certificate:     ~p", [Certificate]),

    %% Check if this is a local message.
    case ets:lookup(?SERVICE_TABLE, SvcName) of
	[ #service_entry { url = URL }] -> %% This is a local message
	    case authorize_rpc:authorize_remote_message(St#st.cs, 
							SvcName, 
							Certificate, 
							Signature) of
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


dispatch_to_local_service([ $w, $s, $: | WSPidStr], services_available, 
			  [{ services, Services}] ) ->
    ?info("service_edge:dispatch_to_local_service(service_available, websock): ~p", [ Services]),
    wse:call(list_to_pid(WSPidStr), wse:window(),
	     "services_available", 
	     [ "services", Services ]),
    ok;

dispatch_to_local_service([ $w, $s, $: | WSPidStr], services_unavailable, 
			  [{ services, Services}] ) ->
    ?info("service_edge:dispatch_to_local_service(service_unavailable, websock): ~p", [ Services]),
    wse:call(list_to_pid(WSPidStr), wse:window(),
	     "services_unavailable", 
	     [ "services", Services ]),
    ok;

dispatch_to_local_service([ $w, $s, $: | WSPidStr], message, 
			 [{ service_name, SvcName}, { parameters, Args}] ) ->
    ?info("service_edge:dispatch_to_local_service(message, websock): ~p", [Args]),
    wse:call(list_to_pid(WSPidStr), wse:window(),
	     "message", 
	     [ "service_name", SvcName ] ++ flatten_ws_args(Args)),
    ok;

%% Dispatch to regular JSON-RPC over HTTP.
dispatch_to_local_service(URL, Command, Args) ->
    CmdStr = atom_to_list(Command),
    Res = rvi_common:send_json_request(URL, CmdStr, Args),
    ?debug("dispatch_to_local_service():  Command:         ~p",[ CmdStr]),
    ?debug("dispatch_to_local_service():  Args:            ~p",[ Args]),
    ?debug("dispatch_to_local_service():  URL:             ~p",[ URL]),
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
    case rvi_common:get_request_result(
	   dispatch_to_local_service(URL, 
				     message, 
				     {struct, [ { service_name, LocalSvcName },
						{ parameters, Parameters }]})) of

	%% Request delivered.
	%% -1 is transaction ID.
	{ ok, _Result } ->
	    [ ok, -1 ];

	%% status returned was an error code.
	{ Other, _Result } ->
	    ?warning("service_edge:forward_to_local(): ~p:~p Failed: ~p.", 
		     [URL, SvcName, Other]),
	    [not_found, -1];

	Other ->
	    ?warning("service_edge:forward_to_local(): ~p:~p Unknown error: ~p.", 
		     [URL, SvcName, Other]),
	    [internal, -1]
    end.
    

announce_service_availability(Available, SvcName) ->
    Cmd = case Available of
	      available -> services_available;
	      unavailable -> services_unavailable
	  end,

    ets:foldl(
      %% Notify if this is not the originating service.
      fun(#service_entry { 
	     service = ServiceEntry,
	     url = URL }, _Acc) when 
		ServiceEntry =/= SvcName ->

	      dispatch_to_local_service(URL, Cmd, 
					{struct, [ { services, 
						     { array, [SvcName]}
						   }
						 ]}),
	      [];

	 %% This is the originating service regsitering itself. Ignore.
	 (_, _) -> []

      end, [], ?SERVICE_TABLE).
