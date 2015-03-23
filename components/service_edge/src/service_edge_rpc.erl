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

-export([register_remote_services/3,
	 unregister_remote_services/3,
	 handle_remote_message/6]).

-export([start_json_server/0, 
	 start_websocket/0]).


%%-include_lib("lhttpc/include/lhttpc.hrl").
-include_lib("lager/include/log.hrl").

-include_lib("rvi_common/include/rvi_common.hrl").

-define(SERVER, ?MODULE). 


-record(st, { 
	  %% Component specification
	  cs = #component_spec{}
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


register_remote_services(CompSpec, Service, LocalServiceAddresses) ->
    rvi_common:request(service_edge, ?SERVER, register_remote_services, 
		       [ Service, LocalServiceAddresses ], 
		       [ service, local_service_addresses ],
		       [ status ],
		       CompSpec).


unregister_remote_services(CompSpec, Services, LocalServiceAddresses) ->
    rvi_common:request(service_edge, ?SERVER, unregister_remote_service, 
		       [ Services, LocalServiceAddresses ], 
		       [ services, local_service_addresses ],
		       [ status ],
		       CompSpec).


%%
%% Handle a message, delivered from a remote node through protocol, that is
%% to be forwarded to a locally connected service.
%%
handle_remote_message(CompSpec, ServiceName, Timeout, 
		      Parameters, Signature, Certificate) ->
    rvi_common:request(servide_edge, ?SERVER, handle_remote_message,
		       [ ServiceName, Timeout, Parameters, Signature, Certificate ],
		       [ service, timeout, parameters, signature, certificate ],
		       [ status ], 
		       CompSpec).






%% Announces the services listed in Services with all 
%% local services listed under LocalServices
%% SkipAddress is a single address that, if found in LocalServiceAddresses,
%% will not receive an announcement.
%% This is to avoid that a local service registering itself will get a callback
%% about its own availability.

announce_service_availability(Cmd, LocalServiceAddresses, Services) ->
    announce_service_availability(Cmd, LocalServiceAddresses, Services, undefined).

announce_service_availability(Cmd, LocalServiceAddresses, Services, SkipAddress) ->
    ?info("service_edge_rpc:announce_service_availability(~p, ~p, ~p): Called.", 
		  [ Cmd, LocalServiceAddresses, Services ]),
    
    lists:map(fun(LocalServiceAddress) when LocalServiceAddress =:= SkipAddress ->
		      ok;

		 (LocalServiceAddress) ->
		      dispatch_to_local_service(LocalServiceAddress, Cmd, 
						{struct, [ { services, { array, [Services ]}}]})
	      end, LocalServiceAddresses),
    { ok, [ { status, rvi_common:json_rpc_status(ok)} ] }.
    


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
dispatch_to_local_service(NetworkAddress, Command, Args) ->
    CmdStr = atom_to_list(Command),
    Res = rvi_common:send_http_request(NetworkAddress, CmdStr, Args),
    ?debug("dispatch_to_local_service():  Command:         ~p",[ CmdStr]),
    ?debug("dispatch_to_local_service():  Args:            ~p",[ Args]),
    ?debug("dispatch_to_local_service():  Result:          ~p",[ Res]),
    Res.


%% Forward a message to a specific locally connected service.
%% Called by forward_message_to_local_service/2.
%%
forward_message_to_local_service(ServiceName, NetworkAddress, Parameters, _CompSpec) ->
    ?debug("service_edge:forward_to_local(): URL:         ~p", [NetworkAddress]),
    ?debug("service_edge:forward_to_local(): Parameters:  ~p", [Parameters]),

    %%
    %% Strip our node prefix from service_name so that
    %% the service receiving the JSON rpc call will have
    %% a service_name that is identical to the service name
    %% it registered with.
    %%
    SvcName = string:substr(ServiceName, 
			    length(rvi_common:local_service_prefix())),

    %% Deliver the message to the local service, which can
    %% be either a wse websocket, or a regular HTTP JSON-RPC call
    case rvi_common:get_request_result(
	   dispatch_to_local_service(NetworkAddress, 
				     message, 
				     {struct, [ { service_name, SvcName },
						{ parameters, Parameters }]})) of

	%% Request delivered.
	{ ok, _Result } ->
	    ok;

	%% status returned was an error code.
	{ Other, _Result } ->
	    ?warning("service_edge:forward_to_local(): ~p:~p Failed: ~p.", 
		     [NetworkAddress, ServiceName, Other]),
	    Other;

	Other ->
	    ?warning("service_edge:forward_to_local(): ~p:~p Unknown error: ~p.", 
		     [NetworkAddress, ServiceName, Other]),
	    internal
    end.
    
%% A message is to targeting a service that is connected to the local RVI
%% node. We can just bounce the messsage straight over to the target service.
forward_message_to_local_service(ServiceName, Parameters, CompSpec) ->
    %%
    %% Resolve the local service name to an URL that we can send the
    %% request to
    %%
    ?debug("service_edge:forward_to_local(): service_name: ~p", [ServiceName]),

    case service_discovery_rpc:resolve_local_service(CompSpec, ServiceName) of
	[ ok, NetworkAddress]  -> 
	    forward_message_to_local_service(ServiceName, NetworkAddress, Parameters, CompSpec);

	%% Local service could not be resolved to an URL
	[ not_found ] ->
	    ?info("service_edge_rpc:forward_message_to_local() Not found: ~p", 
		   [ ServiceName ]),
	    not_found;
		    
	Err ->  
	    ?debug("service_edge_rpc:local_msg() Failed at service discovery: ~p", 
			      [ Err ]),
	    internal
    end.



%% JSON-RPC entry point
%% Called by local exo http server
handle_rpc("register_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),
    [ok, FullSvcName ] = gen_server:call(?SERVER, 
					 { rvi_call, register_local_service, 
					   [ Service, Address]}),

    {ok, [ {status, rvi_common:json_rpc_status(ok) }, { service, FullSvcName }]};


handle_rpc("unregister_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    gen_server:call(?SERVER, { rvi_call, unregister_local_service, [ Service]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok) }]};

handle_rpc("get_available_services", _Args) ->
    [ Status, Services ] = gen_server:call(?SERVER, { rvi_call, get_available_services, []}),
    ?debug("get_available_services(): ~p ~p", [ Status, Services ]),
    {ok, [ { status, rvi_common:json_rpc_status(ok)},
	   { services, {array, Services}} ]};



handle_rpc("register_remote_services", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, LocalServiceAddresses} = rvi_common:get_json_element(["local_service_addresses"], Args),

    gen_server:call(?SERVER, { rvi_call, register_remote_services, 
			       [ Services, LocalServiceAddresses]}),

    {ok, [ { status, rvi_common:json_rpc_status(ok)} ]};

handle_rpc("unregister_remote_services", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, LocalServiceAddresses} = rvi_common:get_json_element(["local_service_addresses"], Args),

    gen_server:call(?SERVER, { rvi_call, unregister_remote_services, 
			       [ Services, LocalServiceAddresses]}),

    {ok, [ { status, rvi_common:json_rpc_status(ok)} ]};

handle_rpc("message", Args) ->
    {ok, ServiceName} = rvi_common:get_json_element(["service_name"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, Parameters} = rvi_common:get_json_element(["parameters"], Args),
    [ Res ] = gen_server:call(?SERVER, { rvi_call, handle_local_message, 
					 [ ServiceName, Timeout, Parameters]}),
    {ok,[ { status, rvi_common:json_rpc_status(Res)} ]};

handle_rpc("handle_remote_message", Args) ->
    { ok, ServiceName } = rvi_common:get_json_element(["service_name"], Args),
    { ok, Timeout } = rvi_common:get_json_element(["timeout"], Args),
    { ok, Parameters } = rvi_common:get_json_element(["parameters"], Args),
    { ok, Certificate } = rvi_common:get_json_element(["certificate"], Args),
    { ok, Signature } = rvi_common:get_json_element(["signature"], Args),
    [ Res ] = gen_server:call(?SERVER, { rvi_call, handle_remote_message, 
					 [ 
					   ServiceName, 
					   Timeout, 
					   Parameters,
					   Certificate, 
					   Signature
					 ]}),

    {ok, [ { status, rvi_common:json_rpc_status(Res)} ]};


handle_rpc(Other, _Args) ->
    ?debug("service_edge_rpc:handle_rpc(~p): unknown command", [ Other ]),
    {ok,[ { status, rvi_common:json_rpc_status(invalid_command)} ]}.


%% Websocket iface 
wse_register_service(Ws, Service ) ->
    ?debug("service_edge_rpc:wse_register_service(~p) service:     ~p", [ Ws, Service ]),
    gen_server:call(?SERVER, { rvi_call, register_local_service, [ Service, "ws:" ++ pid_to_list(Ws)]}),
    { ok, [ { status, rvi_common:json_rpc_status(ok)} ]}.

wse_unregister_service(Ws, Service ) ->
    ?debug("service_edge_rpc:wse_unregister_service(~p) service:    ~p", [ Ws, Service ]),
    gen_server:call(?SERVER, { rvi_call, unregister_local_service, [ Service ]}),
    { ok, [ { status, rvi_common:json_rpc_status(ok)} ]}.


wse_get_available_services(_Ws ) ->
    ?debug("service_edge_rpc:wse_get_available_services()"),
    [ Services ] = gen_server:call(?SERVER, { rvi_call, get_available_services, []}),
    { ok, [ { status, rvi_common:json_rpc_status(ok)},
	    { services, Services}] }.


wse_message(Ws, ServiceName, Timeout, JSONParameters) ->
    %% Parameters are delivered as JSON. Decode into tuple
    { ok, Parameters } = exo_json:decode_string(JSONParameters),
    ?debug("service_edge_rpc:wse_message(~p) ServiceName:          ~p", [ Ws, ServiceName ]),
    ?debug("service_edge_rpc:wse_message(~p) Timeout:         ~p", [ Ws, Timeout]),
    ?debug("service_edge_rpc:wse_message(~p) Parameters:      ~p", [ Ws, Parameters ]),

    [ Res ] = gen_server:call(?SERVER, { rvi_call, handle_local_message, 
					 [ ServiceName, Timeout, Parameters]}),

    { ok, [ { status, rvi_common:json_rpc_status(Res) } ] }.

%% Deprecated
wse_message(Ws, ServiceName, Timeout, JSONParameters, _CallingService) ->
    wse_message(Ws, ServiceName, Timeout, JSONParameters).


%% Handle calls received through regular gen_server calls, routed byh
%% rvi_common:request() We only need to implement
%% register_remote_serviecs() and handle_remote_message Since they are
%% the only calls invoked by other components, and not the locally
%% connected services that uses the same HTTP port to transmit their
%% register_service, and message calls.
handle_call({ rvi_call, register_local_service, [Service, ServiceAddress] }, _From, St) ->
    ?debug("service_edge_rpc:register_local_service(): service: ~p ", [Service]),
    ?debug("service_edge_rpc:register_local_service(): address: ~p ", [ServiceAddress]),

    [ok, FullSvcName ] =
	service_discovery_rpc:register_local_service(St#st.cs, 
						     Service, 
						     ServiceAddress),
    
    SvcString = rvi_common:local_service_to_string(Service),

    %% Announce the new service to all connected nodes
    data_link_bert_rpc_rpc:
	announce_available_local_service(St#st.cs, SvcString),
    
    %% Retrieve addresses of all locally registered services.
    [ ok, AnnounceAddresses ] = 
	service_discovery_rpc:get_local_network_addresses(St#st.cs),
    
    announce_service_availability(services_available, AnnounceAddresses, 
				  [FullSvcName], ServiceAddress),


    %% Return ok.
    { reply, [ ok, FullSvcName ], St };

handle_call({ rvi_call, unregister_local_service, [Service] }, _From, St) ->
    ?debug("service_edge_rpc:unregister_local_service(): service: ~p ", [Service]),

    service_discovery_rpc:unregister_local_service(St#st.cs, Service),
    data_link_bert_rpc_rpc:

	announce_unavailable_local_service(St#st.cs, Service),

    [ ok, AnnounceAddresses ] = service_discovery_rpc:
	get_local_network_addresses(St#st.cs),
    %% Send out an announcement to all locally connected services, but skip
    %% the one that made the registration call
    announce_service_availability(services_unavailable, AnnounceAddresses, Service),

    %% Return ok.
    { reply, [ ok ], St };



handle_call({rvi_call, get_available_services, []}, _From, St) ->
    ?debug("service_edge_rpc:get_available_services()"),
    {reply, service_discovery_rpc:get_all_services(St#st.cs), St};


handle_call({ rvi_call, handle_local_message, 
	      [ServiceName, Timeout, Parameters] }, _From, St) ->
    ?debug("service_edge_rpc:local_msg: service_name:    ~p", [ServiceName]),
    ?debug("service_edge_rpc:local_msg: timeout:         ~p", [Timeout]),
    ?debug("service_edge_rpc:local_msg: parameters:      ~p", [Parameters]),

    %%
    %% Authorize local message and retrieve a certificate / signature
    %% that will be accepted by the receiving node that will deliver
    %% the messaage to its locally connected service_name service.
    %%
    [ok, Certificate, Signature ] = 
	authorize_rpc:authorize_local_message(St#st.cs, ServiceName),
    
    
    %%
    %% Check if this is a local service by trying to resolve its service name. 
    %% If successful, just forward it to its service_name.
    %% 
    Res = case service_discovery_rpc:resolve_local_service(St#st.cs, ServiceName) of
	[ ok, NetworkAddress]  -> %% ServiceName is local. Forward message
	    ?debug("service_edge_rpc:local_msg(): Service is local. Forwarding."),
	    [ forward_message_to_local_service(ServiceName, 
					       NetworkAddress, 
					       Parameters,
					       St#st.cs) ];

	_ -> %% ServiceName is remote
	    %% Ask Schedule the request to resolve the network address
	    ?debug("service_edge_rpc:local_msg(): Service is remote. Scheduling."),
		  schedule:schedule_message(ServiceName, 
					    Timeout, 
					    Parameters,
					    Certificate,
					    Signature)
    end,
    { reply, Res, St};

handle_call({rvi_call, register_remote_services, 
	     [ Services, LocalServiceAddresses ]}, _From, State) ->

    announce_service_availability(services_available, LocalServiceAddresses, Services),
    { reply, [ ok], State };

handle_call({rvi_call, unregister_remote_services, 
	     [Services, LocalServiceAddresses]}, _From, State) ->

    announce_service_availability(services_unavailable, LocalServiceAddresses, Services),
    { reply, [ ok ] , State };

handle_call({rvi_call, handle_remote_message, 
	     [
	      ServiceName,
	      Timeout,
	      Parameters,
	      Certificate,
	      Signature
	     ] }, _From, St) ->

    ?debug("service_edge:remote_msg(): service_name:    ~p", [ServiceName]),
    ?debug("service_edge:remote_msg(): timeout:         ~p", [Timeout]),
    ?debug("service_edge:remote_msg(): parameters:      ~p", [Parameters]),
    ?debug("service_edge:remote_msg(): signature:       ~p", [Signature]),
    ?debug("service_edge:remote_msg(): certificate:     ~p", [Certificate]),
    case 
	authorize_rpc:authorize_remote_message(St#st.cs, 
					       ServiceName, 
					       Certificate, 
					       Signature) of
	[ ok ] -> 
	    { reply, [ forward_message_to_local_service(ServiceName, Parameters, St#st.cs) ], St };


	%% Authorization failed.
	[ Err ] ->
	    ?warning("    service_edge:remote_msg(): Authorization failed:     ~p", [Err]),
	    {reply, [ authorization_failed ], St}
    end;

handle_call(Other, _From, St) ->
    ?warning("service_edge_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, [ invalid_command ], St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

