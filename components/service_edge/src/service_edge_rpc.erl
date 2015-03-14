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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([init_rvi_component/0]).


%%-include_lib("lhttpc/include/lhttpc.hrl").
-include_lib("lager/include/log.hrl").

-define(SERVER, ?MODULE). 
-record(st, { }).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?debug("service_edge_rpc:init(): called."),
    {ok, #st {}}.

%% Called by service_edge_app:start_phase().
init_rvi_component() ->
    ?notice("---- Service Edge URL:          ~s", [ rvi_common:get_component_url(service_edge)]),
    ?notice("---- Node Service Prefix:       ~s", [ rvi_common:local_service_prefix()]),
    case rvi_common:get_component_config(service_edge, exo_http_opts) of
	{ ok, ExoHttpOpts } ->
	    exoport_exo_http:instance(service_edge_sup, 
				      service_edge_rpc,
				      ExoHttpOpts);
	Err -> Err
    end,

    %%
    %% Fire up the websocket subsystem, if configured
    %%
    case rvi_common:get_component_config(service_edge, websocket, not_found) of
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

register_local_service(Service, ServiceAddress) ->
    ?debug("service_edge_rpc:register_local_service(): service: ~p ", [Service]),
    ?debug("service_edge_rpc:register_local_service(): address: ~p ", [ServiceAddress]),

    case 
	%% Register the service at service discovery
	rvi_common:send_component_request(service_discovery, register_local_service,
					  [
					   {service, Service}, 
					   {network_address, ServiceAddress}
					  ], [service]) of
	{ ok, JSONStatus, [ FullSvcName ]} -> 
	    
	    %% Announce the new service to all connected nodes.
	    rvi_common:send_component_request(data_link, announce_available_local_service,
					      [
					       %% Convert /some/svc to jlr.com/some/svc
					       {service, rvi_common:local_service_to_string(Service)}
					      ], [service]),	

	    %% Retrieve addresses of all locally registered services.
	    {ok, _JSONStatus, [ AnnounceAddresses ]} =
		rvi_common:send_component_request(service_discovery, get_local_network_addresses,
						    [], [addresses]),	
	    %% Send out an announcement to all locally connected services, but skip
	    %% the one that made the registration call
	    announce_service_availability(services_available, AnnounceAddresses, 
					  [FullSvcName], ServiceAddress),
	    

	    %% Return ok.
	    { ok, [ {service, FullSvcName}, 
		    {status, rvi_common:json_rpc_status(JSONStatus)} ] };
	
	Err -> 
	    ?debug("service_edge_rpc:register_local_service() Failed at service_discovery(): ~p", 
		      [ Err ]),
	    Err
    end.

unregister_local_service(Service) ->
    ?debug("service_edge_rpc:unregister_local_service(): service: ~p ", [Service]),

    case 
	%% Register the service at service discovery
	rvi_common:send_component_request(service_discovery, unregister_local_service,
					  [
					   {service, Service}
					  ]) of
	{ ok, ok } -> 
	    %% Announce the new service to all connected nodes.
	    rvi_common:send_component_request(data_link, announce_unavailable_local_service,
					      [
					       %% Convert /some/svc to jlr.com/some/svc
					       {service, Service}
					      ], [service]),	

	    {ok, ok, [ AnnounceAddresses ]} =
		rvi_common:send_component_request(service_discovery, 
						  get_local_network_addresses,
						  [], [addresses]),	
	    %% Send out an announcement to all locally connected services, but skip
	    %% the one that made the registration call
	    announce_service_availability(services_unavailable, AnnounceAddresses, Service),

	    %% Return ok.
	    { ok, [ {status, rvi_common:json_rpc_status(ok)} ] };
	
	Err -> 
	    ?debug("service_edge_rpc:unregister_local_service() Failed at service_discovery(): ~p", 
		      [ Err ]),
	    Err
    end.


get_available_services() ->
    ?debug("service_edge_rpc:get_available_services()"),

    case 
	%% Register the service at service discovery
	rvi_common:send_component_request(service_discovery, get_all_services,
					  [], [services]) of
	{ ok, _JSONStatus, [ Services ]} -> 
	    { ok, Services};

	Err -> 
	    ?debug("service_edge_rpc:get_all_services() Failed at service_discovery(): ~p", 
		      [ Err ]),
	    Err
    end.




announce_service_availability(Cmd, LocalServiceAddresses, Services) ->
    announce_service_availability(Cmd, LocalServiceAddresses, Services, undefined).


%% Announces the services listed in Services with all 
%% local services listed under LocalServices
%% SkipAddress is a single address that, if found in LocalServiceAddresses,
%% will not receive an announcement.
%% This is to avoid that a local service registering itself will get a callback
%% about its own availability.
announce_service_availability(Cmd, LocalServiceAddresses, Services, SkipAddress) ->
    ?info("service_edge_rpc:announce_service_availability(~p, ~p, ~p): Called.", 
		  [ Cmd, LocalServiceAddresses, Services ]),
    
    lists:map(fun(LocalServiceAddress) when LocalServiceAddress =:= SkipAddress ->
		      ok;

		 (LocalServiceAddress) ->
		      dispatch_to_local_service(LocalServiceAddress, Cmd, 
						[ { services, Services }])
	      end, LocalServiceAddresses),
    { ok, [ { status, rvi_common:json_rpc_status(ok)} ] }.
    
    

%%
%% Handle a message, delivered from a locally connected service, that is
%% to be forwarded to a remote service.
%%
handle_local_message(ServiceName, Timeout, Parameters) ->
    ?debug("service_edge_rpc:local_msg: service_name:    ~p", [ServiceName]),
    ?debug("service_edge_rpc:local_msg: timeout:         ~p", [Timeout]),
    ?debug("service_edge_rpc:local_msg: parameters:      ~p", [Parameters]),

    case 
	%%
	%% Authorize local message and retrieve a certificate / signature
	%% that will be accepted by the receiving node that will deliver
	%% the messaage to its locally connected service_name service.
	%%
	rvi_common:send_component_request(authorize, authorize_local_message,
					  [
					   {service_name, ServiceName}
					  ],
					  [ certificate, signature ]) of
	{ ok, ok, [Certificate, Signature] } -> 

	    %%
	    %% Check if this is a local service by trying to resolve its service name. 
	    %% If successful, just forward it to its service_name.
	    %% 
	    case rvi_common:send_component_request(service_discovery, resolve_local_service,
						   [
						    {service, ServiceName}
						   ], [ network_address ]) of
		{ ok, ok, [ NetworkAddress] } -> %% ServiceName is local. Forward message
		    ?debug("service_edge_rpc:local_msg(): Service is local. Forwarding."),
		    forward_message_to_local_service(ServiceName, NetworkAddress, Parameters);
		    
		_ -> %% ServiceName is remote
		    %% Ask Schedule the request to resolve the network address
		    ?debug("service_edge_rpc:local_msg(): Service is remote. Scheduling."),
		    forward_message_to_scheduler(ServiceName, Timeout, Parameters, Certificate, Signature)
	    end;

	Err -> 
	    ?warning("    service_edge_rpc:local_msg() Failed at authorize: ~p", 
		      [ Err ]),
	    Err
    end.


%%
%% Handle a message, delivered from a remote node through protocol, that is
%% to be forwarded to a locally connected service.
%%
handle_remote_message(ServiceName, Timeout, Parameters, Signature, Certificate) ->
    ?debug("service_edge:remote_msg(): service_name:    ~p", [ServiceName]),
    ?debug("service_edge:remote_msg(): timeout:         ~p", [Timeout]),
    ?debug("service_edge:remote_msg(): parameters:      ~p", [Parameters]),
    ?debug("service_edge:remote_msg(): signature:       ~p", [Signature]),
    ?debug("service_edge:remote_msg(): certificate:     ~p", [Certificate]),
    case 
	rvi_common:send_component_request(authorize, authorize_remote_message,
					  [
					   {service_name, ServiceName}, 
					   {certificate, Certificate},
					   {signature, Signature}
					  ]) of
	{ ok, ok } -> 
	    forward_message_to_local_service(ServiceName, Parameters);

	%% Authorization failed.
	{ ok, Err } ->
	    ?warning("    service_edge:remote_msg(): Authorization failed:     ~p", [Err]),
	    {error, { authorization, Err }};

	%% Authorization component error (HTTP, or similar).
	Err ->
	    ?warning("    service_edge:remote_msg(): Authorization failed:     ~p", [Err]),
	    Err
    end.

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
    ?debug("dispatch_to_local_service():  Result:          ~p",[ Res]).
    

%% Forward a message to a specific locally connected service.
%% Called by forward_message_to_local_service/2.
%%
forward_message_to_local_service(ServiceName, NetworkAddress, Parameters) ->
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
				     [ { service_name, SvcName },
				       { parameters, Parameters }])) of

	%% Request delivered.
	{ ok, ok, _ } ->
	    { ok, [ { status, rvi_common:json_rpc_status(ok)} ] };

	%% status returned was an error code.
	{ ok, undefined } ->
	    ?warning("service_edge:forward_to_local(): "
		     "Local Service ~p at ~p not available.", 
		     [ServiceName, NetworkAddress]),
	    { ok, [ { status, rvi_common:json_rpc_status(not_available)}]};

	{ ok, Status } ->
	    ?warning("    service_edge:forward_to_local(): Status:   ~p", 
		     [Status]),
	    { error, [{ status, rvi_common:json_rpc_status(Status)}]};

	%% HTTP or similar error.
	Err -> 
	    ?warning("service_edge:forward_to_local(): Local service failed: ~p", [Err]),
	    Err
    end.
    
%% A message is to targeting a service that is connected to the local RVI
%% node. We can just bounce the messsage straight over to the target service.
forward_message_to_local_service(ServiceName, Parameters) ->
    %%
    %% Resolve the local service name to an URL that we can send the
    %% request to
    %%
    ?debug("service_edge:forward_to_local(): service_name: ~p", [ServiceName]),

    case rvi_common:send_component_request(service_discovery, 
					   resolve_local_service,
					   [ {service, ServiceName} ],
					   [ network_address ]) of
	{ ok, ok, [ NetworkAddress] } -> 
	    forward_message_to_local_service(ServiceName, NetworkAddress, Parameters);

	%% Local service could not be resolved to an URL
	{ok, not_found, _} ->
	    ?info("    service_edge_rpc:local_msg() Local service ~p not found.", 
		   [ ServiceName ]),
	    { ok, [ { status, rvi_common:json_rpc_status(not_found)} ] };
		    
	Err ->  
	    ?debug("service_edge_rpc:local_msg() Failed at service discovery: ~p", 
			      [ Err ]),
	    { ok, [ { status, rvi_common:json_rpc_status(internal)} ] }
    end.


forward_message_to_scheduler(ServiceName, Timeout, Parameters, Certificate, Signature) ->
    %% Resolve the service_name.
    case 
	rvi_common:send_component_request(schedule, schedule_message,
					  [
					   { timeout, Timeout },
					   { parameters, Parameters }, 
					   { certificate, Certificate },
					   { signature, Signature },
					   { service_name, ServiceName }
					  ]) of

	{ ok,  ok } -> 
	    %% We are happy. Return.
	    { ok, [ { status, rvi_common:json_rpc_status(ok)} ] };

	Err -> 
	    ?debug("service_edge_rpc:local_msg() Failed at scheduling: ~p", 
		   [ Err ]),
	    { ok, [ { status, rvi_common:json_rpc_status(internal)} ] }
    end.
	


%% JSON-RPC entry point
%% Called by local exo http server
handle_rpc("register_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),
    register_local_service(Service, Address);

handle_rpc("unregister_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    unregister_local_service(Service);

handle_rpc("get_available_services", _Args) ->
    { ok, Services } = get_available_services(),
    { ok, [ { status, rvi_common:json_rpc_status(ok)},
	    { services, {array, Services}}] };


handle_rpc("register_remote_services", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, LocalServiceAddresses} = rvi_common:get_json_element(["local_service_addresses"], Args),
    announce_service_availability(services_available, LocalServiceAddresses, Services),
    { ok, [ { status, rvi_common:json_rpc_status(ok)} ] };

handle_rpc("unregister_remote_services", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, LocalServiceAddresses} = rvi_common:get_json_element(["local_service_addresses"], Args),
    announce_service_availability(services_unavailable, LocalServiceAddresses, Services),
    { ok, [ { status, rvi_common:json_rpc_status(ok)} ] };

handle_rpc("message", Args) ->
    {ok, ServiceName} = rvi_common:get_json_element(["service_name"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, Parameters} = rvi_common:get_json_element(["parameters"], Args),
    handle_local_message( ServiceName, Timeout, Parameters);

handle_rpc("handle_remote_message", Args) ->
    { ok, ServiceName } = rvi_common:get_json_element(["service_name"], Args),
    { ok, Timeout } = rvi_common:get_json_element(["timeout"], Args),
    { ok, Parameters } = rvi_common:get_json_element(["parameters"], Args),
    { ok, Certificate } = rvi_common:get_json_element(["certificate"], Args),
    { ok, Signature } = rvi_common:get_json_element(["signature"], Args),
    handle_remote_message( ServiceName, Timeout, Parameters, Certificate, Signature);


handle_rpc(Other, _Args) ->
    ?debug("service_edge_rpc:handle_rpc(~p): unknown command", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


%% Websocket iface 
wse_register_service(Ws, Service ) ->
    ?debug("service_edge_rpc:wse_register_service(~p) service:     ~p", [ Ws, Service ]),
    register_local_service(Service, "ws:" ++ pid_to_list(Ws)).

wse_unregister_service(Ws, Service ) ->
    ?debug("service_edge_rpc:wse_unregister_service(~p) service:    ~p", [ Ws, Service ]),
    unregister_local_service(Service).

wse_get_available_services(_Ws ) ->
    ?debug("service_edge_rpc:wse_get_available_services()"),
    { ok, Services }  = get_available_services(),
    { ok, [ { status, rvi_common:json_rpc_status(ok)},
	    { services, Services}] }.


wse_message(Ws, ServiceName, Timeout, JSONParameters) ->
    %% Parameters are delivered as JSON. Decode into tuple
    { ok, Parameters } = exo_json:decode_string(JSONParameters),
    ?debug("service_edge_rpc:wse_message(~p) ServiceName:          ~p", [ Ws, ServiceName ]),
    ?debug("service_edge_rpc:wse_message(~p) Timeout:         ~p", [ Ws, Timeout]),
    ?debug("service_edge_rpc:wse_message(~p) Parameters:      ~p", [ Ws, Parameters ]),
    handle_local_message( ServiceName, Timeout,  [Parameters]).

%% Deprecated
wse_message(Ws, ServiceName, Timeout, JSONParameters, _CallingService) ->
    wse_message(Ws, ServiceName, Timeout, JSONParameters).

    


%% Handle calls received through regular gen_server calls, routed byh
%% rvi_common:send_component_request()
%% We only need to implement register_remote_serviecs() and handle_remote_message
%% Since they are the only calls invoked by other components, and not the
%% locally connected services that uses the same HTTP port to transmit
%% their register_service, and message calls.

handle_call({rvi_call, register_remote_services, Args}, _From, State) ->
    {_, Services} = lists:keyfind(services, 1, Args),
    {_, LocalServiceAddresses} = lists:keyfind(local_service_addresses, 1, Args),
    announce_service_availability(services_available, LocalServiceAddresses, Services),
    { reply, { ok, [ { status, rvi_common:json_rpc_status(ok)} ]}, State };

handle_call({rvi_call, unregister_remote_services, Args}, _From, State) ->
    {_, Services} = lists:keyfind(services, 1, Args),
    {_, LocalServiceAddresses} = lists:keyfind(local_service_addresses, 1, Args),
    announce_service_availability(services_unavailable, LocalServiceAddresses, Services),
    { reply, {ok, [ { status, rvi_common:json_rpc_status(ok)} ] }, State };

handle_call({rvi_call, handle_remote_message, Args}, _From, State) ->
    { _, ServiceName } = lists:keyfind(service_name, 1, Args),
    { _, Timeout } = lists:keyfind(timeout, 1, Args),
    { _, Parameters } = lists:keyfind(parameters, 1, Args),
    { _, Certificate } = lists:keyfind(certificate, 1, Args),
    { _, Signature } = lists:keyfind(signature, 1, Args),
    
    {reply, handle_remote_message(ServiceName, 
				  Timeout, 
				  Parameters, 
				  Certificate, 
				  Signature), State };

handle_call(Other, _From, State) ->
    ?warning("service_edge_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ]}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

