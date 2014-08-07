%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(service_edge_rpc).


-export([handle_rpc/2]).
-export([init/0]).

%%-include_lib("lhttpc/include/lhttpc.hrl").
-include_lib("lager/include/log.hrl").

%% Called by service_edge_app:start_phase().
init() ->
    case rvi_common:get_component_config(service_edge, exo_http_opts) of
	{ ok, ExoHttpOpts } ->
	    exoport_exo_http:instance(service_edge_sup, 
				      service_edge_rpc,
				      ExoHttpOpts);
	Err -> Err
    end.


register_service(Service, Address) ->
    ?debug("service_edge_rpc:register_service(): service: ~p ", [Service]),
    ?debug("service_edge_rpc:register_service(): address: ~p ", [Address]),

    case 
	rvi_common:send_component_request(service_discovery, register_local_service,
					  [
					   {service, Service}, 
					   {network_address, Address}
					  ], [service]) of
	{ ok, JSONStatus, [ FullSvcName], _JSON} -> 
	    { ok, [ {service, FullSvcName}, 
		    {status, rvi_common:json_rpc_status(JSONStatus)} ] };

	Err -> 
	    ?debug("service_edge_rpc:register_service() Failed at service_discovery(): ~p", 
		      [ Err ]),
	    Err
    end.

%%
%% Handle a message, delivered from a locally connected service, that is
%% to be forwarded to a remote service.
%%
handle_local_message(Target, Timeout, Parameters, CallingService) ->
    ?debug("service_edge_rpc:call(): target:          ~p", [Target]),
    ?debug("service_edge_rpc:call(): timeout:         ~p", [Timeout]),
    ?debug("service_edge_rpc:call(): parameters:      ~p", [Parameters]),
    ?debug("service_edge_rpc:call(): calling_service: ~p", [CallingService]),

    case 
	rvi_common:send_component_request(authorize, authorize_local_message,
					  [
					   {calling_service, CallingService}, 
					   {target, Target}
					  ],
					  [ certificate, signature ]) of
	{ ok, ok, [Certificate, Signature], _AuthJSON } -> 
	    case 
		rvi_common:send_component_request(service_discovery, 
						  resolve_service,
						  [ {service, Target} ],
						  [ network_address ]) of
		{ ok, ok, [ NetworkAddress], _SDJSON } -> 
		    
		    %% Remember this transaction ID for the reply
		    %% so that we know where to send the reply.

		    %% Ask Schedule the request to resolve the network address
		    case 
			rvi_common:send_component_request(schedule, schedule_message,
							  [
							   { timeout, Timeout },
							   { parameters, Parameters }, 
							   { network_address, NetworkAddress }, 
							   { certificate, Certificate },
							   { signature, Signature },
							   { target, Target }
							  ]) of

			{ ok,  ok, _SchJSON } -> 
			    %% We are happy. Return.
			    { ok, [ { status, rvi_common:json_rpc_status(ok)} ] };

			Err -> 
			    ?debug("service_edge_rpc:handle_local_message() Failed at scheduling: ~p", 
				      [ Err ]),
			    { ok, [ { status, rvi_common:json_rpc_status(internal)} ] }
		    end;

		{ok, not_found, _, _} ->
		    ?debug("service_edge_rpc:handle_local_message() Service ~p not found", 
			   [ Target ]),
		    { ok, [ { status, rvi_common:json_rpc_status(not_found)} ] };
		    
		Err ->  
		    ?debug("service_edge_rpc:handle_local_message() Failed at service discovery: ~p", 
			      [ Err ]),
		    { ok, [ { status, rvi_common:json_rpc_status(internal)} ] }
	    end;

	Err -> 
	    ?debug("service_edge_rpc:handle_local_message() Failed at authorize: ~p", 
		      [ Err ]),
	    Err
    end.


%%
%% Handle a message, delivered from a remote node through protocol, that is
%% to be forwarded to a locally connected service.
%%
handle_remote_message(Target, Timeout, Parameters, Signature, Certificate) ->
    ?debug("service_edge:remote_msg(): target:          ~p", [Target]),
    ?debug("service_edge:remote_msg(): timeout:         ~p", [Timeout]),
    ?debug("service_edge:remote_msg(): parameters:      ~p", [Parameters]),
    ?debug("service_edge:remote_msg(): signature:       ~p", [Signature]),
    ?debug("service_edge:remote_msg(): certificate:     ~p", [Certificate]),

    case 
	rvi_common:send_component_request(authorize, authorize_remote_message,
					  [
					   {target, Target}, 
					   {certificate, Certificate},
					   {signature, Signature}
					  ]) of
	{ ok, ok, _AuthJSON } -> 
	    case 
		rvi_common:send_component_request(service_discovery, 
						  resolve_service,
						  [ { service, Target } ],
						  [ network_address ]) of
		{ ok, ok, [ NetworkAddress], _SDJSON } -> 
		    SvcName = string:substr(Target, 
					    length(rvi_common:local_service_prefix())),
		    case 
			%% Strip our node prefix from target so that
			%% the service receiving the JSON rpc call will have
			%% a target that is identical to the service name
			%% it registered with.

			%% Deliver the message to the local service
			rvi_common:get_request_result(
			  rvi_common:send_http_request(NetworkAddress, 
						        "message", 
						       [ { target, SvcName },
							 { parameters, Parameters }])) of

			%% Request delivered.
			{ ok, ok, _ } ->
			    { ok, [ { status, rvi_common:json_rpc_status(ok)} ] };

			%% Request delivered (with no status reply)
			{ok,  undefined  } ->
			    { ok, [ { status, rvi_common:json_rpc_status(ok)} ] };

			%% status returned was an error code.
			{ ok, Status, _ } ->
			    { error, { json_rpc, rvi_common:json_rpc_status(Status)}};

			%% HTTP or similar error.
			Err ->
			    Err
		    end;

		%% service discovery failed.
		_ -> { error, { unknown_target, Target}}
	    end;

	%% Authorization failed.
	{ ok, Err, _} ->
	    {error, { authorization, Err }};

	%% Authorization component error (HTTP, or similar).
	Err -> Err
    end.


%% JSON-RPC entry point
%% Called by local exo http server
handle_rpc("register_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),
    register_service(Service, Address);


handle_rpc("message", Args) ->
    {ok, Target} = rvi_common:get_json_element(["target"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, Parameters} = rvi_common:get_json_element(["parameters"], Args),
    {ok, CallingService} = rvi_common:get_json_element(["calling_service"], Args),
    handle_local_message( Target, Timeout, Parameters, CallingService);

handle_rpc("handle_remote_message", Args) ->
    { ok,  Target } = rvi_common:get_json_element(["target"], Args),
    { ok, Timeout } = rvi_common:get_json_element(["timeout"], Args),
    { ok, Parameters } = rvi_common:get_json_element(["parameters"], Args),
    { ok, Certificate } = rvi_common:get_json_element(["certificate"], Args),
    { ok, Signature } = rvi_common:get_json_element(["signature"], Args),
    handle_remote_message( Target, Timeout, Parameters, Certificate, Signature);



handle_rpc(Other, _Args) ->
    ?debug("service_edge_rpc:handle_rpc(~p): unknown command", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.

