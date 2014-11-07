%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(service_discovery_rpc).

-export([handle_rpc/2]).
-export([init/0]).

-include_lib("lager/include/log.hrl").
-define(LOCAL_SERVICE_TABLE, rvi_local_services).
-define(REMOTE_SERVICE_TABLE, rvi_remote_services).
-define(REMOTE_ADDRESS_TABLE, rvi_address_services).


-record(service_entry, {
	  service = [],
	  network_address = undefined %% Address where service can be found
	 }).

%% Called by service_discovery_app:start_phase().
init() ->
    ets:new(?LOCAL_SERVICE_TABLE, [set,  public, named_table, { keypos, #service_entry.service }]),
    ets:new(?REMOTE_SERVICE_TABLE, [set,  public, named_table, 
				    {keypos, #service_entry.service}]),
    ets:new(?REMOTE_ADDRESS_TABLE, [duplicate_bag,  public, named_table, 
				    {keypos, #service_entry.network_address}]),

    case rvi_common:get_component_config(service_discovery, exo_http_opts) of
	{ ok, ExoHttpOpts } ->
	    exoport_exo_http:instance(service_discovery_sup, 
				      service_discovery_rpc,
				      ExoHttpOpts);
	Err -> Err
    end,
    ok.

    


register_remote_service(NetworkAddress) ->
    ?info("service_discovery_rpc:register_remote_service(): service:         empty"),
    ?info("service_discovery_rpc:register_remote_service(): network_address: ~p", [NetworkAddress]),

    ets:insert(?REMOTE_ADDRESS_TABLE, 
	       #service_entry {
		  service = "",
		  network_address = NetworkAddress
		 }),

    {ok, [ {service, ""}, { status, rvi_common:json_rpc_status(ok)}]}.


register_remote_service(Service, NetworkAddress) ->
    ?info("service_discovery_rpc:register_remote_service(): service:         ~p", [Service]),
    ?info("service_discovery_rpc:register_remote_service(): network_address: ~p", [NetworkAddress]),

    FullSvcName = rvi_common:remote_service_to_string(Service),

    ets:insert(?REMOTE_SERVICE_TABLE, 
	       #service_entry {
		  service = FullSvcName,
		  network_address = NetworkAddress
		 }),

    %% Delete any addresses stored with an empty service name,
    %% installed with register_remote_service/1, since we now have at
    %% least one service name.
    ets:match_delete(?REMOTE_ADDRESS_TABLE, 
		     #service_entry { 
			service = [], 
			network_address = NetworkAddress 
		      }),
    ets:insert(?REMOTE_ADDRESS_TABLE, 
	       #service_entry {
		  service = FullSvcName,
		  network_address = NetworkAddress
		 }),

    {ok, [ {service, FullSvcName}, { status, rvi_common:json_rpc_status(ok)}]}.


unregister_remote_service(NetworkAddress) ->
    ?info("service_discovery_rpc:unregister_remote_service(): network_address: ~p", 
	  [NetworkAddress]),

    %% Delete all services registered under the given address.
    Svcs = ets:lookup(?REMOTE_ADDRESS_TABLE, NetworkAddress),

    ?info("service_discovery_rpc:unregister_remote_service(): ~p -> ~p", 
	  [NetworkAddress, Svcs]),
    

    %% We now have a bunch of service records, convert them to a list of service
    %% names and send them of to schedule for deregistration
    SvcNames = lists:foldr(fun(#service_entry {  service = SvcName }, Acc) -> 
				       [SvcName | Acc]
			       end, [], Svcs),

    rvi_common:send_component_request(schedule, unregister_remote_services, 
				      [
				       { services, SvcNames }
				      ]),

    [ ets:delete(?REMOTE_SERVICE_TABLE, Svc#service_entry.service) || Svc <- Svcs ],
    ets:delete(?REMOTE_ADDRESS_TABLE, NetworkAddress),

    {ok, [ { status, rvi_common:json_rpc_status(ok)}]}.


register_local_service(Service, NetworkAddress) ->
    ?info("service_discovery_rpc:register_local_service(): service:         ~p", [Service]),
    ?info("service_discovery_rpc:register_local_service(): network_address: ~p", [NetworkAddress]),

    FullSvcName = rvi_common:local_service_to_string(Service),
    ets:insert(?LOCAL_SERVICE_TABLE, 
	       #service_entry {
		  service = FullSvcName,
		  network_address = NetworkAddress
		 }),

    {ok, [ { service, FullSvcName }, { status, rvi_common:json_rpc_status(ok) }]}.


resolve_local_service(RawService) ->
    ?info("service_discovery_rpc:resolve_local_service(): RawService:  ~p", [RawService]),
    resolve_service(?LOCAL_SERVICE_TABLE, RawService).


resolve_remote_service(RawService) ->
    ?info("service_discovery_rpc:resolve_remote_service(): RawService: ~p", [RawService]),
    resolve_service(?REMOTE_SERVICE_TABLE, RawService).


resolve_service(Table, RawService) ->
    Service = rvi_common:sanitize_service_string(RawService),

    ?info("service_discovery_rpc:resolve_service(): CleanedService:    ~p", [RawService]),

    %% For info purposes only
    Svcs = ets:foldl(fun({service_entry, ServiceName, ServiceAddr}, Acc) -> 
			     [  {ServiceName, ServiceAddr}  | Acc ] end, 
			 [], Table),
    ?info("service_discovery_rpc:resolve_service(): Services:          ~p", [Svcs]),

    
    case ets:lookup(Table, Service) of
	%% We found a service entry, report it back
	[#service_entry { network_address = NetworkAddress }] ->
	    ?info("service_discovery_rpc:resolve_service(): service: ~p -> ~p", 
		   [ Service, NetworkAddress ]),

	    {ok, [ { status, rvi_common:json_rpc_status(ok) },
		   { network_address, NetworkAddress }]};

	%% We did not find a service entry, check statically configured nodes.
	[] -> 
	    ?info("service_discovery_rpc:resolve_service(~p): Service not found in ets. "
		  "Trying static nodes",
		     [Service]),

	    
	    %% Check if this is a service residing on the backend server
	    case rvi_common:find_static_node(Service) of
		not_found -> %% Not found
		    ?info("service_discovery_rpc:resolve_service(~p): Service not found in static nodes", 
			   [Service]),
		    
		    { ok, [ { status, rvi_common:json_rpc_status(not_found) }]};

		NetworkAddress -> %% Found
			    ?info("service_discovery_rpc:resolve_service(~p): Service is on static node ~p", 
				   [Service, NetworkAddress]),
		    {ok, [ { status, rvi_common:json_rpc_status(ok) },
			   { network_address, NetworkAddress }]}
	    end

    end.


get_remote_services() ->
    get_services(?REMOTE_SERVICE_TABLE).

get_local_services() ->
    get_services(?LOCAL_SERVICE_TABLE).

get_services(Table) ->
    Services = ets:foldl(fun(#service_entry {service = ServiceName, 
					     network_address = ServiceAddr}, Acc) -> 
				 [ {struct, 
				    [ 
				      {service, ServiceName}, 
				      {address, ServiceAddr}
				    ]
				   } | Acc ] end, 
			 [], Table),

    ?info("service_discovery_rpc:get_services(): ~p", [ Services]),
    {ok, [ { status, rvi_common:json_rpc_status(ok) },
	   { services, {array, Services }}]}.


%%
%% Get all unique network addresses that are currently active.
%%
get_remote_network_addresses() ->
    get_network_addresses(?REMOTE_ADDRESS_TABLE).

get_local_network_addresses() ->
    get_network_addresses(?LOCAL_SERVICE_TABLE).

get_network_addresses(Table) ->
    AddrList = ets:foldl(fun(#service_entry {network_address = NetworkAddr}, Acc) 
				when NetworkAddr =:= unavailable -> 
				  Acc; %% Don't report if service is not active

			     %% We have an active network address
			     (#service_entry {network_address = NetworkAddr}, Acc)  ->
				  %% Avoid duplicates
				  case lists:keyfind(NetworkAddr, 1, Acc) of
				      false ->[ NetworkAddr | Acc ];
				      _ -> Acc
				  end
			  end, [], Table),

    %% Remove duplicates
    ScrubbedAddrList = sets:to_list(sets:from_list(AddrList)),
    ?info("service_discovery_rpc:get_network_addresses(~p): ~p", [ Table, ScrubbedAddrList ]),

    {ok, [ { status, rvi_common:json_rpc_status(ok) },
	   { addresses, {array, ScrubbedAddrList }}]}.


%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc("register_local_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),
    register_local_service(Service, Address);


handle_rpc("register_remote_services", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),

    %% Loop through the services and register them.
    case Services of 
	[] -> register_remote_service(Address); 
	_ -> lists:map(fun(Svc) -> register_remote_service(Svc, Address) end, Services)
    end,


    %% Forward to scheduler now that we have updated our own state
    rvi_common:send_component_request(schedule, register_remote_services, 
				      [
				       {services, Services}, 
				       { network_address, Address }
				      ]),

    %% Leave service edge out of it for now, to ease use cases.
    %% rvi_common:send_component_request(service_edge, register_remote_services, 
    %% 				      [
    %% 				       {services, Services}, 
    %% 				       { network_address, Address }
    %% 				      ]),

    {ok, [ { status, rvi_common:json_rpc_status(ok) } ]};

handle_rpc("unregister_remote_services", Args) ->
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),

    %% Loop through the services and de-register them.
    unregister_remote_service(Address),
    {ok, [ { status, rvi_common:json_rpc_status(ok) } ]};

handle_rpc("resolve_remote_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    resolve_remote_service(Service);

handle_rpc("get_remote_services", _Args) ->
    get_remote_services();

handle_rpc("get_remote_network_addresses", _Args) ->
    get_remote_network_addresses();


handle_rpc("resolve_local_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    resolve_local_service(Service);

handle_rpc("get_local_services", _Args) ->
    get_local_services();

handle_rpc("get_local_network_addresses", _Args) ->
    get_local_network_addresses();

handle_rpc( Other, _Args) ->
    ?info("service_discovery_rpc:handle_rpc(~p)", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.

