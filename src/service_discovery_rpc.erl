-module(service_discovery_rpc).

-export([handle_rpc/2]).
-export([init/0]).

-include_lib("lager/include/log.hrl").
-define(SERVICE_TABLE, rvi_service_discovery).


-record(service_entry, {
	  service = [],
	  network_address = undefined %% Address where service can be found
	 }).

%% Called by service_discovery_app:start_phase().
init() ->
    ets:new(?SERVICE_TABLE, [set,  public, named_table, {keypos,2}]),
    case rvi_common:get_component_config(service_discovery, exo_http_opts) of
	{ ok, ExoHttpOpts } ->
	    exoport_exo_http:instance(service_discovery_sup, 
				      service_discovery_rpc,
				      ExoHttpOpts);
	Err -> Err
    end,
    ok.


register_service(Service, NetworkAddress) ->
    ?debug("service_discovery_rpc:register_service(): service:         ~p ~n", [Service]),
    ?debug("service_discovery_rpc:register_service(): network_address: ~p ~n", [NetworkAddress]),

    ets:insert(?SERVICE_TABLE, 
	       #service_entry {
		  service = rvi_common:service_to_string(Service),
		  network_address = NetworkAddress
		 }),

    {ok, [ { status, rvi_common:json_rpc_status(ok)}]}.


resolve_service(RawService) ->
    Service = rvi_common:sanitize_service_string(RawService),
    ?debug("service_discovery_rpc:resolve_service(~p): raw(~p)~n", [Service, RawService]),
    Svcs = ets:foldl(fun({service_entry, ServiceName, ServiceAddr}, Acc) -> 
			     [  {ServiceName, ServiceAddr}  | Acc ] end, 
			 [], ?SERVICE_TABLE),

    ?debug("service_discovery_rpc:resolve_service(~p): ~p~n", [Service, Svcs]),
    
    case ets:lookup(?SERVICE_TABLE, Service) of
	[] -> 
	    ?debug("service_discovery_rpc:resolve_service(~p): Service not found in ets~n",
		     [Service]),

	    %% Check if this is a service residing on the backend server
	    case rvi_common:is_backend_service(Service) of
		false -> %% Not found
		    ?debug("service_discovery_rpc:resolve_service(~p): Service not found backend~n", [Service]),
		    
		    { ok, [ { status, rvi_common:json_rpc_status(not_found) }]};

		true -> %% FOund
			    ?debug("service_discovery_rpc:resolve_service(~p): Service is backend~n", [Service]),	    {ok, [ { status, rvi_common:json_rpc_status(ok) },
			   { network_address, rvi_common:backend_address_string() }]}
	    end;

	[#service_entry { network_address = NetworkAddress }] ->
	    ?debug("service_discovery_rpc:resolve_service(): service: ~p -> ~p~n", 
		      [ Service, NetworkAddress ]),

	    {ok, [ { status, rvi_common:json_rpc_status(ok) },
		   { network_address, NetworkAddress }]}
    end.


get_services() ->
    Services = ets:foldl(fun({service_entry, ServiceName, ServiceAddr}, Acc) -> 
				 [ {struct, 
				    [ 
				      {service, ServiceName}, 
				      {address, ServiceAddr}
				    ]
				   } | Acc ] end, 
			 [], ?SERVICE_TABLE),

    ?debug("service_discovery_rpc:get_services(): ~p~n", [ Services]),
    {ok, [ { status, rvi_common:json_rpc_status(ok) },
	   { services, {array, Services }}]}.


%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc("register_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),
    register_service(Service, Address);

handle_rpc("resolve_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    resolve_service(Service);

handle_rpc("get_services", _Args) ->
    get_services();

handle_rpc( Other, _Args) ->
    ?debug("service_discovery_rpc:handle_rpc(~p)~n", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.

