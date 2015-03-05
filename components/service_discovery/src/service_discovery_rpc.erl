%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(service_discovery_rpc).
-behaviour(gen_server).

-export([handle_rpc/2]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([init_rvi_component/0]).

-include_lib("lager/include/log.hrl").
-define(LOCAL_SERVICE_TABLE, rvi_local_services).
-define(REMOTE_SERVICE_TABLE, rvi_remote_services).
-define(REMOTE_ADDRESS_TABLE, rvi_address_services).

-record(service_entry, {
	  service = [],
	  network_address = undefined %% Address where service can be found
	 }).

-define(SERVER, ?MODULE). 
-record(st, { }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?debug("service_discovery_rpc:init(): called."),
    {ok, #st {}}.

%% Called by service_discovery_app:start_phase().
init_rvi_component() ->
    ets:new(?LOCAL_SERVICE_TABLE,  [set,  public, named_table, { keypos, #service_entry.service }]),
    ets:new(?REMOTE_SERVICE_TABLE, [set,  public, named_table, { keypos, #service_entry.service }]),
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

dump_table(_Table, '$end_of_table') ->
    true;

dump_table(Table, Key) ->
    Val = ets:lookup(Table, Key),
    ?info("Table: ~p(~p) - ~p", [ Table, Key, Val ]),
    dump_table(Table, ets:next(Table, Key)).

dump_table(Table) ->
    dump_table(Table, ets:first(Table)).

register_remote_service(NetworkAddress) ->
    ?info("service_discovery_rpc:register_remote_service(): service:         empty"),
    ?info("service_discovery_rpc:register_remote_service(): network_address: ~p", [NetworkAddress]),

    ets:insert(?REMOTE_ADDRESS_TABLE, 
	       #service_entry {
		  service = "",
		  network_address = NetworkAddress
		 }),

    dump_table(?REMOTE_ADDRESS_TABLE),
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

    %% Delete any previous instances of the given entry, in case
    %% the service registers multiple times
    ets:match_delete(?REMOTE_ADDRESS_TABLE, 
		     #service_entry { 
			service = FullSvcName, 
			network_address = NetworkAddress 
		      }),

    ets:insert(?REMOTE_ADDRESS_TABLE, 
	       #service_entry {
		  service = FullSvcName,
		  network_address = NetworkAddress
		 }),

    {ok, [ {service, FullSvcName}, { status, rvi_common:json_rpc_status(ok)}]}.


unregister_remote_services(NetworkAddress) ->
    ?info("service_discovery_rpc:unregister_remote_services(): network_address: ~p", 
	  [NetworkAddress]),

    %% Delete all services registered under the given address.
    Svcs = ets:lookup(?REMOTE_ADDRESS_TABLE, NetworkAddress),

    ?info("service_discovery_rpc:unregister_remote_services(): ~p -> ~p", 
	  [NetworkAddress, Svcs]),
    

    %% We now have a bunch of service records, convert them to a list of service
    %% names and send them of to schedule for deregistration
    SvcNames = lists:foldr(fun(#service_entry {  service = SvcName }, Acc) -> 
				       [SvcName | Acc]
			       end, [], Svcs),

    ets:delete(?REMOTE_ADDRESS_TABLE, NetworkAddress),
    case Svcs of 
	[] ->
	    true;
	_ ->
	    rvi_common:send_component_request(schedule, unregister_remote_services, 
				      [
				       { services, SvcNames }
				      ]),

	    [ ets:delete(?REMOTE_SERVICE_TABLE, Svc#service_entry.service) || Svc <- Svcs ],

	    %% Forward to service edge so that it can inform its locally
	    %% connected services.
	    %% Build a list of all our local services' addresses to provide
	    %% to service edge so that it knows where to send the,
	    LocalSvcAddresses = 
		ets:foldl(fun(#service_entry { network_address = LocalAddress }, Acc) -> 
				  [ LocalAddress | Acc ] end, 
			  [], ?LOCAL_SERVICE_TABLE),

	    %% Call service edge with local addresses (sorted and de-duped) and
	    %% the services to register.
	    rvi_common:send_component_request(service_edge, unregister_remote_services, 
					      [
					       { local_service_addresses, lists:usort(LocalSvcAddresses)}, 
					       { services, SvcNames}				       
					      ])
    end,

    {ok, [ { status, rvi_common:json_rpc_status(ok)}]}.


unregister_local_service(Service) ->
    ?info("service_discovery_rpc:unregister_local_service(): Service~p", 
	  [Service]),

    ets:delete(?LOCAL_SERVICE_TABLE, Service),
    {ok, [ { status, rvi_common:json_rpc_status(ok)}]}.


register_local_service(NetworkAddress, Service) ->
    ?info("service_discovery_rpc:register_local_service(): service:         ~p", [Service]),
    ?info("service_discovery_rpc:register_local_service(): network_address: ~p", [NetworkAddress]),

    FullSvcName = rvi_common:local_service_to_string(Service),

    ets:insert(?LOCAL_SERVICE_TABLE, 
	       #service_entry {
		  service = FullSvcName,
		  network_address = NetworkAddress
		 }),
    FullSvcName.



resolve_local_service(RawService) ->
    Service = rvi_common:sanitize_service_string(RawService),
    ?info("service_discovery_rpc:resolve_local_service(): RawService:      ~p", [RawService]),
    ?info("service_discovery_rpc:resolve_local_service(): Cleaned Service: ~p", [Service]),
    case resolve_service(?LOCAL_SERVICE_TABLE, Service) of 
	not_found ->
	    { ok, [ { status, rvi_common:json_rpc_status(not_found) }]};
	
	{ok, NetworkAddress } ->
	    {ok, [ { status, rvi_common:json_rpc_status(ok) },
		   { network_address, NetworkAddress }]}
    end.

resolve_remote_service(RawService) ->
    Service = rvi_common:sanitize_service_string(RawService),
    ?info("service_discovery_rpc:resolve_remote_service(): RawService:      ~p", [RawService]),
    ?info("service_discovery_rpc:resolve_remote_service(): Cleaned Service: ~p", [Service]),
    case resolve_service(?REMOTE_SERVICE_TABLE, Service) of
	{ok, NetworkAddress } ->
	    {ok, [ { status, rvi_common:json_rpc_status(ok) },
		   { network_address, NetworkAddress }]};

	not_found ->
	    ?info("service_discovery_rpc:resolve_remote_service(~p): Service not found in ets. "
		  "Trying static nodes",
		  [Service]),

	    
	    %% Check if this is a service residing on the backend server
	    case rvi_common:get_static_node(Service) of
		not_found -> %% Not found
		    ?info("service_discovery_rpc:resolve_remote_service(~p): Service not found in static nodes.", 
			   [Service]),
		    
		    { ok, [ { status, rvi_common:json_rpc_status(not_found) }]};

		NetworkAddress -> %% Found
			    ?info("service_discovery_rpc:resolve_service(~p): Service is on static node ~p", 
				   [Service, NetworkAddress]),

		    {ok, [ { status, rvi_common:json_rpc_status(ok) },
			   { network_address, NetworkAddress }]}
	    end
    end.
					  
					  


register_remote_services(Address, Services) ->
    %% Loop through the services and register them.
    case Services of 
	[] -> register_remote_service(Address); 
	_ -> 
	    lists:map(fun(Svc) -> register_remote_service(Svc, Address) end, Services),

	    %% Forward to scheduler now that we have updated our own state
	    rvi_common:send_component_request(schedule, register_remote_services, 
						  [
						   {services, Services}, 
						   { network_address, Address }
						  ]),

	    %% Forward to service edge so that it can inform its locally
	    %% connected services.
	    %% Build a list of all our local services' addresses to provide
	    %% to service edge so that it knows where to send the,
	    LocalSvcAddresses = 
		ets:foldl(fun(#service_entry { network_address = LocalAddress }, Acc) -> 
				  [ LocalAddress | Acc ] end, 
			  [], ?LOCAL_SERVICE_TABLE),

	    %% Call service edge with local addresses (sorted and de-duped) and
	    %% the services to register.
	    rvi_common:send_component_request(service_edge, register_remote_services, 
					      [
					       { local_service_addresses, lists:usort(LocalSvcAddresses)}, 
					       { services, Services}				       
					      ])
    end,

    {ok, [ { status, rvi_common:json_rpc_status(ok) } ]}.

resolve_service(Table, Service) ->

    ?info("service_discovery_rpc:resolve_service(): CleanedService:    ~p", [Service]),

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

	    {ok, NetworkAddress };

	%% We did not find a service entry, check statically configured nodes.
	[] -> 
	    not_found
    end.



get_services(Table) ->
    Services = ets:foldl(fun(#service_entry {service = ServiceName, 
					     network_address = ServiceAddr}, Acc) -> 
				 [ {ServiceName, ServiceAddr } | Acc ] end, 
			 [], Table),

    ?info("service_discovery_rpc:get_services(): ~p", [ Services]),
    Services.

get_all_services() ->
    RemoteSvc = ets:foldl(fun(#service_entry {service = ServiceName}, Acc) -> 
				  [ ServiceName  | Acc ] end, 
			  [], ?REMOTE_SERVICE_TABLE),

    LocalSvc = ets:foldl(fun(#service_entry {service = ServiceName}, Acc) -> 
				  [ ServiceName  | Acc ] end, 
			  [], ?LOCAL_SERVICE_TABLE),

    Services = RemoteSvc++LocalSvc,
    ?info("service_discovery_rpc:get_all_services(): ~p", [ Services]),
    Services.


get_json_services(Table) ->
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


get_network_addresses_(Table) ->
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


    %% Return a dup-scrubbed list.
    Addresses = sets:to_list(sets:from_list(AddrList)),
    ?info("service_discovery_rpc:get_network_addresses(~p): ~p", [ Table, Addresses ]),
    Addresses.


get_json_network_addresses(Table) ->
    Addresses = get_network_addresses_(Table),
    {ok, [ { status, rvi_common:json_rpc_status(ok) },
	   { addresses, {array, Addresses }}]}.
    
get_network_addresses(Table) ->
    Addresses = get_network_addresses_(Table),
    {ok, [ { status, rvi_common:json_rpc_status(ok) },
	   { addresses, Addresses }]}.


%% JSON-RPC entry point
%% Called by local exo http server

%% Register local services

handle_rpc("register_local_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),
    FullSvcName   = register_local_service(Address, Service),

    {ok, [ { service, FullSvcName }, 
	   { status, rvi_common:json_rpc_status(ok) }
	 ]
    };


%% Register remote services

handle_rpc("register_remote_services", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),
    register_remote_services(Address, Services);


handle_rpc("unregister_remote_services", Args) ->
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),

    %% Loop through the services and de-register them.
    unregister_remote_services(Address);

handle_rpc("unregister_local_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    %% De-register service
    unregister_local_service(Service);


%%
%% Resolve remote service
%%
handle_rpc("resolve_remote_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    resolve_remote_service(Service);


%%
%% Get remote services
%%
handle_rpc("get_remote_services", _Args) ->
    get_json_services(?REMOTE_SERVICE_TABLE);

%%
%% Get all services
%%
handle_rpc("get_all_services", _Args) ->
    Services = get_all_services(),
    {ok, [ { status, rvi_common:json_rpc_status(ok) },
	   { services, {array, Services }}]};


%%
%% Get remote network addresses
%%
handle_rpc("get_remote_network_addresses", _Args) ->
    get_json_network_addresses(?REMOTE_ADDRESS_TABLE);


%%
%% Resolve local service
%%
handle_rpc("resolve_local_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    resolve_local_service(Service);


%%
%% Get local services
%%
handle_rpc("get_local_services", _Args) ->
    get_json_services(?LOCAL_SERVICE_TABLE);


%%
%% Get local network addresses
%%
handle_rpc("get_local_network_addresses", _Args) ->
    get_json_network_addresses(?LOCAL_SERVICE_TABLE);


%% 
%% Handle the rest.
%%
handle_rpc( Other, _Args) ->
    ?info("service_discovery_rpc:handle_rpc(~p)", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


%% Handle calls received through regular gen_server calls, routed byh
%% rvi_common:send_component_request()

handle_call({rvi_call, register_local_service, Args}, _From, State) ->
    {_, Service} = lists:keyfind(service, 1, Args),
    {_, Address} = lists:keyfind(network_address, 1, Args),
    FullSvcName  = register_local_service(Address, Service),

    {reply, {ok, [ { service, FullSvcName }, 
		   { status, rvi_common:json_rpc_status(ok) }
		 ]
	    }, State  };


handle_call({rvi_call, register_remote_services, Args}, _From, State) ->
    {_, Services} = lists:keyfind(services, 1, Args),
    {_, Address} = lists:keyfind(network_address, 1, Args),
    {reply, register_remote_services(Address, Services), State };

handle_call({rvi_call, unregister_remote_services, Args}, _From, State) ->
    {_, Address} = lists:keyfind(network_address, 1, Args),
    {reply, unregister_remote_services(Address), State };

handle_call({rvi_call, unregister_local_service, Args}, _From, State) ->
    {_, Service} = lists:keyfind(service, 1, Args),
    {reply, unregister_local_service(Service), State };

handle_call({rvi_call, resolve_remote_service, Args}, _From, State) ->
    {_, Service} = lists:keyfind(service, 1, Args),
    {reply, resolve_remote_service(Service), State };

handle_call({rvi_call, get_remote_services, _Args}, _From, State) ->
    Services = get_services(?REMOTE_SERVICE_TABLE),
    {reply,  {ok, 
	      [ { status, rvi_common:json_rpc_status(ok) },
		{ services, Services }]}, State };

handle_call({rvi_call, get_all_services, _Args}, _From, State) ->
    Services = get_all_services(),
    {reply,  {ok, 
	      [ { status, rvi_common:json_rpc_status(ok) },
		{ services, Services }]}, State };

handle_call({rvi_call, get_remote_network_addresses, _Args}, _From, State) ->
    {reply, get_network_addresses(?REMOTE_ADDRESS_TABLE), State };

handle_call({rvi_call, resolve_local_service, Args}, _From, State) ->
    {_, Service} = lists:keyfind(service, 1, Args),
    {reply, resolve_local_service(Service), State };

handle_call({rvi_call, get_local_services, _Args}, _From, State) ->
    Services = get_services(?LOCAL_SERVICE_TABLE),
    {reply,  {ok, 
	      [ { status, rvi_common:json_rpc_status(ok) },
		{ services, Services }]}, State };

handle_call({rvi_call, get_local_network_addresses, _Args}, _From, State) ->
    {reply, get_network_addresses(?LOCAL_SERVICE_TABLE), State };

handle_call(Other, _From, State) ->
    ?warning("service_discovery_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ]}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
