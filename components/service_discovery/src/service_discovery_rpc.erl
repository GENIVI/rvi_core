%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(service_discovery_rpc).
-behaviour(gen_server).

-export([handle_rpc/2,
	 handle_notification/2]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([get_all_services/1,
	 get_local_addresses/1,
	 get_remote_addresses/1,
	 get_local_services/1,
	 get_remote_services/1,
	 subscribe_to_service_availability/2,
	 unsubscribe_from_service_availability/2,
	 resolve_local_service/2,
	 resolve_remote_service/2,
	 register_remote_services/3,
	 register_local_service/3,
	 unregister_remote_services_by_address/2,
	 unregister_remote_services_by_name/2,
	 unregister_local_service/2]).

-export([start_json_server/0]).

-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").

-define(LOCAL_SERVICE_TABLE, rvi_local_services).
-define(REMOTE_SERVICE_TABLE, rvi_remote_services).
-define(REMOTE_ADDRESS_TABLE, rvi_address_services).
-define(NOTIFICATION_SUBS_TABLE, rvi_notification_subs).

-record(service_entry, {
	  service = [],
	  address = {undefined,undefined} %% Module and Address where service can be found
	 }).

-record(notification_subs, {
	  service = [],
	  modules = [] %% List of modules subscribing to this service
	 }).

-define(SERVER, ?MODULE). 
-record(st, {
	  %% Component specification
	  cs = #component_spec{}
	 }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?debug("service_discovery_rpc:init(): called."),
    ets:new(?REMOTE_SERVICE_TABLE, [set,  public, named_table, { keypos, #service_entry.service }]),
    ets:new(?REMOTE_ADDRESS_TABLE, [duplicate_bag,  public, named_table, 
				    {keypos, #service_entry.address}]),

    ets:new(?NOTIFICATION_SUBS_TABLE, [set,  public, named_table, { keypos, #notification_subs.service }]),

    {ok, #st { cs = rvi_common:get_component_specification() } }.


start_json_server() ->
    rvi_common:start_json_rpc_server(service_discovery, ?MODULE, service_discovery_sup).

get_all_services(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_all_services, [], [status, services], CompSpec).


get_local_services(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_local_services, [], [status, services], CompSpec).

get_remote_services(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_remote_services, [], [status, services], CompSpec).

get_local_addresses(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_local_addresses, [], [status], CompSpec).

get_remote_addresses(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_remote_addresses, [], [status], CompSpec).


resolve_local_service(CompSpec, RawService) ->
    rvi_common:request(service_discovery, ?MODULE, resolve_local_service, 
		       [{ service, RawService }],
		       [status, full_service], CompSpec).

resolve_remote_service(CompSpec, RawService) ->
    rvi_common:request(service_discovery, ?MODULE, resolve_remote_service, 
		       [{ service, RawService }],
		       [status], CompSpec).

register_remote_services(CompSpec, DataLinkModule, Address, Services) ->
    rvi_common:notification(service_discovery, ?MODULE, register_remote_services, 
			    [{ data_link_module, DataLinkModule },
			     { address, Address },  
			     { services, Services }],
			    CompSpec).


register_local_service(CompSpec, Address, Services) ->
    rvi_common:request(service_discovery, ?MODULE, register_local_service, 
			    [{ address, Address }, 
			     { service, Services }],
			    [status], CompSpec).


unregister_remote_services_by_address(CompSpec, DataLinkModule, Address) ->
    rvi_common:notification(service_discovery, ?MODULE, unregister_remote_services_by_address, 
			    [{ data_link_module, DataLinkModule},
			     { address, Address }],
			    CompSpec).

unregister_remote_services_by_name(CompSpec, Service) ->
    rvi_common:notification(service_discovery, ?MODULE, unregister_remote_services_by_name, 
			    [{ services, Service }], 
			    CompSpec).

unregister_local_service(CompSpec, Service) ->
    rvi_common:notification(service_discovery, ?MODULE, unregister_local_service, 
			    [{ service, Service }], 
			    CompSpec).


subscribe_to_service_availability(Service, Module) ->
    rvi_common:notification(service_discovery, ?MODULE, subscribe_to_service_availability, 
			    [{ service, Service }], 
			    [{ module, Module }], 
			    CompSpec).

unsubscribe_from_service_availability(Service, Module) ->
    rvi_common:notification(service_discovery, ?MODULE, unsubscribe_from_service_availability, 
			    [{ service, Service }], 
			    [{ module, Module }], 
			    CompSpec).
    

%% JSON-RPC entry point
%% Called by local exo http server

%% Register remote services
handle_notification("register_remote_services", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),
    {ok, Address} = rvi_common:get_json_element(["address"], Args),

    gen_server:cast(?SERVER, { rvi, register_remote_services, 
			       [ Services, DataLinkModule, Address ]}),
    ok;



handle_notification("unregister_remote_services_by_address", Args) ->
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),
    {ok, Address} = rvi_common:get_json_element(["address"], Args),
    gen_server:cast(?SERVER, { rvi, unregister_remote_services_by_address, 
			       [ DataLinkModule, Address ]}),
    ok;


handle_notification("unregister_remote_services_by_name", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    gen_server:cast(?SERVER, { rvi, unregister_remote_service_by_Name, 
			       [ Services ]}),
    ok;



handle_notification("unregister_local_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    %% De-register service
    gen_server:cast(?SERVER, { rvi, unregister_local_service, 
			       [ Service ]}),
    ok;


handle_notification( Other, _Args) ->
    ?info("service_discovery_rpc:handle_notification(~p): unknown", [ Other ]),
    ok.

handle_notification("subscribe_to_service_availability", Args) ->
    {ok, Service } = rvi_common:get_json_element(["service"], Args),
    {ok, Module } = rvi_common:get_json_element(["module"], Args),

    %% De-register service
    gen_server:cast(?SERVER, { rvi, subscribe_to_service_availability, 
			       [ Service, Module ]}),
    ok;

handle_notification("unsubscribe_from_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Module } = rvi_common:get_json_element(["module"], Args),

    %% De-register service
    gen_server:cast(?SERVER, { rvi, unsubscribe_from_service, 
			       [ Service, Module ]}),
    ok;



%% Register local services
handle_rpc("register_local_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Address} = rvi_common:get_json_element(["address"], Args),

    [ok, FullSvcName] = gen_server:call(?SERVER, { rvi, register_local_service, 
						   [ Service, Address ]}),

    {ok, [ {status, rvi_common:json_rpc_status(ok) }, { full_service_name, FullSvcName }]};


%%
%% Get remote services
%%
handle_rpc("get_remote_services", _Args) ->
    [ok, Services ] = gen_server:call(?SERVER, { rvi, get_remote_services,
						 [ ]}),
    JSONServices = convert_json_services(Services),

    {ok, [ {status, rvi_common:json_rpc_status(ok)} , 
	   { services, { array, JSONServices } }]};

%%
%% Get all services
%%
handle_rpc("get_all_services", _Args) ->
    ?debug("service_discovery_rpc:get_all_services(json-rpc)"),
    [ok, Services ] = gen_server:call(?SERVER, { rvi, get_all_services, 
						 []}),
    ?debug("service_discovery_rpc:Done"),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} , { services, { array, Services } }]};


%%
%% Get remote network addresses
%%
handle_rpc("get_remote_addresses", _Args) ->
    [ok, Addresses ] = gen_server:call(?SERVER, { rvi, get_remote_addresses, 
						 []}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)}, { addresses, { array, Addresses }}]};

%%
%% Resolve remote service
%%
handle_rpc("resolve_remote_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    
    case gen_server:call(?SERVER, { rvi, resolve_remote_service, 
				    [Service]}) of
	[ok, Addresses ] -> 
	    {ok, [ {status, rvi_common:json_rpc_status(ok)}, { addresses, { array, Addresses }}]};

	[ Other ]  -> {ok, [ {status, rvi_common:json_rpc_status(Other)} ]}
    end;

    

%%
%% Resolve local service
%%
handle_rpc("resolve_local_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),

    case gen_server:call(?SERVER, { rvi, resolve_local_service, 
				    [Service]}) of
	[ok, Addresses ] -> 
	    {ok, [ {status, rvi_common:json_rpc_status(ok)}, { addresses, { array, Addresses }}]};

	[ Other ]  -> {ok, [ {status, rvi_common:json_rpc_status(Other)} ]}
    end;




%%
%% Get local services
%%
handle_rpc("get_local_services", _Args) ->
    [ok, LocalServices ] = 
	gen_server:call(?SERVER, { rvi, get_local_services, []}),
    
    JSONServices = convert_json_services(LocalServices),

    {ok, [ {status, rvi_common:json_rpc_status(ok)} , 
	   { services, { array, JSONServices }}]};



%%
%% Get local network addresses
%%
handle_rpc("get_local_addresses", _Args) ->
    [ok, LocalAddresses ] = 
	gen_server:call(?SERVER, { rvi, get_local_addresses, []}),

    {ok, [ {status, rvi_common:json_rpc_status(ok)} , { addresses, { array, LocalAddresses }}]};



%% 
%% Handle the rest.
%%
handle_rpc( Other, _Args) ->
    ?info("service_discovery_rpc:handle_rpc(~p): unknown", [ Other ]),
    {ok, [ { status, invalid_command } ]}.


%% Handle calls received through regular gen_server calls, routed by
%% rvi_common:request()

handle_call({rvi, register_local_service, [Service, Address] }, _From, St) ->
    ?info("service_discovery_rpc:register_local_service(): ~p ->  ~p",
	  [Service, Address]),

    FullSvcName = rvi_common:local_service_to_string(Service),

    ets:insert(?LOCAL_SERVICE_TABLE, 
	       #service_entry {
		  service = FullSvcName,
		  address = Address
		 }),
    
    notify_subscribers(available, Service, local, Address), 
    {reply, [ ok, FullSvcName ], St  };


handle_call({rvi, resolve_remote_service, [RawService]}, _From, St) ->
    Service = rvi_common:sanitize_service_string(RawService),
    ?debug("service_discovery_rpc:resolve_remote_service(): RawService:      ~p", [RawService]),
    ?debug("service_discovery_rpc:resolve_remote_service(): Cleaned Service: ~p", [Service]),
    case resolve_service_(?REMOTE_SERVICE_TABLE, Service) of
	{ok, Address } ->
	    {reply, [ok, Address ], St};

	not_found ->
	    ?debug("service_discovery_rpc:resolve_remote_service(~p): Service not found in ets. "
		  "Trying static nodes",
		  [Service]),

	    
	    %% Check if this is a service residing on the backend server
	    case rvi_common:get_static_node(Service) of
		not_found -> %% Not found
		    ?info("service_discovery_rpc:resolve_remote_service(~p): Service not found.", 
			   [Service]),
		    
		    {reply, [not_found], St };

		Address -> %% Found
		    ?debug("service_discovery_rpc:resolve_service(~p): Service is on static node ~p", 
			   [Service, Address]),
		    
		    {reply, [ok, Address ], St}
	    end
    end;

handle_call({rvi, get_remote_services, _Args}, _From, St) ->
    Services = get_services_(?REMOTE_SERVICE_TABLE),
    {reply,  [ok, Services ], St };


handle_call({rvi, get_all_services, _Args}, _From, St) ->
    RemoteSvc = ets:foldl(fun(#service_entry {service = ServiceName}, Acc) -> 
				  [ ServiceName  | Acc ] end, 
			  [], ?REMOTE_SERVICE_TABLE),

    LocalSvc = ets:foldl(fun(#service_entry {service = ServiceName}, Acc) -> 
				  [ ServiceName  | Acc ] end, 
			  [], ?LOCAL_SERVICE_TABLE),

    Services = RemoteSvc++LocalSvc,

    {reply,  [ok, Services], St };


handle_call({rvi, get_remote_addresses, _Args}, _From, St) ->
    Addresses =  get_addresses_(?REMOTE_ADDRESS_TABLE),
    {reply, [ ok, Addresses ], St };


handle_call({rvi, resolve_local_service, [RawService]}, _From, St) ->
    Service = rvi_common:sanitize_service_string(RawService),
    ?debug("service_discovery_rpc:resolve_local_service(): RawService:      ~p", [RawService]),
    ?debug("service_discovery_rpc:resolve_local_service(): Cleaned Service: ~p", [Service]),

    case resolve_service_(?LOCAL_SERVICE_TABLE, Service) of 
	not_found ->
	    { reply, [not_found], St };
	
	{ok, Address } ->
	    { reply, [ok, Address], St}
    end;

handle_call({rvi, get_local_services, _Args}, _From, St) ->
    Services = get_services_(?LOCAL_SERVICE_TABLE),
    {reply,  [ok,  Services ], St };

handle_call({rvi, get_local_addresses, _Args}, _From, St) ->
    Addresses = get_addresses_(?LOCAL_SERVICE_TABLE),
    {reply, [ ok, Addresses ], St };


handle_call(Other, _From, St) ->
    ?warning("service_discovery_rpc:handle_call(~p): unknown", [ Other ]),
    { reply,  [unknown_command] , St}.


handle_cast({rvi, subscribe_to_service_availability, [Service, Module] }, St) ->
    case ets:lookup(?NOTIFICATION_SUBS_TABLE, Service) of
	'$end_of_table' ->
	    %% Insert new entry.
	    ets:insert(?NOTIFICATION_SUBS_TABLE,
		       #notification_subs { service = Service, 
					    modules = [Module] });

	#notification_subs { modules = Modules } ->
	    %% Replace existing entry
	    ets:insert(?NOTIFICATION_SUBS_TABLE,
		       #notification_subs { service = Service, 
					    modules = lists:usort([ Module | Modules ]) }),
	    
    end,
    notify_on_existing_service(Service, Module)
    { noreply, St};

handle_cast({rvi, unsubscribe_to_service_availability, [Service, Module] }, St) ->
    case ets:lookup(?NOTIFICATION_SUBS_TABLE, Service) of
	'$end_of_table' ->
	    %% No match.
	    ok;

	#notification_subs { modules = Modules } ->
	    %% Replace existing entry
	    ets:insert(?NOTIFICATION_SUBS_TABLE,
		       #notification_subs { service = Service, 
					    modules = Modules -- Module })
    end,
    { noreply, St};

handle_cast({rvi, register_remote_services, [Services, DataLinkModule, Address] }, St) ->
    case Services of 
	%% We have zero services associated with address.
	%% Just register the address.
	[] -> 
	    ?info("service_discovery_rpc:register_remote_service_(): service(n/a) -> ~p", 
		  [Address]),

	    ets:insert(?REMOTE_ADDRESS_TABLE, 
		       #service_entry {
			  service = "",
			  address = { Address, DataLinkModule }
			 }),

	    dump_table(?REMOTE_ADDRESS_TABLE),
	    ok;

	%% Loop through the services and register them.
	_ -> 
	    lists:map(fun(Svc) -> 
			      register_remote_service_(Svc, 
						       DataLinkModule, 
						       Address) 
		      end, Services),

	    notify_subscribers(available, Service, DataLinkModule, Address), 

	    %% Forward to scheduler now that we have updated our own state
	    schedule_rpc:register_remote_services(St#st.cs, Address, Services),

	    %% Forward to service edge so that it can inform its locally
	    %% connected services.
	    %% Build a list of all our local services' addresses to provide
	    %% to service edge so that it knows where to send the,
	    LocalSvcAddresses = 
		ets:foldl(fun(#service_entry { address = LocalAddress }, Acc) -> 
				  [ LocalAddress | Acc ] end, 
			  [], ?LOCAL_SERVICE_TABLE),

	    %% Call service edge with local addresses (sorted and de-duped) and
	    %% the services to register.
	    service_edge_rpc:
		register_remote_services(St#st.cs, 
					 Services, 
					 lists:usort(LocalSvcAddresses))
    end,
    {noreply, St };


%%
%% Delete all services registered under the given address
%%
handle_cast({rvi, unregister_remote_services_by_address, [DataLinkModule, Address]}, St) ->

    %% Retrieve all services associated with the remote address
    Svcs = ets:lookup(?REMOTE_ADDRESS_TABLE, { Address, DataLinkModule }),

    %% We now have a list of service records, convert them to a list of service
    %% names and send them of to schedule for deregistration
    AllSvcNames = lists:foldr(fun(#service_entry {  service = SvcName }, Acc) -> 
				      [SvcName | Acc]
			      end, [], Svcs),

    ?info("service_discovery_rpc:unregister_remote_services_by_address(): ~p -> ~p", 
	  [Address, AllSvcNames]),

    %% We need to filter AllSvcNames to remove all service entries that have
    %% been registered under another name. 
    %% We do this by creating a list of all matching entries associated
    %% with a network address not matching the disconnected Address
    %%
    %% See issue https://github.com/PDXostc/rvi/issues/14 for details
    FilterSvc =
	lists:foldr(
	  fun(Service, Acc) -> 
		  %% Lookup the service in the service table.
		  case ets:lookup(?REMOTE_SERVICE_TABLE, Service) of

		      %% Not found. Do not filter out.
		      [] ->
			  Acc;

		      %% We found or own entry, tiet to the disconnected address.
		      %% Do not add to addresses to be removed.
		      [ #service_entry { address = { Address, DataLinkModule} } ] ->
			  Acc;

		      %% We found an entry that does not the disconnected
		      %% network address. This one should be filtered out
		      [ _ ] ->
			  [ Service | Acc ]

		  end 
	  end, [], AllSvcNames),
    
    SvcNames = AllSvcNames -- FilterSvc,

    ?info("service_discovery_rpc:unregister_remote_services_by_address(): "
	  "Resurrected services: ~p", 
	  [FilterSvc]),
    

    %% Delete any addresses stored with an empty service name,
    %% installed with register_remote_service/1, since we now have at
    %% least one service name.
    ets:match_delete(?REMOTE_ADDRESS_TABLE, 
		     #service_entry { 
			service = [], 
			address = { Address, DataLinkModule} 
		      }),

    ets:delete(?REMOTE_ADDRESS_TABLE, {Address, DataLinkModule}),

    %% Go through all service names and unregister them with schedulew
    %% and service edge
    case SvcNames of 
	%% Nothing to do.
	[] ->
	    true;
	_ ->
	    notify_subscribers(unavailable, SvcNames, DataLinkModule, Address), 

	    %% Tell scheduler to kill off services
	    schedule_rpc:unregister_remote_services(St#st.cs, SvcNames),

	    %% Delete all services from remote service table.
	    [ ets:delete(?REMOTE_SERVICE_TABLE, Svc#service_entry.service) || Svc <- Svcs ],

 	    %% Forward to service edge so that it can inform its locally
	    %% connected services.
	    %% Build a list of all our local services' addresses to provide
	    %% to service edge so that it knows where to send the,
	    LocalSvcAddresses = 
		ets:foldl(fun(#service_entry { address = LocalAddress }, Acc) -> 
				  [ LocalAddress | Acc ] end, 
			  [], ?LOCAL_SERVICE_TABLE),

	    %% Call service edge with local addresses (sorted and
	    %% de-duped) and the services to register.
	    service_edge_rpc:unregister_remote_services(St#st.cs,
							SvcNames, 
							lists:usort(LocalSvcAddresses))

    end,
    {noreply, St };

handle_cast({rvi, unregister_remote_services_by_name, [Services]}, St) ->
    unregister_remote_services_by_name_(St#st.cs, Services),
    {noreply, St };

handle_cast({rvi, unregister_local_service, [Service]}, St) ->
    ?info("service_discovery_rpc:unregister_local_service(): ~p", 
	  [Service]),

    ets:delete(?LOCAL_SERVICE_TABLE, Service),
    {noreply, St };

handle_cast(Other, St) ->
    ?warning("service_discovery_rpc:handle_cast(~p): unknown", [ Other ]),
    {noreply, St}.

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


%%
%% INTERNAL SUPPORT FUNCTIONS
%%
dump_table(_Table, '$end_of_table') ->
    true;

dump_table(Table, Key) ->
    Val = ets:lookup(Table, Key),
    ?info("Table: ~p(~p) - ~p", [ Table, Key, Val ]),
    dump_table(Table, ets:next(Table, Key)).

dump_table(Table) ->
    dump_table(Table, ets:first(Table)).


register_remote_service_(Service, DataLinkModule, Address) ->
    ?info("service_discovery_rpc:register_remote_service_(): service(~p) -> ~p", 
	  [Service, Address]),

    FullSvcName = rvi_common:remote_service_to_string(Service),

    ets:insert(?REMOTE_SERVICE_TABLE, 
	       #service_entry {
		  service = FullSvcName,
		  address = { Address, DataLinkModule }
		 }),

    %% Delete any addresses stored with an empty service name,
    %% installed with register_remote_service/1, since we now have at
    %% least one service name.
    ets:match_delete(?REMOTE_ADDRESS_TABLE, 
		     #service_entry { 
			service = [], 
			address = { Address, DataLinkModule} 
		      }),

    %% Delete any previous instances of the given entry, in case
    %% the service registers multiple times
    ets:match_delete(?REMOTE_ADDRESS_TABLE, 
		     #service_entry { 
			service = FullSvcName, 
			address = { Address , DataLinkModule }
		      }),

    %% Insert new element
    ets:insert(?REMOTE_ADDRESS_TABLE, 
	       #service_entry {
		  service = FullSvcName,
		  address = { Address, DataLinkModule}
		 }),

    {ok, FullSvcName}.



unregister_single_remote_service_by_name_(CompSpec, Service) ->
    ?info("service_discovery_rpc:unregister_single_remote_service_by_name_(): ~p", 
	  [Service]),


    %% Delete any remote address table entries with a matching Service.
    ets:match_delete(?REMOTE_ADDRESS_TABLE, 
		     #service_entry { 
			service = Service,
			address = '_'
		      }),

    ets:delete(?REMOTE_SERVICE_TABLE, Service),
    After = ets:foldl(fun(#service_entry { service = Svc }, Acc) -> 
			      [ Svc | Acc ] end, 
		      [], ?REMOTE_SERVICE_TABLE),

    ?debug("Ater removing ~p: ~p", [ Service, After ]),
    
    %% Forward to service edge so that it can inform its locally
    %% connected services.
    %% Build a list of all our local services' addresses to provide
    %% to service edge so that it knows where to send the,
    LocalSvcAddresses = 
	ets:foldl(fun(#service_entry { address = LocalAddress }, Acc) -> 
			  [ LocalAddress | Acc ] end, 
		  [], ?LOCAL_SERVICE_TABLE),
    
    %% Call service edge with local addresses (sorted and de-duped) and
    %% the services to register.
    notify_subscribers(unavailable, [Service], fixme, fixme), 

    service_edge_rpc:unregister_remote_services(CompSpec,
						[[Service]], 
						lists:usort(LocalSvcAddresses)),


    ok.


%% Loop through multiple services and remove them one by one
unregister_remote_services_by_name_(CompSpec, Services) ->
    [ unregister_single_remote_service_by_name_(CompSpec, Svc) || Svc <- Services],
    ok.



get_services_(Table) ->
    Services = ets:foldl(fun(#service_entry {service = ServiceName, 
					     address = ServiceAddr}, Acc) -> 
				 [ { ServiceName, ServiceAddr } | Acc ] end, 
			 [], Table),

    ?debug("service_discovery_rpc:get_services_(): ~p", [ Services ]),
    Services.


get_addresses_(Table) ->
    AddrList = ets:foldl(fun(#service_entry {address = Addr}, Acc) 
				when Addr =:= unavailable -> 
				  Acc; %% Don't report if service is not active

			     %% We have an active network address
			     (#service_entry {address = Addr}, Acc)  ->
				  %% Avoid duplicates
				  case lists:keyfind(Addr, 1, Acc) of
				      false ->[ Addr | Acc ];
				      _ -> Acc
				  end
			  end, [], Table),


    %% Return a dup-scrubbed list.
    Addresses = sets:to_list(sets:from_list(AddrList)),
    ?debug("service_discovery_rpc:get_addresses(~p): ~p", [ Table, Addresses ]),
    Addresses.


resolve_service_(Table, Service) ->
    case ets:lookup(Table, Service) of
	%% We found a service entry, report it back
	[#service_entry { address = Address }] ->
	    ?debug("service_discovery_rpc:resolve_service_(~p): service: ~p -> ~p", 
		   [ Table, Service, Address ]),

	    {ok, Address };

	%% We did not find a service entry, check statically configured nodes.
	[] -> 
	    ?debug("service_discovery_rpc:resolve_service_(~p): service: ~p -> Not Found", 
		   [ Table, Service ]),
	    not_found
    end.



%% Convert services returned by get_services_ to JSON format
convert_json_services(Services) ->
    JSONServices = lists:foldl(fun({ ServiceName, ServiceAddr}, Acc) -> 
				 [ {struct, 
				    [ 
				      {service, ServiceName }, 
				      {address, ServiceAddr }
				    ]
				   } | Acc ] end, 
			 [], Services),
    ?debug("service_discovery_rpc:get_services_(): ~p", [ Services]),
    JSONServices.


get_subscribers([], St) ->
    [];


%% jlr.com/abc
notify_subscribers(_Available, [], _DataLinkModule, _Address) -> 
    ok;


notify_subscribers(Available, [ Service | Rem], DataLinkModule, Address) -> 
    %% ?NOTIFICATION_SUBS_TABLE contains services that
    %% should be matched against the given service.
    %% If there is a match, we notify the associated module

    Fun = case Available of
	      available -> service_available;
	      unavailable -> service_unavailable
	  end,

    %% Retrieve all modules subscribing to a specific service.
    case ets:lookup(?NOTIFICATION_SUBS_TABLE, Service) of
	'$end_of_table' ->
	    ok;
	
	#notify_subscribers { modules = Modules } ->
	    %% Notify each subscriber of the given service.
	    [ Module:Fun(Service, DataLinkModule, Address) || Module <- Modules],
	    ok
    end,
    
    %% Move on to remaining subscribers.
    notify_subscribers(Address, Rem, Address).



notify_on_existing_service(Service, Module) ->
    case resolve_service_(?LOCAL_SERVICE_TABLE, Service) of 
	not_found ->
	    case resolve_service(?REMOTE_SERVICE_TABLE, Service) of
		not_found -> not_found;
		{ ok, { DataLinkModule, Address }} ->
		    Module:service_available(Service, DataLinkModule, Address),
		    ok
	    end;
	
	{ok, Address } ->
	    Module:service_available(Service, local, Address)
    end
