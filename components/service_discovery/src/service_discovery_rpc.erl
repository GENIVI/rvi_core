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

-export([get_all_services/1,
	 get_local_network_addresses/1,
	 get_remote_network_addresses/1,
	 get_local_services/1,
	 get_remote_services/1,
	 resolve_local_service/2,
	 resolve_remote_service/2,
	 register_remote_services/3,
	 register_local_service/3,
	 unregister_remote_services_by_address/2,
	 unregister_remote_service_by_name/2,
	 unregister_local_service/2]).

-export([start_json_server/0]).

-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").

-define(LOCAL_SERVICE_TABLE, rvi_local_services).
-define(REMOTE_SERVICE_TABLE, rvi_remote_services).
-define(REMOTE_ADDRESS_TABLE, rvi_address_services).

-record(service_entry, {
	  service = [],
	  network_address = undefined %% Address where service can be found
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
    ets:new(?LOCAL_SERVICE_TABLE,  [set,  public, named_table, { keypos, #service_entry.service }]),
    ets:new(?REMOTE_SERVICE_TABLE, [set,  public, named_table, { keypos, #service_entry.service }]),
    ets:new(?REMOTE_ADDRESS_TABLE, [duplicate_bag,  public, named_table, 
				    {keypos, #service_entry.network_address}]),

    {ok, #st { cs = rvi_common:get_component_specification() } }.


start_json_server() ->
    rvi_common:start_json_rpc_server(service_discovery, ?MODULE, service_discovery_sup).

get_all_services(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_all_services, [], [], [status, services], CompSpec).


get_local_services(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_local_services, [], [], [status, services], CompSpec).

get_remote_services(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_remote_services, [], [], [status, services], CompSpec).

get_local_network_addresses(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_local_network_addresses, [], [], [status], CompSpec).

get_remote_network_addresses(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_remote_network_addresses, [], [], [status], CompSpec).


resolve_local_service(CompSpec, RawService) ->
    rvi_common:request(service_discovery, ?MODULE, resolve_local_service, 
		       [RawService], [service], 
		       [status, full_service], CompSpec).

resolve_remote_service(CompSpec, RawService) ->
    rvi_common:request(service_discovery, ?MODULE, resolve_remote_service, 
		       [RawService], [service], 
		       [status], CompSpec).

register_remote_services(CompSpec, Address, Services) ->
    rvi_common:request(service_discovery, ?MODULE, register_remote_services, 
		       [Address, Services], [network_address, services], 
		       [status], CompSpec).


register_local_service(CompSpec, Address, Services) ->
    rvi_common:request(service_discovery, ?MODULE, register_local_service, 
		       [Address, Services], [network_address, service], 
		       [status], CompSpec).



unregister_remote_services_by_address(CompSpec, Address) ->
    rvi_common:request(service_discovery, ?MODULE, unregister_remote_services_by_address, 
		       [Address], [network_address], 
		       [status], CompSpec).

unregister_remote_service_by_name(CompSpec, Service) ->
    rvi_common:request(service_discovery, ?MODULE, unregister_remote_service_by_name, 
		       [Service], [service], 
		       [status], CompSpec).

unregister_local_service(CompSpec, Service) ->
    rvi_common:request(service_discovery, ?MODULE, unregister_local_service, 
		       [Service], [service], 
		       [status], CompSpec).


%% JSON-RPC entry point
%% Called by local exo http server

%% Register local services

handle_rpc("register_local_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),
    [ok, FullSvcName] = gen_server:call(?SERVER, { rvi_call, register_local_service, 
						   [ Service, Address ]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok) }, { full_service_name, FullSvcName }]};

%% Register remote services

handle_rpc("register_remote_services", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),

    [ok ] = gen_server:call(?SERVER, { rvi_call, register_remote_services, 
				      [ Services, Address ]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};


handle_rpc("unregister_remote_services_by_address", Args) ->
    {ok, Address} = rvi_common:get_json_element(["network_address"], Args),
    [ok] = gen_server:call(?SERVER, { rvi_call, unregister_remote_services_by_address, 
				       [ Address ]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};

handle_rpc("unregister_remote_service_by_name", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    [ok ] = gen_server:call(?SERVER, { rvi_call, unregister_remote_service_by_Name, 
				       [ Service ]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};


handle_rpc("unregister_local_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    %% De-register service
    [ok ] = gen_server:call(?SERVER, { rvi_call, unregister_local_service, 
				       [ Service ]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};


%%
%% Get remote services
%%
handle_rpc("get_remote_services", _Args) ->
    [ok, Services ] = gen_server:call(?SERVER, { rvi_call, get_remote_services,
						 [ ]}),
    JSONServices = convert_json_services(Services),

    {ok, [ {status, rvi_common:json_rpc_status(ok)} , 
	   { services, { array, JSONServices } }]};

%%
%% Get all services
%%
handle_rpc("get_all_services", _Args) ->
    ?debug("service_discovery_rpc:get_all_services(json-rpc)"),
    [ok, Services ] = gen_server:call(?SERVER, { rvi_call, get_all_services, 
						 []}),
    ?debug("service_discovery_rpc:Done"),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} , { services, { array, Services } }]};


%%
%% Get remote network addresses
%%
handle_rpc("get_remote_network_addresses", _Args) ->
    [ok, Addresses ] = gen_server:call(?SERVER, { rvi_call, get_remote_network_addresses, 
						 []}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)}, { addresses, { array, Addresses }}]};

%%
%% Resolve remote service
%%
handle_rpc("resolve_remote_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    
    case gen_server:call(?SERVER, { rvi_call, resolve_remote_service, 
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

    case gen_server:call(?SERVER, { rvi_call, resolve_local_service, 
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
	gen_server:call(?SERVER, { rvi_call, get_local_services, []}),
    
    JSONServices = convert_json_services(LocalServices),

    {ok, [ {status, rvi_common:json_rpc_status(ok)} , 
	   { services, { array, JSONServices }}]};



%%
%% Get local network addresses
%%
handle_rpc("get_local_network_addresses", _Args) ->
    [ok, LocalAddresses ] = 
	gen_server:call(?SERVER, { rvi_call, get_local_network_addresses, []}),

    {ok, [ {status, rvi_common:json_rpc_status(ok)} , { network_addresses, { array, LocalAddresses }}]};



%% 
%% Handle the rest.
%%
handle_rpc( Other, _Args) ->
    ?info("service_discovery_rpc:handle_rpc(~p): unknown", [ Other ]),
    {ok, [ { status, invalid_command } ]}.


%% Handle calls received through regular gen_server calls, routed by
%% rvi_common:request()

handle_call({rvi_call, register_local_service, [Service, Address] }, _From, State) ->
    ?info("service_discovery_rpc:register_local_service(): ~p ->  ~p",
	  [Service, Address]),

    FullSvcName = rvi_common:local_service_to_string(Service),

    ets:insert(?LOCAL_SERVICE_TABLE, 
	       #service_entry {
		  service = FullSvcName,
		  network_address = Address
		 }),
    {reply, [ ok, FullSvcName ], State  };


handle_call({rvi_call, register_remote_services, [Services, Address] }, _From, State) ->
    case Services of 
	%% We have zero services associated with address.
	%% Just register the address.
	[] -> 
	    ?info("service_discovery_rpc:register_remote_service_(): service(n/a) -> ~p", 
		  [Address]),

	    ets:insert(?REMOTE_ADDRESS_TABLE, 
		       #service_entry {
			  service = "",
			  network_address = Address
			 }),

	    dump_table(?REMOTE_ADDRESS_TABLE),
	    ok;

	%% Loop through the services and register them.
	_ -> 
	    lists:map(fun(Svc) -> 
			      register_remote_service_(Svc, Address) 
		      end, Services),

	    %% Forward to scheduler now that we have updated our own state
	    schedule_rpc:register_remote_services(Address, Services),

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
	    service_edge_rpc:register_remote_services(Services, lists:usort(LocalSvcAddresses))
    end,
    {reply, [ok], State };


%%
%% Delete all services registered under the given address
%%
handle_call({rvi_call, unregister_remote_services_by_address, [Address]}, _From, State) ->

    %% Retrieve all services associated with the remote address
    Svcs = ets:lookup(?REMOTE_ADDRESS_TABLE, Address),

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
		      [ #service_entry { network_address = Address } ] ->
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
			network_address = Address 
		      }),

    ets:delete(?REMOTE_ADDRESS_TABLE, Address),

    %% Go through all service names and unregister them with schedulew
    %% and service edge
    case SvcNames of 
	%% Nothing to do.
	[] ->
	    true;
	_ ->
	    %% Tell scheduler to kill off services
	    schedule_rpc:unregister_remote_services(SvcNames),

	    %% Delete all services from remote service table.
	    [ ets:delete(?REMOTE_SERVICE_TABLE, Svc#service_entry.service) || Svc <- Svcs ],

 	    %% Forward to service edge so that it can inform its locally
	    %% connected services.
	    %% Build a list of all our local services' addresses to provide
	    %% to service edge so that it knows where to send the,
	    LocalSvcAddresses = 
		ets:foldl(fun(#service_entry { network_address = LocalAddress }, Acc) -> 
				  [ LocalAddress | Acc ] end, 
			  [], ?LOCAL_SERVICE_TABLE),

	    %% Call service edge with local addresses (sorted and
	    %% de-duped) and the services to register.
	    service_edge_rpc:unregister_remote_services(SvcNames, lists:usort(LocalSvcAddresses))

    end,
    {reply, [ok], State };

handle_call({rvi_call, unregister_remote_services_by_name, [Services]}, _From, State) ->
    unregister_remote_services_by_name_(Services),
    {reply, [ok], State };

handle_call({rvi_call, unregister_local_service, [Service]}, _From, State) ->
    ?info("service_discovery_rpc:unregister_local_service(): ~p", 
	  [Service]),

    ets:delete(?LOCAL_SERVICE_TABLE, Service),
    {reply, [ok], State };

handle_call({rvi_call, resolve_remote_service, [RawService]}, _From, State) ->
    Service = rvi_common:sanitize_service_string(RawService),
    ?debug("service_discovery_rpc:resolve_remote_service(): RawService:      ~p", [RawService]),
    ?debug("service_discovery_rpc:resolve_remote_service(): Cleaned Service: ~p", [Service]),
    case resolve_service_(?REMOTE_SERVICE_TABLE, Service) of
	{ok, NetworkAddress } ->
	    {reply, [ok, NetworkAddress ], State};

	not_found ->
	    ?debug("service_discovery_rpc:resolve_remote_service(~p): Service not found in ets. "
		  "Trying static nodes",
		  [Service]),

	    
	    %% Check if this is a service residing on the backend server
	    case rvi_common:get_static_node(Service) of
		not_found -> %% Not found
		    ?info("service_discovery_rpc:resolve_remote_service(~p): Service not found.", 
			   [Service]),
		    
		    {reply, [not_found], State };

		NetworkAddress -> %% Found
		    ?debug("service_discovery_rpc:resolve_service(~p): Service is on static node ~p", 
			   [Service, NetworkAddress]),
		    
		    {reply, [ok, NetworkAddress ], State}
	    end
    end;

handle_call({rvi_call, get_remote_services, _Args}, _From, State) ->
    Services = get_services_(?REMOTE_SERVICE_TABLE),
    {reply,  [ok, Services ], State };

handle_call({rvi_call, get_all_services, _Args}, _From, State) ->
    RemoteSvc = ets:foldl(fun(#service_entry {service = ServiceName}, Acc) -> 
				  [ ServiceName  | Acc ] end, 
			  [], ?REMOTE_SERVICE_TABLE),

    LocalSvc = ets:foldl(fun(#service_entry {service = ServiceName}, Acc) -> 
				  [ ServiceName  | Acc ] end, 
			  [], ?LOCAL_SERVICE_TABLE),

    Services = RemoteSvc++LocalSvc,

    {reply,  [ok, Services], State };

handle_call({rvi_call, get_remote_network_addresses, _Args}, _From, State) ->
    Addresses =  get_network_addresses_(?REMOTE_ADDRESS_TABLE),
    {reply, [ ok, Addresses ], State };


handle_call({rvi_call, resolve_local_service, [RawService]}, _From, State) ->
    Service = rvi_common:sanitize_service_string(RawService),
    ?debug("service_discovery_rpc:resolve_local_service(): RawService:      ~p", [RawService]),
    ?debug("service_discovery_rpc:resolve_local_service(): Cleaned Service: ~p", [Service]),

    case resolve_service_(?LOCAL_SERVICE_TABLE, Service) of 
	not_found ->
	    { reply, [not_found], State };
	
	{ok, NetworkAddress } ->
	    { reply, [ok, NetworkAddress], State}
    end;


handle_call({rvi_call, get_local_services, _Args}, _From, State) ->
    Services = get_services_(?LOCAL_SERVICE_TABLE),
    {reply,  [ok,  Services ], State };

handle_call({rvi_call, get_local_network_addresses, _Args}, _From, State) ->
    Addresses = get_network_addresses_(?LOCAL_SERVICE_TABLE),
    {reply, [ ok, Addresses ], State };

handle_call(Other, _From, State) ->
    ?warning("service_discovery_rpc:handle_call(~p): unknown", [ Other ]),
    { reply,  [unknown_command] , State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


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


register_remote_service_(Service, NetworkAddress) ->
    ?info("service_discovery_rpc:register_remote_service_(): service(~p) -> ~p", 
	  [Service, NetworkAddress]),

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

    {ok, FullSvcName}.



unregister_single_remote_service_by_name_(Service) ->
    ?info("service_discovery_rpc:unregister_remote_services_by_name(): ~p", 
	  [Service]),


    %% Delete any remote address table entries with a matching Service.
    ets:match_delete(?REMOTE_ADDRESS_TABLE, 
		     #service_entry { 
			service = Service,
			network_address = '_'
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
	ets:foldl(fun(#service_entry { network_address = LocalAddress }, Acc) -> 
			  [ LocalAddress | Acc ] end, 
		  [], ?LOCAL_SERVICE_TABLE),
    
    %% Call service edge with local addresses (sorted and de-duped) and
    %% the services to register.
    service_edge_rpc:unregister_remote_services([Service], lists:usort(LocalSvcAddresses)),


    ok.


%% Loop through multiple services and remove them one by one
unregister_remote_services_by_name_(Services) ->
    [ unregister_single_remote_service_by_name_(Svc) || Svc <- Services],
    ok.



get_services_(Table) ->
    Services = ets:foldl(fun(#service_entry {service = ServiceName, 
					     network_address = ServiceAddr}, Acc) -> 
				 [ { ServiceName, ServiceAddr } | Acc ] end, 
			 [], Table),

    ?debug("service_discovery_rpc:get_services_(): ~p", [ Services ]),
    Services.


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
    ?debug("service_discovery_rpc:get_network_addresses(~p): ~p", [ Table, Addresses ]),
    Addresses.


resolve_service_(Table, Service) ->
    case ets:lookup(Table, Service) of
	%% We found a service entry, report it back
	[#service_entry { network_address = NetworkAddress }] ->
	    ?debug("service_discovery_rpc:resolve_service_(~p): service: ~p -> ~p", 
		   [ Table, Service, NetworkAddress ]),

	    {ok, NetworkAddress };

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
