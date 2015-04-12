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
	 get_services_by_module/2,
	 get_modules_by_service/2,
	 subscribe/3,
	 unsubscribe/3,
	 register_services/3,
	 unregister_services/3]).

-export([start_json_server/0]).

-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").

-define(SERVICE_TABLE, rvi_services).
-define(MODULE_TABLE, rvi_modules).
-define(NOTIFICATION_TABLE, rvi_notification_subs).

-record(service_entry, {
	  service = [],             %% Servie handled by this entry.
	  data_link_mod = undefined %% Module handling service, 'local' if local service
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
    ?debug("svc_disc:init(): called."),
    ets:new(?SERVICE_TABLE, [ duplicate_bag,  public, named_table, 
			     { keypos, #service_entry.service }]),

    ets:new(?MODULE_TABLE, [ duplicate_bag,  public, named_table, 
			     { keypos, #service_entry.data_link_mod }]),

    ets:new(?NOTIFICATION_TABLE, [set,  public, named_table, 
				  { keypos, #notification_subs.service }]),

    {ok, #st { cs = rvi_common:get_component_specification() } }.


start_json_server() ->
    rvi_common:start_json_rpc_server(service_discovery, ?MODULE, service_discovery_sup).

get_all_services(CompSpec) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_all_services, [], [status, services], CompSpec).

get_services_by_module(CompSpec, DataLinkMod) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_services_by_module, 
		       [ { data_link_module, DataLinkMod }], 
		       [status, services], CompSpec).

get_modules_by_service(CompSpec, Service) ->
    rvi_common:request(service_discovery, ?MODULE, 
		       get_modules_by_service,
		       [ { service, Service }], 
		       [status, modules], CompSpec).


register_services(CompSpec, Services, DataLinkModule) ->
    rvi_common:notification(service_discovery, ?MODULE, register_services, 
			    [{ services, Services },
			     { data_link_module, DataLinkModule }],
			    CompSpec).

unregister_services(CompSpec, Services, DataLinkModule) ->
    rvi_common:notification(service_discovery, ?MODULE, unregister_service, 
			    [{ data_link_module, DataLinkModule},
			     { services,  Services }],
			     CompSpec).

subscribe(CompSpec, Service, SubscribingMod) ->
    rvi_common:notification(service_discovery, ?MODULE, subscribe, 
			    [ { service, Service }, 
			      { subscribing_module, SubscribingMod }], 
			    CompSpec).

unsubscribe(CompSpec, Service, SubscribingMod) ->
    rvi_common:notification(service_discovery, ?MODULE, unsubscribe_from_service_availability, 
			    [ { service, Service }, 
			      { subscribing_module, SubscribingMod }], 
			    CompSpec).
    

%% JSON-RPC entry point
%% Called by local exo http server

%% Register remote services
handle_notification("register_services", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),

    gen_server:cast(?SERVER, { rvi, register_services, 
			       [ Services, DataLinkModule ]}),
    ok;


handle_notification("unregister_services", Args) ->
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, DataLinkModule } = rvi_common:get_json_element(["data_link_module"], Args),
    gen_server:cast(?SERVER, { rvi, unregister_services, 
			       [ Services, DataLinkModule ]}),
    ok;


handle_notification("subscribe", Args) ->
    {ok, Service } = rvi_common:get_json_element(["service"], Args),
    {ok, Module } = rvi_common:get_json_element(["subscribing_module"], Args),

    %% De-register service
    gen_server:cast(?SERVER, { rvi, subscribe, 
			       [ Service, Module ]}),
    ok;

handle_notification("unsubscribe_from_service", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Module } = rvi_common:get_json_element(["subscribing_module"], Args),

    %% De-register service
    gen_server:cast(?SERVER, { rvi, unsubscribe_from_service, 
			       [ Service, Module ]}),
    ok;

handle_notification( Other, _Args) ->
    ?info("svc_disc:handle_notification(~p): unknown", [ Other ]),
    ok.


%%
%% Get all services
%%
handle_rpc("get_all_services", _Args) ->
    ?debug("svc_disc:get_all_services(json-rpc)"),
    [ok, Services ] = gen_server:call(?SERVER, { rvi, get_all_services, []}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} , { services, { array, Services } }]};


handle_rpc("get_services_by_module", Args) ->
    {ok, DataLinkMod } = rvi_common:get_json_element(["data_link_module"], Args),
    ?debug("svc_disc:get_services_by_module(json-rpc): ~p ", [DataLinkMod]),
    [ok, Services ] = gen_server:call(?SERVER, 
				      { rvi, 
					get_services_by_module, 
					[DataLinkMod]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} , { services, { array, Services } }]};


handle_rpc("get_modules_by_service", Args) ->
    {ok, Service } = rvi_common:get_json_element(["service"], Args),
    ?debug("svc_disc:get_modules_by_service(json-rpc): ~p ", [Service]),
    [ok, Modules ] = gen_server:call(?SERVER, 
				      { rvi,
					get_modules_by_service,
					[Service]}),

    {ok, [ {status, rvi_common:json_rpc_status(ok)} , { modules, { array, Modules } }]};




%% 
%% Handle the rest.
%%
handle_rpc( Other, _Args) ->
    ?info("svc_disc:handle_rpc(~p): unknown", [ Other ]),
    {ok, [ { status, invalid_command } ]}.


handle_call({rvi, get_all_services, _Args}, _From, St) ->
    Svcs = ets:foldl(fun(#service_entry {service = ServiceName}, Acc) -> 
			    [ ServiceName | Acc ] end, 
		    [], ?SERVICE_TABLE),
    {reply,  [ok, Svcs], St };


handle_call({rvi, get_services_by_module, [Module]}, _From, St) ->
    {reply,  [ok, get_services_by_module_(Module)], St };


handle_call({rvi, get_modules_by_service, [Service]}, _From, St) ->
    {reply,  [ok, get_modules_by_service_(Service)], St };


handle_call(Other, _From, St) ->
    ?warning("svc_disc:handle_call(~p): unknown", [ Other ]),
    { reply,  [unknown_command] , St}.



handle_cast({rvi, subscribe, [Service, SubsMod] }, St) ->
    case ets:lookup(?NOTIFICATION_TABLE, Service) of
	[] ->
	    %% Insert new entry.
	    ets:insert(?NOTIFICATION_TABLE,
		       #notification_subs { service = Service, 
					    modules = [SubsMod] });

	[#notification_subs { modules = SubsMods }] ->
	    %% Replace existing entry
	    ets:insert(?NOTIFICATION_TABLE,
		       #notification_subs { service = Service, 
					    modules = lists:usort([ SubsMod | SubsMods ]) })
	    
    end,
    notify_on_existing_service(St#st.cs, Service, SubsMod),
    { noreply, St};


handle_cast({rvi, unsubscribe, [Service, SubsMod] }, St) ->
    case ets:lookup(?NOTIFICATION_TABLE, Service) of
	[] ->
	    %% No match.
	    ok;

	[#notification_subs { modules = SubsMods }] ->
	    %% Replace existing entry
	    ets:insert(?NOTIFICATION_TABLE,
		       #notification_subs { service = Service, 
					    modules = SubsMods -- SubsMod })
    end,
    { noreply, St};


%% Handle calls received through regular gen_server calls, routed by
%% rvi_common:request()
handle_cast({rvi, register_services, [Services, DataLinkModule] }, St) ->
    ?info("svc_disc:register_service(): ~p:~p",
	  [DataLinkModule, Services]),

    [ register_single_service_(SvcName, DataLinkModule) || SvcName <- Services],

    %% Notify all subscribers
    notify_subscribers(St#st.cs,
		       available, 
		       Services, 
		       DataLinkModule),


    {noreply, St };


%% Handle calls received through regular gen_server calls, routed by
%% rvi_common:request()
handle_cast({rvi, unregister_services, [Services, DataLinkModule] }, St) ->

    ?info("svc_disc:unregister_service(): ~p:~p",
	  [DataLinkModule, Services]),

    [ unregister_single_service_(SvcName, DataLinkModule) || SvcName <- Services],

    %% Notify all subscribers
    notify_subscribers(St#st.cs,
		       unavailable, 
		       Services, 
		       DataLinkModule),


    {noreply, St };



handle_cast(Other, St) ->
    ?warning("svc_disc:handle_cast(~p): unknown", [ Other ]),
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
%% dump_table(_Table, '$end_of_table') ->
%%     true;

%% dump_table(Table, Key) ->
%%     Val = ets:lookup(Table, Key),
%%     ?info("Table: ~p(~p) - ~p", [ Table, Key, Val ]),
%%     dump_table(Table, ets:next(Table, Key)).

%% dump_table(Table) ->
%%     dump_table(Table, ets:first(Table)).


register_single_service_(Service, DataLinkModule) ->
    ?info("svc_disc:register_remote_service_(~p:~p)", 
	  [DataLinkModule,Service]),

    %% Delete any previous instances of the given entry, in case
    %% the service registers multiple times
    ets:match_delete(?MODULE_TABLE, 
		     #service_entry { 
			service = Service, 
			data_link_mod = DataLinkModule 
		      }),

    ets:match_delete(?SERVICE_TABLE, 
		     #service_entry { 
			service = Service, 
			data_link_mod = DataLinkModule 
		      }),

    ets:insert(?SERVICE_TABLE, 
	       #service_entry {
		  service = Service,
		  data_link_mod = DataLinkModule
		 }),

    ets:insert(?MODULE_TABLE, 
	       #service_entry {
		  service = Service,
		  data_link_mod = DataLinkModule
		 }),


    ok.



unregister_single_service_(Service, DataLinkModule) ->
    ?info("svc_disc:unregister_single_service_(): ~p:~p", 
	  [DataLinkModule, Service]),


    %% Delete any service table entries with a matching Service.
    ets:match_delete(?SERVICE_TABLE, 
		     #service_entry { 
			service = Service,
			data_link_mod = DataLinkModule 
		      }),
    %% Delete any remote address table entries with a matching Service.
    ets:match_delete(?MODULE_TABLE, 
		     #service_entry { 
			service = Service,
			data_link_mod = DataLinkModule 
		      }),

    ok.

%%
%% Return all modules that can currently route the provided service.
%%
get_modules_by_service_(Service) ->

    ModMatch = ets:lookup(?SERVICE_TABLE, Service),
    
    ModNames = lists:foldl(fun(#service_entry { 
				  data_link_mod = Mod 
				 }, Acc) -> 
				   [ Mod | Acc ]
		end, [], ModMatch),
    
	


    ?debug("svc_disc:get_modules_by_service_(): ~p -> ~p", [ Service, ModNames ]),
    ModNames.



get_services_by_module_(Module) ->

    SvcMatch = ets:lookup(?MODULE_TABLE, Module),
    
    SvcNames = lists:foldl(fun(#service_entry { 
				  service = Svc 
				 }, Acc) -> 
				   [ Svc | Acc ]
		end, [], SvcMatch),
    
	


    ?debug("svc_disc:get_services_by_module_(): ~p -> ~p", [ Module, SvcNames ]),
    SvcNames.




notify_subscribers(_CompSpec, _Available, [], _DataLinkModule) -> 
    ok;

notify_subscribers(CompSpec, Available, [ Service | Rem], DataLinkModule) -> 
    %% ?NOTIFICATION_TABLE contains services that
    %% should be matched against the given service.
    %% If there is a match, we notify the associated module

    Fun = case Available of
	      available -> service_available;
	      unavailable -> service_unavailable
	  end,

    %% Retrieve all modules subscribing to a specific service.
    case ets:lookup(?NOTIFICATION_TABLE, Service) of
	[] ->
	    ok;
	
	[#notification_subs { modules = Modules }] ->
	    %% Notify each subscriber of the given service.
	    [ Module:Fun(CompSpec, Service, DataLinkModule) || Module <- Modules],
	    ok
    end,
    
    %% Move on to remaining subscribers.
    notify_subscribers(CompSpec, Available, Rem, DataLinkModule).


notify_on_existing_service(CompSpec, Service, SubsMod) ->
    Modules = ets:lookup(?SERVICE_TABLE, Service),
    [ SubsMod:service_available(CompSpec, Service, Mod) || Mod <- Modules],
    ok.
