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
	 is_service_available/2,
	 subscribe/2,
	 unsubscribe/2,
	 register_services/3,
	 unregister_services/3]).

-export([start_json_server/0]).

-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").

-define(SERVICE_TABLE, rvi_svcdisc_services).
-define(MODULE_TABLE, rvi_svcdisc_modules).
-define(SUBSCRIBER_TABLE, rvi_svcdisc_subscribers).

-record(service_entry, {
	  service = [],             %% Servie handled by this entry.
	  data_link_mod = undefined %% Module handling service, 'local' if local service
	 }).


-record(subscriber_entry, {
	  module %% Module subscribing to service availability
	 }).


-define(SERVER, ?MODULE).

-record(st, {
	  %% Component specification
	  cs = #component_spec{}
	 }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?info("svc_disc:init(): called."),
    ets:new(?SERVICE_TABLE, [ duplicate_bag,  public, named_table,
			     { keypos, #service_entry.service }]),

    ets:new(?MODULE_TABLE, [ duplicate_bag,  public, named_table,
			     { keypos, #service_entry.data_link_mod }]),

    ets:new(?SUBSCRIBER_TABLE, [set,  public, named_table,
				  { keypos, #subscriber_entry.module }]),

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

is_service_available(CompSpec, Service) ->
    rvi_common:request(service_discovery, ?MODULE,
		       is_service_available,
		       [ { service, Service }],
		       [status, result], CompSpec).

register_services(CompSpec, Services, DataLinkModule) ->
    ?debug("register_services(Mod=~p): ~p", [DataLinkModule, Services]),
    rvi_common:notification(service_discovery, ?MODULE, register_services,
			    [{ services, Services },
			     { data_link_module, DataLinkModule }],
			    CompSpec).

unregister_services(CompSpec, Services, DataLinkModule) ->
    ?debug("unregister_services(Mod=~p): ~p", [DataLinkModule, Services]),
    rvi_common:notification(service_discovery, ?MODULE, unregister_services,
			    [{ services,  Services },
			     { data_link_module, DataLinkModule}],
			     CompSpec).

subscribe(CompSpec, SubscribingMod) ->
    rvi_common:notification(service_discovery, ?MODULE, subscribe,
			    [ { subscribing_module, SubscribingMod }],
			    CompSpec).

unsubscribe(CompSpec, SubscribingMod) ->
    rvi_common:notification(service_discovery, ?MODULE, unsubscribe,
			    [ { subscribing_module, SubscribingMod }],
			    CompSpec).


%% JSON-RPC entry point
%% Called by local exo http server

%% Register remote services
handle_notification(<<"register_services">>, Args) ->
    LogId = rvi_common:get_json_log_id(Args),
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),

    gen_server:cast(?SERVER, { rvi, register_services,
			       [ Services, list_to_atom(DataLinkModule), LogId ]}),
    ok;


handle_notification(<<"unregister_services">>, Args) ->
    LogId = rvi_common:get_json_log_id(Args),
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, DataLinkModule } = rvi_common:get_json_element(["data_link_module"], Args),
    gen_server:cast(?SERVER, { rvi, unregister_services,
			       [ Services, list_to_atom(DataLinkModule), LogId ]}),
    ok;


handle_notification(<<"subscribe">>, Args) ->
    LogId = rvi_common:get_json_log_id(Args),
    {ok, Module } = rvi_common:get_json_element(["subscribing_module"], Args),

    %% De-register service
    gen_server:cast(?SERVER, { rvi, subscribe, [ list_to_atom(Module), LogId ]}),
    ok;

handle_notification(<<"unsubscribe_from_service">>, Args) ->
    LogId = rvi_common:get_json_log_id(Args),
    {ok, Module } = rvi_common:get_json_element(["subscribing_module"], Args),

    %% De-register service
    gen_server:cast(?SERVER, { rvi, unsubscribe, [ list_to_atom(Module), LogId ]}),
    ok;

handle_notification( Other, _Args) ->
    ?info("svc_disc:handle_notification(~p): unknown", [ Other ]),
    ok.


%%
%% Get all services
%%
handle_rpc(<<"get_all_services">>, Args) ->
    ?debug("svc_disc:get_all_services(json-rpc)"),
    LogId = rvi_common:get_json_log_id(Args),
    [ok, Services ] = gen_server:call(?SERVER, { rvi, get_all_services, [LogId]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} , { services, Services }]};


handle_rpc(<<"get_services_by_module">>, Args) ->
    LogId = rvi_common:get_json_log_id(Args),
    {ok, DataLinkMod } = rvi_common:get_json_element(["data_link_module"], Args),
    ?debug("svc_disc:get_services_by_module(json-rpc): ~p ", [DataLinkMod]),
    [ok, Services ] = gen_server:call(?SERVER,
				      { rvi,
					get_services_by_module,
					[DataLinkMod, LogId]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} , { services, { array, Services } }]};


handle_rpc(<<"get_modules_by_service">>, Args) ->
    LogId = rvi_common:get_json_log_id(Args),
    {ok, Service } = rvi_common:get_json_element(["service"], Args),
    ?debug("get_modules_by_service(json-rpc): ~p ", [Service]),
    [ok, Modules ] = gen_server:call(?SERVER,
				      { rvi,
					get_modules_by_service,
					[Service, LogId]}),

    {ok, [ {status, rvi_common:json_rpc_status(ok)} , { modules, { array, Modules } }]};

handle_rpc(<<"is_service_available">>, Args) ->
    LogId = rvi_common:get_json_log_id(Args),
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    ?debug("service_availability(json-rpc): ~p ", [Service]),
    [ok, Avail] = gen_server:call(
		    ?SERVER,
		    {rvi, is_service_available, [Service, LogId]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)},
	   {result, Avail}]};

%%
%% Handle the rest.
%%
handle_rpc( Other, _Args) ->
    ?info("svc_disc:handle_rpc(~p): unknown", [ Other ]),
    {ok, [ { status, invalid_command } ]}.

handle_call(Req, From, St) ->
    try handle_call_(Req, From, St)
    catch
	error:Reason ->
	    ?debug("~p:handle_call_(~p,~p,~p) -> ERROR: ~p~n~p",
		   [?MODULE, Req, From, St, Reason,
		    erlang:get_stacktrace()]),
	    {reply, [internal_error], St}
    end.

handle_call_({rvi, get_all_services, _Args}, _From, St) ->
    Svcs = ets:foldl(fun(#service_entry {service = ServiceName}, Acc) ->
			    [ ServiceName | Acc ] end,
		    [], ?SERVICE_TABLE),
    {reply,  [ok, Svcs], St };


handle_call_({rvi, get_services_by_module, [Module | _LogId]}, _From, St) ->
    {reply,  [ok, get_services_by_module_(Module)], St };


handle_call_({rvi, get_modules_by_service, [Service | _LogId]}, _From, St) ->
    {reply,  [ok, get_modules_by_service_(Service)], St };

handle_call_({rvi, is_service_available, [Service | _LogId]}, _From, St) ->
    {reply, [ok, ets:member(?SERVICE_TABLE, Service)], St};

handle_call_(Other, _From, St) ->
    ?warning("svc_disc:handle_call(~p): unknown", [ Other ]),
    { reply,  [unknown_command] , St}.



handle_cast({rvi, subscribe, [ SubsMod | _LogId ] }, St) ->
    %% Insert new entry, or replace existing one
    ets:insert(?SUBSCRIBER_TABLE, #subscriber_entry { module = SubsMod}),

    initial_notification(St#st.cs, SubsMod),

    { noreply, St};


handle_cast({rvi, unsubscribe, [ SubsMod | _LogId ] }, St) ->
    ets:delete(?SUBSCRIBER_TABLE, SubsMod),
    { noreply, St};


%% Handle calls received through regular gen_server calls, routed by
%% rvi_common:request()
handle_cast({rvi, register_services, [Services, DataLinkModule | _LogId ] }, St) ->
    ?info("svc_disc:register_services(): ~p:~p",
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
handle_cast({rvi, unregister_services, [Services, DataLinkModule | _LogId ] }, St) ->

    ?info("svc_disc:unregister_services(): ~p:~p",
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


register_single_service_(Service, DataLinkModule) ->
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



notify_single_subscriber(_CompSpec, '$end_of_table', _SubsFun,
			 _DataLinkModule, _Services) ->
    ok;

notify_single_subscriber(CompSpec, SubsModule, SubsFun,
			 DataLinkModule, Services) ->

    %% Invoke subscriber for each service that has been updated.
    ?debug("notify_single_subscriber(~p:~p) ~p:~p()",
	   [SubsModule, SubsFun, DataLinkModule, Services ]),
    [ SubsModule:SubsFun(CompSpec, SvcName, DataLinkModule) || SvcName <- Services],

    %% Move on to the next subscribing module
    notify_single_subscriber(CompSpec,
			     ets:next(?SUBSCRIBER_TABLE, SubsModule), SubsFun,
			     DataLinkModule, Services).

notify_subscribers(CompSpec, Available, Services, DataLinkModule) ->

    ?debug("notify_subscribers(~p:~p) ~p", [ DataLinkModule, Services, Available]),

    %% Figure out the function to invoke
    Fun = case Available of
	      available -> service_available;
	      unavailable -> service_unavailable
	  end,


    ets:foldl(
      %% Notify if this is not the originating service.
      fun(#subscriber_entry { module = Module }, _Acc) ->
	      ?debug("  notify_subscribers module: ~p ", [ Module]),
	      ok
      end, ok, ?SUBSCRIBER_TABLE),

    %% Initiate with the first module
    notify_single_subscriber(CompSpec,
			     ets:first(?SUBSCRIBER_TABLE),
			     Fun,
			     DataLinkModule,
			     Services).



initial_notification(_CompSpec, _SubsMod, '$end_of_table') ->
    ok;

%% Send all available services to the newly subscribing module
initial_notification(CompSpec, SubsMod, Service) ->
    case ets:lookup(?SERVICE_TABLE, Service) of
	[] -> %% Yanked
	    ok;

	[#service_entry { data_link_mod = DataLinkMod }] ->
	    SubsMod:service_available(CompSpec, Service, DataLinkMod)
    end,

    %% Move on to the next service
    initial_notification(CompSpec, SubsMod,
			 ets:next(?SERVICE_TABLE, Service)).


initial_notification(CompSpec, SubsMod) ->
    initial_notification(CompSpec, SubsMod, ets:first(?SERVICE_TABLE)),
    ok.

log([ID], Fmt, Args) ->
    rvi_log:log(ID, <<"authorize">>, rvi_log:format(Fmt, Args));
log(_, _, _) ->
    ok.
