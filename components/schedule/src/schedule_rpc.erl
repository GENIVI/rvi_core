%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(schedule_rpc).
-behaviour(gen_server).
-behaviour(rvi_schedule).

-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").
%% API
-export([start_link/0]).
-export([schedule_message/6,
	 register_remote_services/3, 
	 unregister_remote_services/2]).

%% Invoked by service discovery
%% FIXME: Should be rvi_service_discovery behavior
-export([service_available/3,
	 service_unavailable/3]

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_json_server/0]).
-define(SERVER, ?MODULE). 


-export([handle_rpc/2,
	 handle_notification/2]).


%% Message structure and storage
%%  A message is a piece of 
%%
%%  Service -> ETS -> Messages
-record(service, {
	  name = "", %% Fully qualified service name.
	  %% Set to { IP, Port} when service is available, else unknown_network_address.
	  address = unknown, 

	  %% Targeted data link module
	  data_link_module = unknown

	  %% Table containing #message records, 
	  %% indexed by their transaction ID (and sequence of delivery)
	  messages_tid =  undefined,

	  %% Component specification
	  cs = #component_spec{}

	 }).


% A single message to be delivered to a service.
% Messages are stored in ets tables hosted by a service 

-record(message, { 
	  transaction_id, %% Transaction ID that message is tagged with.
	  timeout,        %% Timeout, UTC
	  data_link,      %% Data Link Module to use. { Module, Opts}
	  protocol,       %% Protocol to use. { Module Opts }
	  routes,         %% Routes retrieved for this 
	  timeout_tref,   %% Reference to erlang timer associated with this message.
	  timeout_cb,     %% Callback to invoke when timeout occurs. 
	  parameters,
	  signature,
	  certificate
	 }).


-record(st, { 
	  next_transaction_id = 1, %% Sequentially incremented transaction id.
	  services_tid = undefined,
	  cs %% Service specification
	 }).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, St} |
%%                     {ok, St, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% Setup the relevant ets tables.
    {ok, #st{ 
	    cs = rvi_common:get_component_specification(),
	    services_tid = ets:new(rvi_schedule_services, 
				     [  set, private, 
					{ keypos, #service.name } ])}}.

start_json_server() ->
    rvi_common:start_json_rpc_server(schedule, ?MODULE, schedule_sup).

schedule_message(CompSpec, 
		 SvcName, 
		 Timeout, 
		 Parameters,
		 Signature,
		 Certificate) ->
    
    rvi_common:request(schedule, ?MODULE, 
		       schedule_message, 
		       [{ service, SvcName }, 
			{ timeout, Timeout },
			{ parameters, Parameters }, 
			{ signature, Signature }, 
			{ certificate, Certificate }], 
		       [status, transaction_id], CompSpec).


register_remote_services(CompSpec, NetworkAddress, Services) ->
    rvi_common:notification(schedule, ?MODULE, 
			    register_remote_services, 
			    [{ network_address, NetworkAddress} ,
			     { services, Services }],
			    CompSpec).


unregister_remote_services(CompSpec, ServiceNames) ->
    rvi_common:notification(schedule, ?MODULE, 
			    unregister_remote_services, 
			    [{ services, ServiceNames }],
			    CompSpec).


service_available(Service, DataLinkModule, Address) ->
    rvi_common:notification(schedule, ?MODULE, 
			    service_available, 
			    [{ service, Service },
			     { data_link_module, DataLinkModule },
			     { address, Address }],
			    CompSpec).

service_unavailable(Service, DataLinkModule, Address) ->
    rvi_common:notification(schedule, ?MODULE, 
			    service_unavailable, 
			    [{ service, Service },
			     { data_link_module, DataLinkModule },
			     { address, Address }],
			    CompSpec).

%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc("schedule_message", Args) ->

    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, Parameters} = rvi_common:get_json_element(["parameters"], Args),
    {ok, Signature} = rvi_common:get_json_element(["signature"], Args),
    {ok, Certificate} = rvi_common:get_json_element(["certificate"], Args),

    ?debug("schedule_rpc:schedule_request(): service:     ~p", [ SvcName]),
    ?debug("schedule_rpc:schedule_request(): timeout:     ~p", [ Timeout]),
%%    ?debug("schedule_rpc:schedule_request(): parameters:      ~p", [Parameters]),
    ?debug("schedule_rpc:schedule_request(): signature:   ~p", [Signature]),
    ?debug("schedule_rpc:schedule_request(): certificate: ~p", [Certificate]),

    [ok, TransID] = gen_server:call(?SERVER, { rvi, schedule_message, 
					       [ SvcName,
						 Timeout,
						 Parameters,
						 Signature,
						 Certificate]}),

    {ok, [ { status, rvi_common:json_rpc_status(ok)},
	   { transaction_id, TransID } ] };




handle_rpc(Other, _Args) ->
    ?debug("schedule_rpc:handle_rpc(~p): unknown", [ Other ]),
    {ok, [ {status, rvi_common:json_rpc_status(invalid_command)}]}.


handle_notification("register_remote_services", Args) ->
    {ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    ?debug("schedule_notification:register_remote_services(): network_address: ~p", [ NetworkAddress]),
    ?debug("schedule_notification:register_remote_services(): services:        ~p", [ Services]),

    gen_server:cast(?SERVER, { rvi, register_remote_services, 
				      [ NetworkAddress,
					Services ]}),

    ok;

handle_notification("unregister_remote_services", Args) ->
    {ok,  DiscountinuedServices} = rvi_common:get_json_element(["services"], Args),
    ?debug("schedule_notification:unregister_remote_services(): services         ~p",
	   [ DiscountinuedServices]),

    gen_server:cast(?SERVER, { rvi, unregister_remote_services, 
			       [ DiscountinuedServices ]}),
    ok;

handle_notification("service_available", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),
    {ok, Address} = rvi_common:get_json_element(["address"], Args),

    gen_server:cast(?SERVER, { rvi, service_available, 
				      [ Service,
					DataLinkModule,
					Address ]}),

    ok;
handle_notification("service_unavailable", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_module"], Args),
    {ok, Address} = rvi_common:get_json_element(["address"], Args),

    gen_server:cast(?SERVER, { rvi, service_unavailable, 
				      [ Service,
					DataLinkModule,
					Address ]}),

    ok;

handle_notification(Other, _Args) ->
    ?debug("schedule_notification:handle_other(~p): unknown", [ Other ]),
    ok.

handle_call( { rvi, schedule_message,
	       [SvcName, 
		Timeout, 
		Parameters,
		Signature,
		Certificate] }, _From, St) ->

    ?debug("schedule:sched_msg(): service:     ~p", [SvcName]),
    ?debug("schedule:sched_msg(): timeout:     ~p", [Timeout]),
    ?debug("schedule:sched_msg(): parameters:  ~p", [Parameters]),
    ?debug("schedule:sched_msg(): signature:   ~p", [Signature]),
    ?debug("schedule:sched_msg(): certificate  ~p", [Certificate]),
    ?debug("schedule:sched_msg(): St:          ~p", [St]),

    %%
    %% Retrieve the routes that we should try for this message
    %%
    case rvi_routing:get_service_routing(SvcName) of 
	not_found -> 
	    {reply, [ no_route ], St };

	Routes ->
	    { NewTransID, NSt1} = create_transaction_id(St),
	    NTS2 = queue_message(SvcName, Routes, Timeout,  Parameter, Signature); 

	    { reply, [ok, TransID], NS2 }
    end;


handle_call(Other, _From, St) ->
    ?warning("schedule:handle_call(~p): unknown", [ Other ]),
    { reply,  [unknown_command] , St}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, St) -> {noreply, St} |
%%                                  {noreply, St, Timeout} |
%%                                  {stop, Reason, St}
%% @end
%%--------------------------------------------------------------------

handle_cast( {rvi, register_remote_services, 
	       [ NetworkAddress, Services]}, St) ->

    ?info("schedule:register_remote_services(): services(~p) -> ~p", 
	  [Services, NetworkAddress]),

    {ok, NSt} = multiple_services_available(Services, NetworkAddress, St),
    {noreply, NSt};



handle_cast( {rvi, unregister_remote_services, [ServiceNames]}, St) ->
    ?info("schedule:unregister_remote_services(): Services(~p)", [ServiceNames]),
    {ok, NSt} =  multiple_services_unavailable(ServiceNames, St),
    {noreply, NSt };

handle_cast( {rvi, service_available, [Service, DataLinkModule, Address]}, St) ->
    %% Find the service
    case ets:lookup(SvcTid, {Service, DataLinkModule}) of
	[] ->  %% No service found - Just unsubscribe. Shouldn't really happen
	    service_discovery_rpc:
		unsubscribe_to_service_availability({Service, DataLinkModule}, 
						    ?MODULE);

	    { noreply, St };

	[ Svc ] -> 
	    %% Update the network address, if it differs, and return
	    %% the new service / State as {ok, NSvc, false, NSt}
	    ?debug("schedule:service_unavailable(): Service ~p:~p now has address~p.", 
		   [ DataLinkModule, Service, Address ]),
	    update_service_network_address(Svc, Address, St),
	    { _, NSt2 } = try_sending_messages(Svc, NSt1),
	    { noreply, NSt2}
    end.

handle_cast( {rvi, service_unavailable, [Service, DataLinkModule, Address]}, 
	    #st { services_tid = SvcTid } = St) ->

    %% Find the service
    case ets:lookup(SvcTid, {Service, DataLinkModule}) of
	[] ->  %% No service found - no op.
	    {noreply, St};

	[ Svc ] -> 
	    %% Delete service if it is unused. 
	    case delete_unused_service(SvcTid, Svc) of
		true ->  %% service was deleted

		    %% Unsubscribe from service availablility notifications
		    service_discovery_rpc:
			unsubscribe_to_service_availability({ Service, DataLinkModule}, 
							    ?MODULE);

		    { noreply, St};

		false -> %% Service was not deleted, update its network address to unknown
		    { _, _, _, NSt} = 
			update_service_network_address(Svc, unknown_network_address, St),
		
		    { noreply, NSt }
	    end

    end.
    { noreply, St};


handle_cast(Other, St) ->
    ?warning("schedule:handle_cast(~p): unknown", [ Other ]),
    {noreply, St}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, St) -> {noreply, St} |
%%                                   {noreply, St, Timeout} |
%%                                   {stop, Reason, St}
%% @end
%%--------------------------------------------------------------------

%% Handle timeouts
handle_info({ rvi_message_timeout, SvcName, DLMod, TransID}, 
	    #st { cs = CompSpec, services_tid = SvcTid } = St) ->

    case  ets:lookup(SvcTid, {SvcName, DLMod}) of
	[ Svc ] ->
	    %% Delete from ets.
	    case ets:lookup(Svc#service.messages_tid, TransID) of
		[ Msg ] ->
		    ?info("schedule:timeout(): trans_id(~p) service(~p)", [ TransID, SvcName]),
		    ets:delete(Svc#service.messages_tid, TransID),
		    queue_message(
		_ -> 
		    ?info("schedule:timeout(): trans_id(~p) service(~p): Yanked while processing", 
			  [ TransID, SvcName]),
		    ok
	    end;
	_-> ok
    end,
    {noreply, St};


handle_info(_Info, St) ->
    {noreply, St}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, St) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _St) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process st when code is changed
%%
%% @spec code_change(OldVsn, St, Extra) -> {ok, NewSt}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



%% No more routes to try
queue_message(_SvcName, 
	      _TransID, 
	      _MessageTimeout
	      [  ],
	      _Parameters, 
	      _Signature,
	      _Certificate, St) ->

    %% FIXME: Handle route failure
    { route_failed, St };


queue_message(SvcName, 
	      TransID, 
	      [ { { Pr, PrOp }, { DL, DLOp } }  | RemainingRoutes ],
	      MessageTimeout, 
	      Parameters, 
	      Signature,
	      Certificate, 
	      St) ->
%%    ?info("schedule:sched_msg(): service(~p)    timeout(~p)", [SvcName, Timeout]),
%%    ?info("schedule:sched_msg(): timeout (~p)", [Timeout]),
%%    ?info("schedule:sched_msg(): parameters:   ~p", [Parameters]),
%%    ?info("schedule:sched_msg(): signature:    ~p", [Signature]),
%%    ?info("schedule:sched_msg(): certificate:  ~p", [Certificate]),

    %% Calculate how many msec we have until message times out
    
    RelativeTimeout = calc_relative_tout(MessageTimeout),

    %% Did we run out of time for the message?
    case RelativeTimeout > 1 of
	%% Nope
	true ->
	    {ok, Service, IsNewService, NSt1 } = find_or_create_service({SvcName, DL}, St),

	    Msg = #message { 
		     service = SvcName,
		     transaction_id = TransID,
		     data_link = { DLMod, DLOp },
		     routes = RemaingRoutes,
		     message_timeout = MessageTimeout,
		     route_timeout_ref = TRef,
		     parameters = Parameters, 
		     signature = Signature,
		     certificate = Certificate
		    },


	    %% Add to ets table
	    TransID = ets:insert(Service#service.messages_tid, Msg),

	    %% If this is a new service, subscribe to updates on this service's availablity.
	    %% If service is already available, we will get service_available/3 notification
	    %% immediately.
	    case IsNewService of
		true ->
		    service_discovery_rpc:
			subscribe_to_service_availability({SvcName, DLMod}, 
							  ?MODULE);
		false ->
		    ok
	    end,


	    %%
	    %% Bring up the relevant data link for the given route.
	    %% Once up, the data link will invoke service_availble()
	    %% to indicate that the service is available for the given DL.
	    %% 
	    case DLMod:setup_data_link(CompSpec, SvcName, DLOp) of
		{ ok, DLTimeout } ->
		    %% Setup a timeout to be 
		    TRef = erlang:send_after(min(RelativeMessageTimeout, DLTimeout),
					     self(), 
					     { rvi_message_timeout, SvcName, DLMod, TransID });
		
		%% We failed with this route. Try the next one
		{ error, _Reason} ->
		    queue_message(SvcName,
				  TransID,
				  RelativeTimeout, 
				  RemainingRoutes,
				  Parameters,
				  Signature,
				  Certificate, 
				  St)
	    end;

	%% We are out of time
	false ->
	    do_timeout_callback(CompSpec, Svc, Msg),
	    { timeout, St}
    end.



forward_to_protocol(Svc, Msg, St) ->
    { DataLinkMod, DataLinkOpts } = Msg#message.data_link,
    { ProtoMod, ProtoOpts } = Msg#messge.protocol,
    
    case ProtoMod:send_message(
	   St#st.cs,
	   SvcName,
	   Msg#message.timeout,
	   ProtoOpts,
	   DataLinkMod,
	   DataLinkOpts,
	   NetworkAddress,
	   Msg#message.parameters,
	   Msg#message.signature,
	   Msg#message.certificate) of
	
	%% Success
	[ok] -> 
	    %% Send the rest.
	    try_sending_messages(Service, St);
	
	%% Failed
	[Err] ->
	    ?info("schedule:try_send(): No send: ~p:~p:~p -> ~p : ~p", 
		  [ProtocolMod, DataLinkMod, SvcName, NetworkAddress, Err]),

	    %% Requeue this message with the next route
	    { _, St1} = queue_message(SvcName,
				      TransID,
				      Msg#message.timeout, 
				      Msg#message.routes,
				      Msg#message.parameters,
				      Msg#message.signature,
				      Msg#message.certificate,
				      St),

	    %% Send the rest of the messgages targeting 
	    %% the same service through the same data link
	    try_sending_messages(Svc, St)
    end.
    


    
%% Check if we can send messages queued up under the given service.
try_sending_messages(#service { 
			name = SvcName,
			network_address = unknown_network_address,
			messages_tid = _Tid } = _Service, St) ->
    ?info("schedule:try_send(): SvcName:   ~p: Not available", [SvcName]),
    {not_available, St};

try_sending_messages(#service { 
			name = SvcName,
			data_link_module = DataLinkModule,
			network_address = NetworkAddress,
			messages_tid = Tid } = Service, St) ->

    ?debug("schedule:try_send(): SvcName:         ~p", [SvcName]),
    ?debug("schedule:try_send(): Network Address: ~p", [NetworkAddress]),

    %% Extract the first message of the queue.
    case ets:first(Tid) of
	%% No more messages to send.
	'$end_of_table' ->
	    ?debug("schedule:try_send(): Nothing to send"),
	    { ok, St };

	Key ->
	    St1 = forward_to_protocol
	    ?debug("schedule:try_send(): Sending: ~p", [Key]),
	    %% Extract first message and try to send it
	    case ets:lookup(Tid, Key) of
		[ Msg ] ->
		    %% Wipe from ets table and cancel timer
		    ets:delete(Tid, Key),
		    erlang:cancel_timer(Msg#message.timeout_tref),

		    %% Forward to protocol and resend
		    forward_to_protocol(Service, Msg, St);

		_ -> 
		    ?info("schedule:try_send(): Message was yanked while trying to send: ~p", [Key]),
		    { ok, St}

	    end
    end.


%%
%% data_link_up has reported that multiple services are now
%% available at NetworkAddress. 
%% Iterate through all services and mark them as available,
%% possibly sending any pending message targeting the newly activated
%% service.
%%
multiple_services_available([], _NetworkAddress, St) ->
    {ok, St};

multiple_services_available([ Svc | T], NetworkAddress, St) ->
    {ok, NSt} = service_available(Svc, NetworkAddress, St),
    multiple_services_available(T, NetworkAddress, NSt).

multiple_services_unavailable([], St) ->
    {ok, St};

multiple_services_unavailable([ SvcName | T], St) ->
    {ok, NSt} = service_unavailable(SvcName, St),
    multiple_services_unavailable(T, NSt).


find_or_create_service(ServiceName, St) ->
    %% Invoke with retain to keep any exising network addresses already
    %% in place in an existing service.
    find_or_create_service(ServiceName, retain_existing_address, St).

find_or_create_service(ServiceName, NetworkAddress, #st { services_tid = SvcTid } = St) ->
    ?debug("schedule:find_or_create_service(): SvcName: ~p", [ ServiceName]),

    case ets:lookup(SvcTid, ServiceName) of
	[] ->  %% The given service does not exist, create it.
	    ?debug("schedule:find_or_create_service(): Creating new ~p", [ ServiceName]),
	    create_service(ServiceName, NetworkAddress, St);

	[ Svc1 ] when NetworkAddress =:= retain_existing_address -> 
	    %% We found a service, and are instructed not to touch
	    %% its network address. Return it.
	    { ok, Svc1, false, St};

	[ Svc2 ] -> 
	    %% Update the network address, if it differs, and return
	    %% the new service / State as {ok, NSvc, false, NSt}
	    ?debug("schedule:find_or_create_service(): Updating existing ~p", [ ServiceName]),
	    update_service_network_address(Svc2, NetworkAddress, St)
    end.

%%
%% Catch where no network address update is necessary.
%%
update_service_network_address(#service { network_address = NetworkAddress } = Service,
			       NetworkAddress, St) ->
    { ok, Service, false, St}; %% False indicates that the service exists.


%% 
%% Update the service in the ets table with a new network address.
%% 
update_service_network_address(#service {} = Service, NetworkAddress, St) ->
    %% Create a new service.
    NewService = Service#service { network_address = NetworkAddress},

    %% Replace existing serviceo in the ets table of services.
    ets:insert(St#st.services_tid, [ NewService ]),

    { ok, NewService, false, St}. %% False indicates that the service exists.




%% If we are creating a new service, we need to remap the
%% retain_existing_address to unknown_network_address in order to get the correct
%% initial state, which is a created service with an unknown network address.
create_service(ServiceName, retain_existing_address,  St) ->
    create_service(ServiceName, unknown_network_address, St);


%% Create a new service and return the new state (not currently modified)
%% and the newly initialized service revord.
%%
create_service(ServiceName, NetworkAddress, #st { services_tid = SvcsTid } = St) ->
    Svc = #service { 
	     name = ServiceName,
	     network_address = NetworkAddress,
	     messages_tid = ets:new(rvi_messages, 
				    [ ordered_set, private, 
				      { keypos, #message.transaction_id } ])
	    },

    %% Insert new service to ets table.
    ets:insert(SvcsTid, Svc),
    
    %% Return new service and existing state.
    ?debug("schedule:create_service():  service(~p) -> NetworkAddress(~p)", [ ServiceName, NetworkAddress]),
    ?debug("schedule:create_service():  MessageTID: ~p", [ Svc#service.messages_tid]),
    { ok, Svc, true, St}.  %% True indicates that the service is newly created.


%% Create a new and unique transaction id
create_transaction_id(St) ->
    ?debug("schedule:create_transaction_id(): St:     ~p", [  St ]),
    ID = St#st.next_transaction_id,

    %% FIXME: Maybe interate pid into transaction to handle multiple
    %% schedulers?
    { ID, St#st { next_transaction_id = ID + 1 }}.

%% Calculate a relative timeout based on the UnixTime TS we are provided with.
calc_relative_tout(UnixTime) ->
    { Mega, Sec, _Micro } = now(),
    Now = Mega * 1000000 + Sec,
    ?debug("schedule:calc_relative_tout(): Timeout(~p) - Now(~p) = ~p", [ UnixTime, Now, UnixTime - Now ]),

    %% Cap the timeout value at something reasonable
    TOut = 
	case UnixTime - Now >= 4294967295 of
	    true -> 
		?info("schedule:calc_relative_tout(): Timeout(~p) - Now(~p) = ~p: "
		      "Truncated to 4294967295", [ UnixTime, Now, UnixTime - Now ]),
		4294967295;

	false -> UnixTime - Now
    end,

    case TOut =< 0 of
	true ->
	    1; %% One millisec is the smallest value we will time out on

	false ->
	    TOut * 1000 
    end.

%% Handle a callback for a timed out message.

do_timeout_callback(CompSpec, Service, 
		    #message {transaction_id = TransID}) ->
    service_edge_rpc:handle_local_timeout(CompSpec, Service, TransID),
    ok;


%% callback element of #message is not an {M,F,A} format, ignore.
do_timeout_callback(_,_,_) ->
    ok.

%% Kill off a service that is no longer used.
%%
delete_unused_service(SvcTid, Svc) ->
    %% Do we have messages waiting for this service?
    case ets:first(Svc#service.messages_tid) of
	%% Nope.
	'$end_of_table' ->
	    ets:delete(Svc#messages_tid),
	    ets:delete(SvcTid, { Service, DataLinkModule})
	    %% Update the network address, if it differs, and return
	    %% the new service / State as {ok, NSvc, false, NSt}
	    ?debug("schedule:service_unavailable(): Service ~p:~p now has no address.", 
		   [ DataLinkModule, Service ]),
	    true;

	_ -> false
    end.
