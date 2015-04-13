%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(schedule_rpc).
-behaviour(gen_server).

-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").
%% API
-export([start_link/0]).
-export([schedule_message/6]).

%% Invoked by service discovery
%% FIXME: Should be rvi_service_discovery behavior
-export([service_available/3,
	 service_unavailable/3]).

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
	  key = { "", unknown }, %% Service name and data link modu

	  %% Is this service currently available on the data link module
	  available = false,

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
    %% Unsubscribe from service availablility notifications
    CS = rvi_common:get_component_specification(),

    service_discovery_rpc:subscribe(CS, ?MODULE),
    {ok, #st{ 
	    cs = CS,
	    services_tid = ets:new(rvi_schedule_services, 
				     [  set, private, 
					{ keypos, #service.key } ])}}.

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



service_available(CompSpec, SvcName, DataLinkModule) ->
    rvi_common:notification(schedule, ?MODULE, 
			    service_available, 
			    [{ service, SvcName },
			     { data_link_mod, DataLinkModule } ],
			    CompSpec).

service_unavailable(CompSpec, SvcName, DataLinkModule) ->
    rvi_common:notification(schedule, ?MODULE, 
			    service_unavailable, 
			    [{ service, SvcName },
			     { data_link_mod, DataLinkModule } ],
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



handle_notification("service_available", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_mod"], Args),

    gen_server:cast(?SERVER, { rvi, service_available, 
				      [ SvcName,
					list_to_atom(DataLinkModule) ]}),
    ok;

handle_notification("service_unavailable", Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    {ok, DataLinkModule} = rvi_common:get_json_element(["data_link_mod"], Args),

    gen_server:cast(?SERVER, { rvi, service_unavailable, 
				      [ SvcName,
					list_to_atom(DataLinkModule) ]}),

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

    ?debug("sched:sched_msg(): service:     ~p", [SvcName]),
    ?debug("sched:sched_msg(): timeout:     ~p", [Timeout]),
    ?debug("sched:sched_msg(): parameters:  ~p", [Parameters]),
    ?debug("sched:sched_msg(): signature:   ~p", [Signature]),
    ?debug("sched:sched_msg(): certificate  ~p", [Certificate]),
    %%?debug("sched:sched_msg(): St:          ~p", [St]),

    %% Create a transaction ID
    { TransID, NSt1} = create_transaction_id(St),

    %% Queue the message
    {_, NSt2 }= queue_message(SvcName, 
			      TransID, 
			      rvi_routing:get_service_routes(SvcName), %% Can be no_route
			      Timeout, 
			      calc_relative_tout(Timeout),
			      Parameters, 
			      Signature, 
			      Certificate,
			      NSt1),

    { reply, [ok, TransID], NSt2 };



handle_call(Other, _From, St) ->
    ?warning("sched:handle_call(~p): unknown", [ Other ]),
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


handle_cast( {rvi, service_available, [SvcName, DataLinkModule]}, St) ->

    %% Find or create the service.
    ?debug("sched:service_available(): ~p:~p", [ DataLinkModule, SvcName ]),
    
    %% Create a new or update an existing service.
    SvcRec = update_service(SvcName, DataLinkModule, available, St),

    %% Try to send any pending messages.
    { _, NSt} = try_sending_messages(SvcRec, St),

    { noreply, NSt};

handle_cast( {rvi, service_unavailable, [SvcName, DataLinkModule]}, 
	    #st { services_tid = SvcTid } = St) ->

    %% Grab the service
    case ets:lookup(SvcTid, {SvcName, DataLinkModule}) of
	[] ->  %% No service found - no op.
	    {noreply, St};

	[ SvcRec ] -> 
	    %% Delete service if it does not have any pending messages.
	    case delete_unused_service(SvcTid, SvcRec) of
		true ->  %% service was deleted
		    { noreply, St};

		false -> %% SvcName was not deleted, set it to not available
		    update_service(SvcName, DataLinkModule, unavailable, St),

		    { noreply, St }
	    end

    end;


handle_cast(Other, St) ->
    ?warning("sched:handle_cast(~p): unknown", [ Other ]),
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
handle_info({ rvi_message_timeout, SvcName, DLMod,TransID}, 
	    #st { services_tid = SvcTid } = St) ->

    ?info("sched:timeout(~p:~p): trans_id: ~p", [ DLMod, SvcName, TransID]),
    %% Look up the service / DataLink mod
    case  ets:lookup(SvcTid, {SvcName, DLMod}) of
	[ SvcRec ] -> %% Found service for specific data link

	    %% Delete message from service queue
	    case ets:lookup(SvcRec#service.messages_tid, TransID) of
		[ Msg ] ->
		    ?debug("sched:timeout(~p:~p): Rescheduling", [ DLMod, SvcName]),
		    ets:delete(SvcRec#service.messages_tid, TransID),

		    %% Try to requeue message
		    { _Res, NSt } = 
			queue_message(SvcName, 
				      Msg#message.transaction_id, 
				      Msg#message.routes,
				      Msg#message.timeout,
				      calc_relative_tout(Msg#message.timeout),
				      Msg#message.parameters,
				      Msg#message.signature,
				      Msg#message.certificate,
				      St),
		    {noreply, NSt};

		_ -> 
		    ?info("sched:timeout(): trans_id(~p) service(~p): Yanked while processing", 
			  [ TransID, SvcName]),

		    {noreply, St}

	    end;
	_ ->
	    ?debug("sched:timeout(~p:~p): Unknown service", [ DLMod, SvcName]),
	    {noreply, St}

    end;


handle_info(Info, St) ->
    ?notice("sched:handle_info(): Unknown: ~p", [Info]),

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





%%
%% The message has timed out
%%
queue_message(SvcName, 
	      TransID, 
	      _Routes,
	      _MessageTimeout, 
	      -1, %% We are timed out
	      _Parameters, 
	      _Signature,
	      _Certificate, 
	      St) ->
    do_timeout_callback(St#st.cs, SvcName, TransID),
    { timeout, St };

%%
%% No more routes to try, or no routes found at all
%% by rvi_routing:get_service_routes()
%%
%% Stash the message in the unknown datalinvariant of the service
%% and opportunistically send it if the service
queue_message(SvcName, 
	      _TransID, 
	      [ ],
	      _MessageTimeout,
	      _RelativeTimeout,
	      _Parameters, 
	      _Signature,
	      _Certificate, St) ->

    %% FIXME: Handle route failure
    ?notice("sched:queue_message(): Ran out of routes to try for ~p", [SvcName]),
    { route_failed, St };

    
%% Try to queue message
queue_message(SvcName, 
	      TransID, 
	      [ { { _Pr, _PrOp }, { DLMod, DLOp } }  | RemainingRoutes ],
	      Timeout, 
	      RelativeTimeout,
	      Parameters, 
	      Signature,
	      Certificate, 
	      St) ->

    ?debug("sched:q(~p:~s): timeout:      ~p", [DLMod, SvcName, Timeout]),
    %%?debug("sched:q(~p:~s): parameters:   ~p", [DLMod, SvcName, Parameters]),
    %%?debug("sched:q(~p:~s): signature:    ~p", [DLMod, SvcName, Signature]),
    %%?debug("sched:q(~p:~s): certificate:  ~p", [DLMod, SvcName, Certificate]),

    SvcRec = find_or_create_service(SvcName, DLMod, St),
    
    %% The service may already be available, give it a shot.
    case try_sending_messages(SvcRec, St) of
	{ ok, NSt } -> %% We managed to send the message. Done.
	    { ok, NSt };

	{ _ , NSt } -> %% Failed to send message. Setup data link
	

	    %%
	    %% Bring up the relevant data link for the given route.
	    %% Once up, the data link will invoke service_availble()
	    %% to indicate that the service is available for the given DL.
	    %% 
	    case DLMod:setup_data_link(NSt#st.cs, SvcName, DLOp) of
		[ ok, DLTimeout ] ->

		    TOut = select_timeout(RelativeTimeout, DLTimeout),
		    %% Setup a timeout that triggers on whatever
		    %% comes first of the message's general timeout
		    %% or the timeout of the data link bringup
		    %%
		    ?debug("sched:q(~p:~s): ~p seconds for link up.", 
			   [ DLMod, SvcName, DLTimeout / 1000.0]),         

		    TRef = erlang:send_after(TOut, self(), 
					     { rvi_message_timeout, SvcName, DLMod, TransID }),


		    %% Setup a message to be added to the service / dl table.
		    Msg = #message { 
			     transaction_id = TransID,
			     data_link = { DLMod, DLOp },
			     routes = RemainingRoutes,
			     timeout = Timeout,
			     timeout_tref = TRef,
			     parameters = Parameters, 
			     signature = Signature,
			     certificate = Certificate
			    },

		    %% Add to ets table
		    ets:insert(SvcRec#service.messages_tid, Msg),
		    {ok, NSt};

		%% We failed with this route. Try the next one
		[ error, _Reason] ->
		    ?debug("sched:q(~p:~s): failed to setup.", [ DLMod, SvcName]),         
		    queue_message(SvcName,
				  TransID,
				  Timeout,
				  calc_relative_tout(Timeout), 
				  RemainingRoutes,
				  Parameters,
				  Signature,
				  Certificate, 
				  NSt)
	    end
    end.




%% The service is not available
try_sending_messages(#service { 
			key = { SvcName, _ },
			available = unavailable,
			messages_tid = _Tid } = _SvcRec, St) ->

    ?info("sched:try_send(): SvcName:   ~p: Not available", [SvcName]),
    { not_available, St };

try_sending_messages(#service { 
			key = { SvcName, DataLinkMod },
			available = available,
			messages_tid = Tid } = SvcRec, St) ->

    ?debug("sched:try_send(): Service:         ~p:~p", [DataLinkMod, SvcName]),

    %% Extract the first message of the queue.
    case first_service_message(SvcRec) of
	empty ->
	    ?debug("sched:try_send(): Nothing to send"),
	    { ok, St };

	yanked -> 
	    ?info("sched:try_send(): Message was yanked while trying to send: ~p", 
		  [SvcRec#service.key]),
	    { ok, St};

	Msg ->
	    %% Wipe from ets table and cancel timer
	    ets:delete(Tid, Msg#message.transaction_id),

	    erlang:cancel_timer(Msg#message.timeout_tref),

	    %% Forward to protocol.
	    { _, DataLinkOpts } = Msg#message.data_link,
	    { ProtoMod, ProtoOpts } = Msg#message.protocol,

	    %% Send off message to the correct protocol module
	    case ProtoMod:send_message(
		   St#st.cs,
		   SvcName,
		   Msg#message.timeout,
		   ProtoOpts,
		   DataLinkMod,
		   DataLinkOpts,
		   Msg#message.parameters,
		   Msg#message.signature,
		   Msg#message.certificate) of

		%% Success
		[ok] -> 
		    %% Send the rest of the messages associated with this service/dl
		    try_sending_messages(SvcRec, St);

		%% Failed
		[Err] ->
		    ?info("sched:try_send(): No send: ~p:~p:~p -> ~p : ~p", 
			  [ProtoMod, DataLinkMod, SvcName, Err]),

		    %% Requeue this message with the next route
		    { _, St1} = queue_message(SvcName,
					      Msg#message.transaction_id,
					      Msg#message.timeout, 
					      calc_relative_tout(Msg#message.timeout),
					      Msg#message.routes,
					      Msg#message.parameters,
					      Msg#message.signature,
					      Msg#message.certificate,
					      St),

		    %% Send the rest of the messgages targeting 
		    %% the same service through the same data link
		    %% If they all fail due to some data link error,
		    %% they will all be requeued to the next route.
		    try_sending_messages(SvcRec, St1)
	    end
    end.


find_or_create_service(SvcName, DataLinkMod, #st { services_tid = SvcTid } = St) ->
    ?debug("sched:find_or_create_service(): ~p:~p", [ DataLinkMod, SvcName]),

    case ets:lookup(SvcTid, { SvcName, DataLinkMod }) of
	[] ->  %% The given service does not exist, create it.
	    ?debug("sched:find_or_create_service(): Creating new ~p", [ SvcName]),
	    update_service(SvcName, DataLinkMod, unavailable, St);

	[ SvcRec ] -> 
	    %% Update the network address, if it differs, and return
	    %% the new service / State as {ok, NSvcRec, false, NSt}
	    ?debug("sched:find_or_create_service(): Updating existing ~p", [ SvcName]),
	    SvcRec
    end.



%% Create a new service, or update an existing one, and return the new
%% state (not currently modified) and the newly initialized service
%% revord.
%%
update_service(SvcName, DataLinkMod, Available, 
	       #st { services_tid = SvcsTid }) ->
    %% Return new service and existing state.
    ?debug("sched:create_service():  ~p:~s ", [ DataLinkMod, SvcName]),
    SvcRec = #service { 
		key = { SvcName, DataLinkMod },
		available = Available, 
		messages_tid = ets:new(rvi_messages, 
				       [ ordered_set, private, 
					 { keypos, #message.transaction_id } ])
	       },

    %% Insert new service to ets table.
    ets:insert(SvcsTid, SvcRec),
    SvcRec. 


%% Create a new and unique transaction id
create_transaction_id(St) ->
    ?debug("sched:create_transaction_id(~p): ", [  St#st.next_transaction_id ]),
    ID = St#st.next_transaction_id,

    %% FIXME: Maybe interate pid into transaction to handle multiple
    %% schedulers?
    { ID, St#st { next_transaction_id = ID + 1 }}.

%% Calculate a relative timeout based on the UnixTime TS we are provided with.
calc_relative_tout(UnixTime) ->
    { Mega, Sec, _Micro } = now(),
    Now = Mega * 1000000 + Sec,
    ?debug("sched:calc_relative_tout(): Timeout(~p) - Now(~p) = ~p", [ UnixTime, Now, UnixTime - Now ]),

    %% Cap the timeout value at something reasonable
    TOut = 
	case UnixTime - Now >= 4294967295 of
	    true -> 
		?info("sched:calc_relative_tout(): Timeout(~p) - Now(~p) = ~p: "
		      "Truncated to 4294967295", [ UnixTime, Now, UnixTime - Now ]),
		4294967295;

	false -> UnixTime - Now
    end,

    case TOut =< 0 of
	true ->
	    -1; %% We have timed out

	false ->
	    TOut * 1000 
    end.

%% Handle a callback for a timed out message.

do_timeout_callback(CompSpec, SvcName, TransID) ->
    service_edge_rpc:handle_local_timeout(CompSpec, SvcName, TransID),
    ok.



%% Kill off a service that is no longer used.
%%
delete_unused_service(SvcTid, SvcRec) ->
    %% Do we have messages waiting for this service?
    case ets:first(SvcRec#service.messages_tid) of
	%% Nope.
	'$end_of_table' ->
	    ets:delete(SvcRec#service.messages_tid),
	    ets:delete(SvcTid, SvcRec#service.key),

	    %% Update the network address, if it differs, and return
	    %% the new service / State as {ok, NSvcRec, false, NSt}
	    ?debug("sched:service_unavailable(): Service ~p:~p now has no address.", 
		   [ SvcRec#service.key ]),
	    true;

	_ -> false
    end.

first_service_message(#service {  messages_tid = Tid }) ->
    case ets:first(Tid) of
	'$end_of_table' ->
	    empty;
	
	Key ->
	    case ets:lookup(Tid, Key) of
		[ Msg ] -> Msg;
		[] -> yanked
	    end
    end.
		    

%% A timeout of -1 means 'does not apply'
%%
%% However, if both are -1, we just return the shortest posssible
%% timeout.
%% If both timeouts are specified, we select the shortest one.
%%
select_timeout(TimeOut1, TimeOut2) ->

    case { TimeOut1 =:= -1, TimeOut2 =:= -1 } of
	{ true, true } -> 1;

	{ true, false } -> TimeOut2;

	{ false, true } -> TimeOut1;

	{ false, false } -> min(TimeOut1, TimeOut2)
    end.


	    
