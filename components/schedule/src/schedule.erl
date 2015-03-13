%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(schedule).


-behaviour(gen_server).
-include_lib("lager/include/log.hrl").

%% API
-export([start_link/0]).
-export([schedule_message/6,
	 register_remote_services/2, 
	 unregister_remote_services/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%% Message structure and storage
%%  A message is a piece of 
%%
%%  Service -> ETS -> Messages
-record(service, {
	  name = "", %% Fully qualified service name.
	  %% Set to { IP, Port} when service is available, else unknown_network_address.
	  network_address = unknown_network_address, 

	  %% Table containing #message records, 
	  %% indexed by their transaction ID (and sequence of delivery)
	  messages_tid =  undefined
	 }).


% A single message to be delivered to a service.
% Messages are stored in ets tables hosted by a service 

-record(message, { 
	  transaction_id, %% Transaction ID that message is tagged with.
	  timeout,        %% Timeout, UTC
	  timeout_tref,  %% Reference to erlang timer associated with this message.
	  timeout_cb,    %% Callback to invoke when timeout occurs. 
	  parameters,
	  signature,
	  certificate
	 }).


-record(st, { 
	  next_transaction_id = 1, %% Sequentially incremented transaction id.
	  services_tid = undefined %% Known services.
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
    {ok, #st{ services_tid = ets:new(rvi_schedule_services, 
				     [  set, private, 
					{ keypos, #service.name } ])}}.

schedule_message(SvcName, 
		 Timeout, 
		 Callback,
		 Parameters,
		 Signature,
		 Certificate) ->
    gen_server:call(?SERVER, { 
		       schedule_message,
		       SvcName, 
		       Timeout, 
		       Callback,
		       Parameters,
		       Signature,
		       Certificate
		      }).

register_remote_services(NetworkAddress, AvailableServices) ->
    gen_server:call(?SERVER, { register_remote_services, NetworkAddress, AvailableServices }).

unregister_remote_services(ServiceNames) ->
    gen_server:call(?SERVER, { unregister_remote_services, ServiceNames }).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, St) ->
%%                                   {reply, Reply, St} |
%%                                   {reply, Reply, St, Timeout} |
%%                                   {noreply, St} |
%%                                   {noreply, St, Timeout} |
%%                                   {stop, Reason, Reply, St} |
%%                                   {stop, Reason, St}
%% @end
%%--------------------------------------------------------------------
handle_call( { schedule_message,
	       SvcName, 
	       Timeout, 
	       Callback,
	       Parameters,
	       Signature,
	       Certificate
	     }, _From, St) ->

    ?debug("schedule:sched_msg(): service_name:     ~p", [SvcName]),
    ?debug("schedule:sched_msg(): timeout:          ~p", [Timeout]),
    ?debug("schedule:sched_msg(): callback:         ~p", [Callback]),
    ?debug("schedule:sched_msg(): parameters:       ~p", [Parameters]),
    ?debug("schedule:sched_msg(): signature:        ~p", [Signature]),
    ?debug("schedule:sched_msg(): certificate:      ~p", [Certificate]),
    ?debug("schedule:sched_msg(): St:               ~p", [St]),


    { ok, TransID, NSt } = queue_message(SvcName, 
					 Timeout, 
					 Callback, 
					 Parameters, 
					 Signature,
					 Certificate,
					 St),

    { reply, {ok, TransID}, NSt };

handle_call({rvi_call, schedule_message, Args}, From, St) ->
    {_, SvcName} = lists:keyfind(service_name, 1, Args),
    {_, Timeout} = lists:keyfind(timeout, 1, Args),
    {_, Parameters} = lists:keyfind(parameters, 1, Args),
    {_, Signature} = lists:keyfind(signature, 1, Args),
    {_, Certificate} = lists:keyfind(certificate, 1, Args),
    { reply, { ok, TransID}, NSt} = 
	handle_call( { schedule_message,
		       SvcName, 
		       Timeout, 
		       no_callback,
		       Parameters,
		       Signature,
		       Certificate
		     }, From, St),
    { reply, { ok, [ 
		     { status, rvi_common:json_rpc_status(ok)},
		     { transaction_id, TransID }]}, NSt};
    
  


handle_call( {register_remote_services, NetworkAddress, AvailableServices}, _From, St) ->
    ?info("schedule:register_remote_services(): services(~p) -> ~p", [AvailableServices, NetworkAddress]),
    {ok, NSt} =  multiple_services_available(AvailableServices, NetworkAddress, St),
    {reply, ok, NSt};


handle_call({rvi_call, register_remote_services, Args}, From, St) ->
    {_, NetworkAddress } = lists:keyfind(network_address, 1, Args),
    {_, AvailableServices } = lists:keyfind(services, 1, Args),
    {reply, ok, NSt } = handle_call({ register_remote_services, 
				      NetworkAddress, 
				      AvailableServices}, From, St),

    { reply, { ok, [{ status, rvi_common:json_rpc_status(ok) }] } , NSt}; 

handle_call( {unregister_remote_services, ServiceNames}, _From, St) ->
    ?info("schedule:unregister_remote_services(): Services(~p)", [ServiceNames]),
    {ok, NSt} =  multiple_services_unavailable(ServiceNames, St),
    {reply, ok, NSt };

handle_call({rvi_call, unregister_remote_services, Args}, From, St) ->
    {_, Services } = lists:keyfind(services, 1, Args),
    {reply, ok, NSt} = handle_call({ unregister_remote_services, Services}, From, St),
    { reply, { ok, [{ status, rvi_common:json_rpc_status(ok) } ]}, NSt};




handle_call(_Request, _From, St) ->
    Reply = ok,
    {reply, Reply, St}.


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

handle_cast(_Msg, St) ->
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
handle_info({ rvi_message_timeout, SvcName, TransID}, #st { services_tid = SvcTid } = St) ->

    case  ets:lookup(SvcTid, SvcName) of
	[ Svc ] ->
	    %% Delete from ets.
	    case ets:lookup(Svc#service.messages_tid, TransID) of
		[ Msg ] ->
		    ?info("schedule:timeout(): trans_id(~p) service(~p)", [ TransID, SvcName]),
		    do_timeout_callback(Svc, Msg),
		    ets:delete(Svc#service.messages_tid, TransID);
		_ -> 
		    ?info("schedule:timeout(): trans_id(~p) service(~p): Yanked while processing", [ TransID, SvcName]),
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


%% Queue a message for transmission once a service become available.
%% If the service does not exist, it will be created.
%% Once the message is queued, an attempt will be made to send all
%% messages queued in the given service. This attempt will fail
%% pretty quickly if the service's network address is set to
%% 'unavailable'
queue_message(SvcName, 
	      Timeout, 
	      Callback, 
	      Parameters, 
	      Signature,
	      Certificate, St) ->
%%    ?info("schedule:sched_msg(): service(~p)    timeout(~p)", [SvcName, Timeout]),
%%    ?info("schedule:sched_msg(): timeout (~p)", [Timeout]),
%%    ?info("schedule:sched_msg(): parameters:   ~p", [Parameters]),
%%    ?info("schedule:sched_msg(): signature:    ~p", [Signature]),
%%    ?info("schedule:sched_msg(): certificate:  ~p", [Certificate]),

    %% Create a new transaction ID.
    {NewTransID, NSt1} = create_transaction_id(St),

    %% Create a timer to handle message timeout
    TRef = erlang:send_after(calculate_timeout_period(Timeout), self(), 
			     { rvi_message_timeout, SvcName, NewTransID }),

    %% Build the message record.
    Msg = #message { 
	     transaction_id = NewTransID,
	     timeout = Timeout,
	     timeout_tref = TRef,
	     timeout_cb = Callback,
	     parameters = Parameters, 
	     signature = Signature,
	     certificate = Certificate
	    },

    %% Find or create the service

    %% If this is a message to a (yet) unknown service, we will create
    %% it to act as a placeholder for its messages until they time out
    %% or the service becomes available.
    %% 
    %% If the service does exist, we queue it and try to send it.
    %% 
    {ok, Service, IsNewService, NSt2 } = find_or_create_service(SvcName, NSt1),

    %% If the service was created by the call above,
    %% We will try to bring up a data link to it.
    case IsNewService of 
	true ->
	    bring_up_data_link(SvcName);

	false ->
	    ok
    end,

    %% Add to ets table
    ets:insert(Service#service.messages_tid, Msg),

    %% Attempt to send the message
    {SendResRes, NSt3 } = try_sending_messages(Service, NSt2),
	    
    %% Return
    { ok, NewTransID, NSt3}.


%% Check if we can send messages queued up under the given service.
try_sending_messages(#service { 
			name = SvcName,
			network_address = unknown_network_address,
			messages_tid = _Tid } = _Service, St) ->
    ?info("schedule:try_send(): SvcName:   ~p: Not available", [SvcName]),
    {not_available, St};


try_sending_messages(#service { 
			name = SvcName,
			network_address = NetworkAddress,
			messages_tid = Tid } = Service, St) ->

    ?debug("schedule:try_send(): SvcName:         ~p", [SvcName]),
    ?debug("schedule:try_send(): Network Address: ~p", [NetworkAddress]),
     
    %% Extract the first message of the queue.
    case ets:first(Tid) of
	%% No more messages to send.
	'$end_of_table' ->
	    ?debug("schedule:try_send(): Nothing to send"),
	    { ok, St};

	Key ->
	    ?debug("schedule:try_send(): Sending: ~p", [Key]),
	    %% Extract first message and try to send it
	    case ets:lookup(Tid, Key) of
		[ Msg ] ->
		    case send_message(NetworkAddress, 
				      SvcName, 
				      Msg#message.timeout,
				      Msg#message.parameters,
				      Msg#message.signature,
				      Msg#message.certificate) of
			ok -> 
			    %% Send successful - delete entry.
			    ets:delete(Tid, Key),

			    %% Delete timeout ref
			    erlang:cancel_timer(Msg#message.timeout_tref),

			    %% Send the rest.
			    try_sending_messages(Service, St);

			Err ->
			    ?info("schedule:try_send(): No send: ~p -> ~p : ~p", [SvcName, NetworkAddress, Err]),
			    %% Failed to send message, leave in queue and err out.
			    { {send_failed, Err}, St}
		    end;
		_ -> 
		    ?info("schedule:try_send(): Message was yanked while trying to send: ~p", [Key]),
		    { ok, St}

	    end
    end.


%% Mark a service as available, as reported by data_link_up
%%  If the service does not exist, it will be created as a host
%%  for future messages destined to that service.
%%
%%  If the service does exist, its network address will be upadted
%%  with whatever address was reported to this function.
%%
%%  Once the service has been created or updated, we will attempt
%%  to send any messages queued inside it.
%%
service_available(SvcName, NetworkAddress, St) ->
    ?info("schedule:service_available(): service(~p) -> NetworkAddress(~p)", [ SvcName, NetworkAddress ]),

    %% Find or create the service.
    {ok, Svc, _, NSt1} = find_or_create_service(SvcName, NetworkAddress, St),

    try_sending_messages(Svc, NSt1).

service_unavailable(SvcName,  #st { services_tid = SvcTid } = St) ->
    ?info("schedule:service_unavailable(): Service(~p)", [ SvcName ]),
    ets:delete(SvcTid, SvcName),
    { ok, St }.


bring_up_data_link(SvcName) ->
    %% Resolve the service to a network address that we can 
    %% use to bring up the data link
    case rvi_common:send_component_request(service_discovery, 
					   resolve_remote_service,
					   [ {service, SvcName} ],
					   [ network_address ]) of

	{ ok, ok, [ NetworkAddress] } -> 
	    %% Tell data link to bring up a communicationc hannel.
	    case rvi_common:send_component_request(data_link, setup_data_link, 
						   [
						    {service, SvcName}, 
						    {network_address, NetworkAddress}
						   ]) of
		{ok, already_connected } ->
		    already_connected;
		Que -> 
		    ?info("schedule:bring_up_data_link() Que:~p.", [Que]),
		    ok
	    end;

	{ok, not_found, _ } ->
	    ?info("schedule:bring_up_data_link() Failed to resolve remote Service: ~p."
		  "Service not found.", 
		  [ SvcName ]),
	    not_found;

	Err ->  
	    ?info("schedule:bring_up_data_link() Failed to resolve remote Service: ~p: ~p", 
		  [ SvcName, Err ]),
	    err
    end.

send_message(NetworkAddress, SvcName, Timeout, 
	     Parameters, Signature, Certificate) ->

    ?info("schedule:send_message(): service(~p) -> addr(~p) ", [SvcName, NetworkAddress]),
%%    ?info("schedule:send_message(): network_address: ~p", [NetworkAddress]),
%%    ?info("schedule:send_message(): timeout:         ~p", [Timeout]),
%%    ?info("schedule:send_message(): parameters:      ~p", [Parameters]),
%%    ?info("schedule:send_message(): signature:       ~p", [Signature]),
%%    ?info("schedule:send_message(): certificate:     ~p", [Certificate]),

    case rvi_common:send_component_request(
	   protocol, send_message, 
	   [
	    { service_name, SvcName },
	    { timeout, Timeout},
	    { network_address, NetworkAddress},
	    { parameters, Parameters},
	    { signature, Signature},
	    { certificate, Certificate }
	   ]) of
	{ ok, _ } -> ok;
	Err -> Err
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
    {ok, NSt}  = service_available(Svc, NetworkAddress, St),
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

%% Calculate a relative timeout based on the UTC TS we are provided with.
calculate_timeout_period(UTC) ->
    { Mega, Sec, _Micro } = now(),
    Now = Mega * 1000000 + Sec,
    ?debug("schedule:calculate_timeout_period(): Timeout(~p) - Now(~p) = ~p", [ UTC, Now, UTC - Now ]),

    %% Cap the timeout value at something reasonable
    TOut = 
	case UTC - Now >= 4294967295 of
	    true -> 
		?info("schedule:calculate_timeout_period(): Timeout(~p) - Now(~p) = ~p: Truncated to 4294967295", [ UTC, Now, UTC - Now ]),
		4294967295;

	false -> UTC - Now
    end,

    case TOut =< 0 of
	true ->
	    1; %% One millisec is the smallest value we will time out on

	false ->
	    TOut * 1000 
    end.

%% Handle a callback for a timed out message.

do_timeout_callback(Service, #message { timeout_cb = { M, F, A}, 
					transaction_id = TransID}) ->
    M:F(Service, TransID, A);

%% callback element of #message is not an {M,F,A} format, ignore.
do_timeout_callback(_,_) ->
    ok.

