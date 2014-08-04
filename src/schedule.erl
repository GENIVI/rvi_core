%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.home>
%%% @copyright (C) 2014, magnus
%%% @doc
%%%
%%% @end
%%% Created : 12 Jul 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(schedule).

-behaviour(gen_server).
-include_lib("lager/include/log.hrl").

%% API
-export([start_link/0]).
-export([schedule_message/6,
	 data_link_up/2, 
	 data_link_down/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%% One message, living in #service.queue, awaiting transmission
%% to the target specified by #service.target

-record(message, { 
	  timeout,
	  parameters,
	  signature,
	  certificate
	 }).

-record(service, {
	  target = "", %% Target service, as reported by data_link_up
	  network_address = unavailable, %% Network address of target, as reported by data_link_up.
	  queue = undefined %% Queue of #messages awaiting target.
	  }).

-record(st, { 
	  services = []
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
    {ok, #st{ services = []}}.

schedule_message(Target, 
		 Timeout, 
		 NetworkAddress, 
		 Parameters,
		 Signature,
		 Certificate) ->
    gen_server:cast(?SERVER, { 
		       schedule_message,
		       Target, 
		       Timeout, 
		       NetworkAddress, 
		       Parameters,
		       Signature,
		       Certificate
		      }).

data_link_up(NetworkAddress, AvailableServices) ->
    gen_server:cast(?SERVER, { data_link_up, NetworkAddress, AvailableServices }).

data_link_down(NetworkAddress, DiscontinuedServices) ->
    gen_server:cast(?SERVER, { data_link_down, NetworkAddress, DiscontinuedServices }).
    
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
handle_cast( { schedule_message,
	       Target, 
	       Timeout, 
	       NetworkAddress, 
	       Parameters,
	       Signature,
	       Certificate
	     }, St) ->
    ?debug("schedule:sched_msg(): target:          ~p", [Target]),
    ?debug("schedule:sched_msg(): timeout:         ~p", [Timeout]),
    ?debug("schedule:sched_msg(): network_address: ~p", [NetworkAddress]),
    ?debug("schedule:sched_msg(): parameters:      ~p", [Parameters]),
    ?debug("schedule:sched_msg(): signature:       ~p", [Signature]),
    ?debug("schedule:sched_msg(): certificate:     ~p", [Certificate]),

    %% Queue the message. The result will tell us if the target service
    %% is available.
    Msg = #message {
	     timeout = Timeout,
	     parameters = Parameters,
	     signature = Signature,
	     certificate = Certificate
	    },
    

    NSt = %% Get the new state from queue_message
	case queue_message(Target, Msg, St) of
	    { first_message, TmpSt } ->
		?debug("schedule:sched_msg(): Queued message is the first one. "
		       "Bring up data link"),
		rvi_common:send_component_request(data_link, setup_data_link, 
						  [
						   {service, Target}, 
						   {network_address, NetworkAddress}
						  ]),
		TmpSt;
	    
	    { send_now, NetworkAddress, TmpSt } ->
		?debug("schedule:sched_msg(): Servivce is already available. Send now."),
		{ok, TmpSt} = send_message(NetworkAddress, Target, Timeout, 
					   Parameters, Signature, Certificate),
		TmpSt;
	    { ok, TmpSt } ->
		?debug("schedule:sched_msg(): Message queued."),
		
		TmpSt
	end,
    
    {noreply, NSt };


handle_cast( {data_link_up, NetworkAddress, AvailableServices}, St) ->
    multiple_services_available(AvailableServices, NetworkAddress, St),
    {noreply, St};




handle_cast( {data_link_down, _NetworkAddress, DiscontinuedServices}, St) ->
    ?debug("schedule:data_link_down(): NetworkAddress: ~p DiscontinuedServices: ~p",
	      [_NetworkAddress, DiscontinuedServices]),

    multiple_services_unavailable(DiscontinuedServices, St),
    {noreply, St };

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
%% Can return:

%% { send_now, St}
%%   The target service is already available and Msg should be sent. Msg not queued.
%%
%% { first_message, St}
%%   The target is not available, and Msg was the first message queued to the service.
%%   Ask data_link to bring up a link to the target.
%%  
%% {ok, St}
%%   Target is not available, and Msg was added to one or more existing messages in the 
%%   queue, awaiting a data_link_up command from data_link so that they can be sent.
%%
queue_message(Target, Msg, St) ->


    ?debug("schedule:sched_msg(): target:              ~p", [Target]),
    ?debug("schedule:sched_msg(): msg.timeout:         ~p", [Msg#message.timeout]),
    ?debug("schedule:sched_msg(): msg.parameters:      ~p", [Msg#message.parameters]),
    ?debug("schedule:sched_msg(): msg.signature:       ~p", [Msg#message.signature]),
    ?debug("schedule:sched_msg(): msg.certificate:     ~p", [Msg#message.certificate]),

    
    %% Does the service even exist?
    case find_service(Target, St) of
	
	%% The given service does not exist,  
	%% Create it with a single message in a freshly created queue.
	%%
	not_found -> 
	    ?debug("schedule:sched_msg(): New service"),

	    %% Create a queue with one element in it.
	    Q = queue:in(Msg, queue:new()),
	    
	    %% Create a new service with unavailable network address and
	    %% a single element queue with the new message.
	    { first_message, modify_service(Target, unavailable, Q, St) };

	%% Svc exists, but is unavailable add the element to the queue.
	%%
	{ unavailable, Svc}  -> 
	    %% Remove the service from the NSt

	    %% Update the queue.
	    NQ = queue:in(Msg, Svc#service.queue),

	    %% Replace the existing service with a new one having the 
	    %% updated queue.
	    NSt = modify_service(Target, unavailable, NQ, St ),

	    %% Check if the exiting queue (before we add Msg) is empty.
	    %% If so, this is the first element queued, and we should
	    %% reply with a first_message response

	    case queue:is_empty(Svc#service.queue) of
		true -> %% Nothing to send
		    ?debug("schedule:sched_msg(): Unavailable - first messge"),
		    { first_message, NSt };

		false -> %% There are already elements in the queue.
		    ?debug("schedule:sched_msg(): Unavailable - additional messsge"),
		    { ok, NSt }
			
	    end;

				  
	%% Service exists, and has a network address. Do not queue. Send now.
	NetworkAddress ->  
	    { send_now, NetworkAddress, St }
    end.
    


%% Mark a service as available, as reported by data_link_up
%% The service either exists in St#st.services, in which case it is modified,
%% or it will be added.
%%
%%  Return value if {ok, St}, or {send_messages, St}, depending on
%%  if messages for the given service are now ready to be sent.
%%
service_available(Target, NetworkAddress, St) ->
    ?debug("schedule:service_available(): target:            ~p~n", [ Target ]),
    ?debug("schedule:service_available(): network_address:   ~p~n",  [ NetworkAddress ]),
								    
    case find_service(Target, St) of 
	not_found -> 	%% The given service does not exist, create it.
	    ?debug("schedule:service_available(): New service. Adding.~n"),
	    { ok, modify_service(Target, NetworkAddress, St) };
	
	{ _, Svc }  ->
	    %% Check if we have elements ready to transmit
	    case queue:is_empty(Svc#service.queue) of
		true -> %% Nothing to send. Add service and return ok
		    ?debug("schedule:service_available(): Existing service. No pending messages.~n"),
		    { ok, modify_service(Target, NetworkAddress, Svc#service.queue, St) };

		false -> %% We have elements to send. Add service and report send.
		    ?debug("schedule:service_available(): Existing service. Pending messages.~n"),
		    { send_messages, modify_service(Target, NetworkAddress, Svc#service.queue, St) }
	    end
    end.


%% Add a service with a fresh queue, or modify an existing one
modify_service(Target, NetworkAddress, St) ->
   modify_service(Target, NetworkAddress, queue:new(), St).

%% Add or modify a service with a carried over queue.
%% Any existing services with the same target name is replaced.
modify_service(Target, NetworkAddress, Queue, St) ->
    %% Delete old service
    NewSvcs = lists:keydelete(Target, #service.target, St#st.services),

    %% Modify state with the new service setup.
    St#st { services = [ #service { 
			    target = Target,
			    network_address = NetworkAddress, 
			    queue = Queue } | NewSvcs ] }.
    


%% Send a single message to protocol.
%% Check that we don't get invoked with network_address == unavailable,
%% which means that the service cannot be reached.
send_message(unavailable, _Target, _Timeout, 
	     _Parameters, _Signature, _Certificate) ->

    ?debug("schedule:send_message(): network_address:   UNAVAILABLE"),
    ?debug("schedule:send_message(): target:            ~p", [_Target]),
    ?debug("schedule:send_message(): timeout:           ~p", [_Timeout]),
    ?debug("schedule:send_message(): parameters:        ~p", [_Parameters]),
    ?debug("schedule:send_message(): signature:         ~p", [_Signature]),
    ?debug("schedule:send_message(): certificate:       ~p", [_Certificate]),
    {error, unavailable};

send_message(NetworkAddress, Target, Timeout, 
	     Parameters, Signature, Certificate) ->

    ?debug("schedule:send_message(): network_address:   ~p", [NetworkAddress]),
    ?debug("schedule:send_message(): target:            ~p", [Target]),
    ?debug("schedule:send_message(): timeout:           ~p", [Timeout]),
    ?debug("schedule:send_message(): parameters:        ~p", [Parameters]),
    ?debug("schedule:send_message(): signature:         ~p", [Signature]),
    ?debug("schedule:send_message(): certificate:       ~p", [Certificate]),

    case rvi_common:send_component_request(
	   protocol, send_message, 
	   [
	    { target, Target },
	    { timeout, Timeout},
	    { network_address, NetworkAddress},
	    { parameters, Parameters},
	    { signature, Signature},
	    { certificate, Certificate }
	   ]) of
	{ ok, _, _} -> ok;
	Err -> Err
    end.

    
send_messages(#service { network_address = NetworkAddress, 
			 target = Target,
			 queue = Queue } = Svc) ->

    ?debug("schedule:send_messages(): target:            ~p", [Target]),
    ?debug("schedule:send_messages(): network_address:   ~p", [NetworkAddress]),
    ?debug("schedule:send_messages(): queue_size:        ~p", [queue:len(Queue)]),

    case queue:out(Queue) of
	{{ value, Msg }, NQ } ->
	    case 
		send_message(NetworkAddress, 
			     Target, 
			     Msg#message.timeout,
			     Msg#message.parameters,
			     Msg#message.signature, 
			     Msg#message.certificate) of
		 ok -> 
		    send_messages(Svc#service { queue = NQ});

		Err -> 
		    ?warning("schedule:send_messages(): Failed: ~p", 
			      [ Err ]),

		    send_messages(Svc#service { queue = NQ}) 

	    end;

	%% No more elements in the queue
	{ empty, _ } ->
	    ?debug("schedule:send_messages(). All forwarded."),
	    Svc
end.


%% data_link_up has reported that multiple services are now
%% available at NetworkAddress. 
%% Iterate through all services and mark them as available,
%% possibly sending any pending message targeting the newly activated
%% service.
%%
multiple_services_available([], _NetworkAddress, St) ->
    {ok, St};

multiple_services_available([ Svc | T], NetworkAddress, St) ->

    NSt = case service_available(Svc, NetworkAddress, St) of
	      { send_messages, TmpSt } ->
		  { _, SvcRec } = find_service(Svc, TmpSt),
		  TmpSt#st { services =  send_messages(SvcRec) };

	      { ok, TmpSt } ->
		  TmpSt

	  end,
    multiple_services_available(T, NetworkAddress, NSt).



multiple_services_unavailable([], St) ->
    {ok, St};

multiple_services_unavailable([ Svc | T], St) ->
    { _, SvcRec } = find_service(Svc, St),
    NSt = modify_service(SvcRec#service.target, unavailable, SvcRec#service.queue, St),
    multiple_services_unavailable(T, NSt).

find_service(Target, #st { services = Svcs }) ->
    case lists:keyfind(Target, #service.target, Svcs) of
	false ->  %% The given service does not exist, create it.
	    not_found;
	
	Svc when Svc#service.network_address =:= unavilable -> 
	    { unavailable, Svc };
	
	Svc ->
	    { available, Svc }
    end.
    
