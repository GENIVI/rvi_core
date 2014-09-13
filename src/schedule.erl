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
-export([schedule_message/5,
	 register_remote_services/2, 
	 unregister_remote_services/2]).

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
	  target = "", %% Target service, as reported by register_remote_services
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
		 Parameters,
		 Signature,
		 Certificate) ->
    gen_server:cast(?SERVER, { 
		       schedule_message,
		       Target, 
		       Timeout, 
		       Parameters,
		       Signature,
		       Certificate
		      }).

register_remote_services(NetworkAddress, AvailableServices) ->
    gen_server:cast(?SERVER, { register_remote_services, NetworkAddress, AvailableServices }).

unregister_remote_services(NetworkAddress, DiscontinuedServices) ->
    gen_server:cast(?SERVER, { unregister_remote_services, NetworkAddress, DiscontinuedServices }).
    
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
	       Parameters,
	       Signature,
	       Certificate
	     }, St) ->
    ?info("    schedule:sched_msg(): target:            ~p", [Target]),
    ?info("    schedule:sched_msg(): timeout:           ~p", [Timeout]),
    ?info("    schedule:sched_msg(): parameters:        ~p", [Parameters]),
    ?info("    schedule:sched_msg(): signature:         ~p", [Signature]),
    ?info("    schedule:sched_msg(): certificate:       ~p", [Certificate]),
    ?info("    schedule:sched_msg(): St:                ~p", [St]),

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
		?info("    schedule:sched_msg(): Queued message is the first one. "
		       "Bring up data link"),
		case bring_up_data_link(Target) of
		    already_connected ->
			?info("    schedule:sched_msg(): Link already up. Send. "),
			ok = send_message(Target, Timeout, Parameters, Signature, Certificate);

		    Other  ->
 			?info("    schedule:sched_msg(): Got. ~p", [Other]),
			ok
		end,

		TmpSt;
	    
	    { send_now, TmpSt } ->
		?info("    schedule:sched_msg(): Servivce is already available. Send now."),
		ok = send_message(Target, Timeout, Parameters, Signature, Certificate),
		TmpSt;

	    { ok, TmpSt } ->
		?info("    schedule:sched_msg(): Message queued."),
		TmpSt
	end,
    
    {noreply, NSt };


handle_cast( {register_remote_services, NetworkAddress, AvailableServices}, St) ->

    ?info("    schedule:register_remote_services(): NetworkAddress:    ~p", [NetworkAddress]),
    ?info("    schedule:register_remote_services(): AvailableService:  ~p", [AvailableServices]),
	   
    {ok, NSt} =  multiple_services_available(AvailableServices, NetworkAddress, St),
    {noreply, NSt};


handle_cast( {unregister_remote_services, NetworkAddress, DiscontinuedServices}, St) ->
    ?info("    schedule:register_remote_services(): NetworkAddress:        ~p", [NetworkAddress]),
    ?info("    schedule:register_remote_services(): DiscontinuedServices:  ~p", 
	   [DiscontinuedServices]),

    {ok, NSt } = multiple_services_unavailable(DiscontinuedServices, St),
    {noreply, NSt };

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
    ?info("    schedule:sched_msg(): target:              ~p", [Target]),
    ?info("    schedule:sched_msg(): msg.timeout:         ~p", [Msg#message.timeout]),
    ?info("    schedule:sched_msg(): msg.parameters:      ~p", [Msg#message.parameters]),
    ?info("    schedule:sched_msg(): msg.signature:       ~p", [Msg#message.signature]),
    ?info("    schedule:sched_msg(): msg.certificate:     ~p", [Msg#message.certificate]),
    
    %% Does the service even exist?
    case find_service(Target, St) of
	
	%% The given service does not exist,  
	%% Create it with a single message in a freshly created queue.
	%%
	not_found -> 
	    ?info("    schedule:sched_msg(): New service"),

	    %% Create a queue with one element in it.
	    Q = queue:in(Msg, queue:new()),
	    
	    %% Create a new service with unavailable network address and
	    %% a single element queue with the new message.
	    { first_message, modify_service(Target, Q, St) };

	%% Service exists, and has a network address. Do not queue. Send now.
	{ok, Svc }->  
	    ?info("    schedule:sched_msg(): SEND NOW: ~p", [Svc]),
	    { send_now, St }
    end.


%% Mark a service as available, as reported by data_link_up
%% The service either exists in St#st.services, in which case it is modified,
%% or it will be added.
%%
%%  Return value if {ok, St}, or {send_messages, St}, depending on
%%  if messages for the given service are now ready to be sent.
%%
service_available(Target, NetworkAddress, St) ->
    ?info("    schedule:service_available(): target:            ~p", [ Target ]),
    ?info("    schedule:service_available(): network_address:   ~p", [ NetworkAddress ]),
								    
    case find_service(Target, St) of 
	not_found -> 	%% The given service does not exist, create it.
	    {ok, St};
	
	{ ok, Svc }  ->
	    %% Check if we have elements ready to transmit
	    case queue:is_empty(Svc#service.queue) of
		true -> %% Nothing to send.
		    { ok, St };

		false -> %% We have elements to send. Send them
		    ?info("    schedule:service_available(): Existing service. Pending messages."),
		    NewSvc = send_messages(Svc, NetworkAddress),
		    %% Update service in state.
		    {ok, modify_service_rec(NewSvc, St) }
	    end
    end.



%% Add or modify a service with a carried over queue.
%% Any existing services with the same target name is replaced.
modify_service(Target, Queue, St) ->
    ?info("    schedule:modify_service(): Target:           ~p", [ Target]),
    ?info("    schedule:modify_service(): Queue:            ~p", [ Queue]),
    ?info("    schedule:modify_service(): St:               ~p", [ St]),

    %% Delete old service
    NewSvcs = lists:keydelete(Target, #service.target, St#st.services),

    %% Modify state with the new service setup.
    St#st { services = [ #service { 
			    target = Target,
			    queue = Queue } | NewSvcs ] }.
    
modify_service_rec(SvcRec, St) ->
    ?info("    schedule:modify_service(rec): Target:           ~p", [ SvcRec#service.target]),
    ?info("    schedule:modify_service(rec): Queue:            ~p", [ SvcRec#service.queue]),
    ?info("    schedule:modify_service(rec): St:               ~p", [ St]),

    %% Delete old service
    NewSvcs = lists:keydelete(SvcRec#service.target, #service.target, St#st.services),

    %% Modify state with the new service setup.
    St#st { services = [ SvcRec | NewSvcs ] }.
    
bring_up_data_link(Target) ->
    %% Resolve the target to a network address that we can 
    %% use to bring up the data link
    case rvi_common:send_component_request(service_discovery, 
					   resolve_remote_service,
					   [ {service, Target} ],
					   [ network_address ]) of

	{ ok, ok, [ NetworkAddress], _SDJSON } -> 
	    %% Tell data link to bring up a communicationc hannel.
	    case rvi_common:send_component_request(data_link, setup_data_link, 
					      [
					       {service, Target}, 
					       {network_address, NetworkAddress}
					      ]) of
		{ok, already_connected, _} ->
		    already_connected;
		Que -> 
		    ?info("    schedule:bring_up_data_link() Que:~p.", [Que]),
		    ok
	    end;

	{ok, not_found, _, _} ->
	    ?info("    schedule:bring_up_data_link() Failed to resolve remote Service: ~p. Service not found.", 
			      [ Target ]),
	    not_found;
		    
	Err ->  
	    ?info("    schedule:bring_up_data_link() Failed to resolve remote Service: ~p: ~p", 
			      [ Target, Err ]),
	    err
    end.

send_message(Target, Timeout, 
	     Parameters, Signature, Certificate) ->
    %% Resolve the target to a network address
    case rvi_common:send_component_request(service_discovery, 
					   resolve_remote_service,
					   [ {service, Target} ],
					   [ network_address ]) of
	{ ok, ok, [ NetworkAddress], _SDJSON } -> 
	    send_message(NetworkAddress, Target, Timeout,
			 Parameters, Signature, Certificate);

	{ok, not_found, _, _} ->
	    ?info("    schedule:send_message() Failed to resolve remote Service: ~p. Service not found.", 
			      [ Target ]),
	    not_found;
		    
	Err ->  
	    ?info("    schedule:send_message() Failed to resolve remote Service: ~p: ~p", 
			      [ Target, Err ]),

	    err
    end.


send_message(NetworkAddress, Target, Timeout, 
	     Parameters, Signature, Certificate) ->

    ?info("    schedule:send_message(): network_address: ~p", [NetworkAddress]),
    ?info("    schedule:send_message(): target:          ~p", [Target]),
    ?info("    schedule:send_message(): timeout:         ~p", [Timeout]),
    ?info("    schedule:send_message(): parameters:      ~p", [Parameters]),
    ?info("    schedule:send_message(): signature:       ~p", [Signature]),
    ?info("    schedule:send_message(): certificate:     ~p", [Certificate]),

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

    
send_messages(#service { target = Target,
			 queue = Queue } = Svc, 
	      NetworkAddress) ->

    ?info("    schedule:send_messages(): target:            ~p", [Target]),
    ?info("    schedule:send_messages(): network_address:   ~p", [NetworkAddress]),
    ?info("    schedule:send_messages(): queue_size:        ~p", [queue:len(Queue)]),

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
		    send_messages(Svc#service { queue = NQ}, NetworkAddress);

		Err -> 
		    ?warning("schedule:send_messages(): Failed: ~p", 
			      [ Err ]),

		    send_messages(Svc#service { queue = NQ}, NetworkAddress) 

	    end;

	%% No more elements in the queue
	{ empty, _ } ->
	    ?info("    schedule:send_messages(). All forwarded: ~p", [Svc]),
	    Svc
end.


%% data_link_up has reported that multiple services are now
%% available at NetworkAddress. 
%% Iterate through all services and mark them as available,
%% possibly sending any pending message targeting the newly activated
%% service.
%%
multiple_services_available([], _NetworkAddress, St) ->
    ?info("    schedule:multiple_services_available():  St: ~p", [ St]),
    {ok, St};

multiple_services_available([ Svc | T], NetworkAddress, St) ->
    {ok, NSt}  = service_available(Svc, NetworkAddress, St),
    multiple_services_available(T, NetworkAddress, NSt).

multiple_services_unavailable(_, St) ->
    {ok, St}.

find_service(Target, #st { services = Svcs } = _St) ->
    ?info("    schedule:find_service(): St: ~p", [ _St]),
    case lists:keyfind(Target, #service.target, Svcs) of
	false ->  %% The given service does not exist, create it.
	    not_found;
	
	Svc ->
	    { ok, Svc }
    end.
    
