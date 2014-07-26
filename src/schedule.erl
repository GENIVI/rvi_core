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

-record(st, { 
	  queue = undefined
	 }).

-record(message, { 
	  target, 
	  timeout,
	  network_address,
	  parameters,
	  signature,
	  certificate
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
    {ok, #st{ queue = queue:new()}}.

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

    case 
	rvi_common:send_component_request(data_link, setup_data_link, 
					  [
					   {service, Target}, 
					   {network_address, NetworkAddress}
					  ]) of
	{ ok, _, _JSON} -> 
	    ok;
	Err -> 
	    ?debug("schedule:schedule_message() Failed at data_link_up: ~p", 
		      [ Err ])
    end,

    NQ = queue:in(#message {
		     target = Target, 
		     timeout = Timeout,
		     network_address = NetworkAddress,
		     parameters = Parameters,
		     signature = Signature,
		     certificate = Certificate
		    }, St#st.queue),
    {noreply, St#st { queue = NQ }};


handle_cast( {data_link_up, NetworkAddress, AvailableServices}, St) ->
    NSt = forward_messages(St, NetworkAddress, AvailableServices),
    {noreply, NSt };


handle_cast( {data_link_down, NetworkAddress, DiscontinuedServices}, St) ->
    ?debug("schedule:data_link_down(): NetworkAddress: ~p DiscontinuedServices: ~p",
	      [NetworkAddress, DiscontinuedServices]),
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

forward_messages(St, NetworkAddress, AvailableServices) ->
    case queue:out(St#st.queue) of
	{{ value, Elem }, NQ } ->
	    case 
		rvi_common:send_component_request(
		  protocol, send_message, 
		  [
		   { target, Elem#message.target },
		   { timeout, Elem#message.timeout},
		   { network_address, Elem#message.network_address},
		   { parameters, Elem#message.parameters},
		   { signature, Elem#message.signature},
		   { certificate, Elem#message.certificate }
		  ]) of
		{ ok, _JSONStatus, _JSON} -> 
		    forward_messages(St#st {queue = NQ}, NetworkAddress, AvailableServices);
		Err -> 
		    ?debug("schedule:forward_messages Failed at protocol:transmit_request(): ~p", 
			      [ Err ]),
		    { noreply, St}
	    end;

	%% No more elements in the queue
	{ empty, _ } ->
	    ?debug("schedule:forward_messages(). All forwarded."),
	    {noreply, St }
    end.





       
