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
%%% Created : 12 Sep 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(bt_connection).

-behaviour(gen_server).
-include_lib("lager/include/log.hrl").

%% API

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([setup/6]).
-export([accept/6]).
-export([send/2]).
-export([send/3]).
-export([is_connection_up/1]).
-export([is_connection_up/2]).
-export([terminate_connection/1]).
-export([terminate_connection/2]).


-define(SERVER, ?MODULE). 

-record(st, {
	  remote_addr = "00:00:00:00:00:00",
	  channel = 0,
	  parent_pid = undefined,
	  rfcomm_ref = undefined,
	  mod = undefined,
	  func = undefined,
	  args = undefined
	 }).

%%%===================================================================
%%% API
%%%===================================================================
%% MFA is to deliver data received on the socket.

setup(Addr, Channel, Ref, Mod, Fun, Arg) ->
    gen_server:start_link(?MODULE, 
			  {connect, Addr, Channel, Ref, Mod, Fun, Arg },
			  []).

accept(Channel, ListenRef, ParentPid, Mod, Fun, Arg) ->
    gen_server:start_link(?MODULE, {accept, 
				    Channel, 
				    ListenRef, 
				    ParentPid,
				    Mod,
				    Fun, 
				    Arg},[]).

send(Pid, Data) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Data}).
    
send(Addr, Channel, Data) ->
    case connection_manager:find_connection_by_address(Addr, Channel) of
	{ok, Pid} ->
	    gen_server:cast(Pid, {send, Data});

	_Err -> 
	    ?info("connection:send(): Connection ~p:~p not found for data: ~p", 
		  [ Addr, Channel, Data]),
	    not_found

    end.

terminate_connection(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, terminate_connection).
    
terminate_connection(Addr, Channel) ->
    case connection_manager:find_connection_by_address(Addr, Channel) of
	{ok, Pid} ->
	    gen_server:call(Pid, terminate_connection);

	_Err -> not_found
    end.


is_connection_up(Pid) when is_pid(Pid) ->
    is_process_alive(Pid).

is_connection_up(Addr, Channel) ->
    case connection_manager:find_connection_by_address(Addr, Channel) of
	{ok, Pid} ->
	    is_connection_up(Pid);

	_Err -> 
	    false
    end.
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
%% MFA used to handle socket closed, socket error and received data
%% When data is received, a separate process is spawned to handle
%% the MFA invocation.
init({connect, BTAddr, Channel, ConnRef, Mod, Fun, Arg}) ->
    connection_manager:add_connection(BTAddr, Channel, self()),

    ?debug("connection:init(): self():   ~p", [self()]),
    ?debug("connection:init(): BTAddr:   ~p", [BTAddr]),
    ?debug("connection:init(): Channel:  ~p", [Channel]),
    ?debug("connection:init(): Ref:      ~p", [ConnRef]),
    ?debug("connection:init(): Module:   ~p", [Mod]),
    ?debug("connection:init(): Function: ~p", [Fun]),
    ?debug("connection:init(): Arg:      ~p", [Arg]),

    %% Grab socket control
    {ok, #st{
	    remote_addr = BTAddr,
	    channel = Channel,
	    rfcomm_ref = ConnRef,
	    mod = Mod,
	    func = Fun,
	    args = Arg
	   }};



init({accept, Channel, ListenRef, ParentPid, Mod, Fun, Arg}) ->
    { ok, ARef } = rfcomm:accept(ListenRef, infinity, self()),
    ?debug("bt_connection:init(): self():    ~p", [self()]),
    ?debug("bt_connection:init(): Channel:   ~p", [Channel]),
    ?debug("bt_connection:init(): ParentPid: ~p", [ParentPid]),
    ?debug("bt_connection:init(): Module:    ~p", [Mod]),
    ?debug("bt_connection:init(): Function:  ~p", [Fun]),
    ?debug("bt_connection:init(): Arg:       ~p", [Arg]),

    {ok, #st{
	    channel = Channel,
	    parent_pid = ParentPid,
	    rfcomm_ref = ARef,
	    mod = Mod,
	    func = Fun,
	    args = Arg
	   }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(terminate_connection, _From,  St) ->
    ?debug("~p:handle_call(terminate_connection): Terminating: ~p", 
	     [ ?MODULE, {St#st.remote_addr, St#st.channel}]),

    {stop, Reason, NSt} = handle_info({tcp_closed, St#st.rfcomm_ref}, St),
    {stop, Reason, ok, NSt};

handle_call(_Request, _From, State) ->
    ?warning("~p:handle_call(): Unknown call: ~p", [ ?MODULE, _Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send, Data},  St) ->
    ?debug("~p:handle_call(send): Sending: ~p", 
	     [ ?MODULE, Data]),

%%    gen_tcp:send(St#st.sock, term_to_binary(Data)),

    {noreply, St};

handle_cast(_Msg, State) ->
    ?warning("~p:handle_cast(): Unknown call: ~p", [ ?MODULE, _Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% An accept reference we've setup now has accetpted an
%% incoming connection.
handle_info({ARef, {accept, BTAddr, Channel} }, 
	    #st { rfcomm_ref = ConnRef, 
		  parent_pid = PPid } = St) 
  when ConnRef =:= ARef ->
    io:format("bt_connection from ~w:~w\n", [BTAddr,Channel]),
    PPid ! {accept, ARef, Channel, ok},
    { noreply, St#st { remote_addr = BTAddr, 
		       channel = Channel } };

handle_info({tcp, _ConnRef, Data}, 
	    #st { remote_addr = BTAddr,
		  channel = Channel,
		  mod = Mod,
		  func = Fun,
		  args = Arg } = State) ->
    ?debug("~p:handle_info(data): Data: ~p", [ ?MODULE, Data]),
    ?debug("~p:handle_info(data): From: ~p:~p ", [ ?MODULE, BTAddr, Channel]),

    try binary_to_term(Data) of
	Term ->
	    ?debug("~p:handle_info(data): Term: ~p", [ ?MODULE, Term]),
	    FromPid = self(),
	    spawn(fun() -> Mod:Fun(FromPid, BTAddr, Channel, 
				   data, Term, Arg) end)
    catch
	_:_ ->
	    ?warning("~p:handle_info(data): Data could not be decoded: ~pp", 
		     [ ?MODULE, Data])

    end,
%%    inet:setopts(Sock, [{active, once}]),
    {noreply, State};


handle_info({rfcomm_closed, ConnRef}, 
	    #st { remote_addr = BTAddr,
		  channel = Channel,
		  mod = Mod,
		  func = Fun,
		  args = Arg } = State) ->
    ?debug("~p:handle_info(tcp_closed): BTAddr: ~p:~p ", [ ?MODULE, BTAddr, Channel]),
    Mod:Fun(self(), BTAddr, Channel, closed, Arg),
    connection_manager:delete_connection_by_pid(self()),
    rfcomm_close:close(ConnRef),
    {stop, normal, State};


handle_info({rfcomm_error, ConnRef}, 
	    #st { remote_addr = BTAddr,
		  channel = Channel,
		  mod = Mod,
		  func = Fun,
		  args = Arg} = State) ->

    ?debug("~p:handle_info(tcp_error): BTAddr: ~p:~p ", [ ?MODULE, BTAddr, Channel]),
    Mod:Fun(self(), BTAddr, Channel, error, Arg),
    rfcomm:close(ConnRef),
    connection_manager:delete_connection_by_pid(self()),
    {stop, normal, State};


handle_info(_Info, State) ->
    ?warning("~p:handle_cast(): Unknown info: ~p", [ ?MODULE, _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ?debug("~p:terminate(): Reason: ~p ", [ ?MODULE, _Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
