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

-export([connect/5]).
-export([accept/5]).
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
	  rfcomm_ref = undefined,
	  mod = undefined,
	  func = undefined,
	  args = undefined
	 }).

%%%===================================================================
%%% API
%%%===================================================================
%% MFA is to deliver data received on the socket.

connect(Addr, Channel, Mod, Fun, Arg) ->
    gen_server:start_link(?MODULE, 
			  {connect, Addr, Channel, Mod, Fun, Arg },
			  []).

accept(Channel, ListenRef, Mod, Fun, Arg) ->
    gen_server:start_link(?MODULE, {accept, 
				    Channel, 
				    ListenRef, 
				    Mod,
				    Fun, 
				    Arg},[]).

send(Pid, Data) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Data}).
    
send(Addr, Channel, Data) ->
    case bt_connection_manager:find_connection_by_address(Addr, Channel) of
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
    case bt_connection_manager:find_connection_by_address(Addr, Channel) of
	{ok, Pid} ->
	    gen_server:call(Pid, terminate_connection);

	_Err -> not_found
    end.


is_connection_up(Pid) when is_pid(Pid) ->
    is_process_alive(Pid).

is_connection_up(Addr, Channel) ->
    case bt_connection_manager:find_connection_by_address(Addr, Channel) of
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
init({connect, BTAddr, Channel, Mod, Fun, Arg}) ->
    
    %% connect will block on rfcomm:open, so cast to self
    %% in order to let init return.
    gen_server:cast(self(), connect),
    {ok, #st{
	    remote_addr = BTAddr,
	    channel = Channel,
	    rfcomm_ref = undefined,
	    mod = Mod,
	    func = Fun,
	    args = Arg
	   }};



init({accept, Channel, ListenRef, Mod, Fun, Arg}) ->
    { ok, ARef } = rfcomm:accept(ListenRef, infinity, self()),
    ?debug("bt_connection:init(accept): self():    ~p", [self()]),
    ?debug("bt_connection:init(accept): Channel:   ~p", [Channel]),
    ?debug("bt_connection:init(accept): ListenRef: ~p", [ListenRef]),
    ?debug("bt_connection:init(accept): AcceptRef: ~p", [ARef]),
    ?debug("bt_connection:init(accept): Module:    ~p", [Mod]),
    ?debug("bt_connection:init(accept): Function:  ~p", [Fun]),
    ?debug("bt_connection:init(accept): Arg:       ~p", [Arg]),

    {ok, #st{
	    channel = Channel,
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
handle_cast(connect,  #st {
			 remote_addr = BTAddr,
			 channel = Channel,
			 mod = Mod,
			 func = Fun,
			 args = Arg
			} = St) ->

    %% Looong call that blocks for ever.
    case rfcomm:open(BTAddr, Channel) of
	{ok, ConnRef} ->
	    ?debug("bt_connection:init(connect): self():   ~p", [self()]),
	    ?debug("bt_connection:init(connect): BTAddr:   ~p", [BTAddr]),
	    ?debug("bt_connection:init(connect): Channel:  ~p", [Channel]),
	    ?debug("bt_connection:init(connect): Ref:      ~p", [ConnRef]),
	    ?debug("bt_connection:init(connect): Module:   ~p", [Mod]),
	    ?debug("bt_connection:init(connect): Function: ~p", [Fun]),
	    ?debug("bt_connection:init(connect): Arg:      ~p", [Arg]),

	    %% Add to managed connections,
	    bt_connection_manager:add_connection(BTAddr, Channel, self()),

	    Mod:Fun(self(), BTAddr, Channel, connected, Arg),

	    %% Update state with new connection
	    {noreply, St#st{
			rfcomm_ref = ConnRef
		       }};

	{ err, Error } ->
	    ?info("Failed to connect to ~p-~p", [ BTAddr, Channel]),
	    { stop, { connect_failed, Error}, St }
    end;

handle_cast({send, Data},  St) ->
    ?debug("~p:handle_call(send): Sending: ~p", 
	     [ ?MODULE, Data]),

    rfcomm:send(St#st.rfcomm_ref, Data),

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
handle_info({rfcomm, _ARef, { accept, BTAddr, _ } }, 
	    #st { mod = Mod,
		  func = Fun,
		  args = Arg,
		  channel = Channel } = St)  ->

    ?info("~p:handle_info(): bt_connection from ~w:~w\n", 
	  [?MODULE, BTAddr,Channel]),
    
    Mod:Fun(self(), BTAddr, Channel, accepted, Arg),
    { noreply, St#st { remote_addr = BTAddr, 
		       channel = Channel } };


handle_info({rfcomm, _ConnRef, {data, Data}}, 
	    #st { remote_addr = BTAddr,
		  channel = Channel,
		  mod = Mod,
		  func = Fun,
		  args = Arg } = State) ->
    ?debug("~p:handle_info(data): Data: ~p", [ ?MODULE, Data]),
    ?info("~p:handle_info(data): From: ~p:~p ", [ ?MODULE, BTAddr, Channel]),
    ?info("~p:handle_info(data): ~p:~p -> ~p:~p", 
	  [ ?MODULE, BTAddr, Channel, Mod, Fun]),
    Self = self(),
    spawn(fun() -> Mod:Fun(Self, BTAddr, Channel, 
			   data, Data, Arg) end),

    {noreply, State};


handle_info({rfcomm_closed, ConnRef}, 
	    #st { remote_addr = BTAddr,
		  channel = Channel,
		  mod = Mod,
		  func = Fun,
		  args = Arg } = State) ->
    ?debug("~p:handle_info(tcp_closed): BTAddr: ~p:~p ", [ ?MODULE, BTAddr, Channel]),
    Mod:Fun(self(), BTAddr, Channel, closed, Arg),
    bt_connection_manager:delete_connection_by_pid(self()),
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
    bt_connection_manager:delete_connection_by_pid(self()),
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
