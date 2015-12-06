%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    General socket server
%%% @end
%%% Created : 22 Aug 2011 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(exo_socket_server).

-behaviour(gen_server).

%%
%% methods
%%   init(Socket, Args) ->
%%      {ok, State'}
%%      {stop, Reason, State'}
%%
%%   data(Socket, Data, State) ->
%%      {ok, State'}
%%      {stop, Reason, State'};
%%
%%   close(Socket, State) ->
%%      {ok, State'}
%%
%%   error(Socket, Error, State) ->
%%      {ok, State'}
%%      {stop, Reason, State'}
%%

%% API
-export([start_link/5, start_link/6]).
-export([start/5, start/6]).
-export([reusable_sessions/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([behaviour_info/1]).

-include("exo_socket.hrl").
-include("log.hrl").

%% -define(debug(Fmt,Args), ok).
%% -define(error(Fmt,Args), error_logger:format(Fmt, Args)).

-define(SERVER, ?MODULE).

-record(state, {
	  listen,    %% #exo_socket{}
	  active,    %% default active mode for socket
	  socket_reuse = none,  %% 'none' | #reuse{}
	  ref,       %% prim_inet internal accept ref number
	  module,    %% session module
	  args       %% session init args
	 }).

-record(reuse, {
	  mode,
	  port,
	  sessions = dict:new(),
	  session_pids = dict:new(),
	  state
	 }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% The plugin behaviour:<br>
%% init(Socket::socket(), Args::[term()] <br>
%%   -> {ok,NewState::state()} | <br>
%%      {stop,Reason::term(),NewState::state()}<br>
%% data(Socket::socket(), Data::io_list(), State::state()) <br>
%%   -> {ok,NewState::state()}|<br>
%%      {close,NewState::state()}|<br>
%%      {stop,Reason::term(),NewState::state()}<br>
%% close(Socket::socket(), State::state())<br>
%%   -> {ok,state()}<br>
%% error(Socket::socket(),Error::error(), State::state())<br>
%%   -> {ok,NewState::state()} | <br>
%%      {stop,Reason::term(),NewState::state()}<br>
%% control(Socket::socket(), Request::term(), From::term(), State::state())<br>
%%   -> {reply, Reply::term(),NewState::state()} | <br>
%%      {noreply, NewState::state()} |<br>
%%      {ignore, NewState::state()} | <br>
%%      {send, Bin::binary(), NewState::state()} |<br>
%%      {data, Data::term(), NewState::state()} |<br>
%%      {stop, Reason::term(),NewState::state()}<br>
%% @end
%%--------------------------------------------------------------------
-spec behaviour_info(callbacks) -> list().
behaviour_info(callbacks) ->
    [
     {init,  2},  %% init(Socket::socket(), Args::[term()]
                  %%   -> {ok,state()} | {stop,reason(),state()}
     {data,  3},  %% data(Socket::socket(), Data::io_list(), State::state())
                  %%   -> {ok,state()}|{close,state()}|{stop,reason(),state()}
     {close, 2},  %% close(Socket::socket(), State::state())
                  %%   -> {ok,state()}
     {error, 3},  %% error(Socket::socket(),Error::error(), State:state())
                  %%   -> {ok,state()} | {stop,reason(),state()}
     {control, 4} %% control(Socket::socket(), Request::term(),
                  %%         From::term(), State:state())
                  %%   -> {reply, Reply::term(),state()} | {noreply, state()} |
                  %%      {ignore, state()} | {send, Bin::binary(), state()} |
                  %%      {data, Data::trem()} |{stop,reason(),state()}
    ];
behaviour_info(_Other) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------

start_link(Port, Protos, Options, Module, Args) ->
    gen_server:start_link(?MODULE, [Port,Protos,Options,Module,Args], []).

start_link(ServerName, Protos, Port, Options, Module, Args) ->
    gen_server:start_link(ServerName, ?MODULE, [Port,Protos,Options,Module,Args], []).

start(Port, Protos, Options, Module, Args) ->
    gen_server:start(?MODULE, [Port,Protos,Options,Module,Args], []).

start(ServerName, Protos, Port, Options, Module, Args) ->
    gen_server:start(ServerName, ?MODULE, [Port,Protos,Options,Module,Args], []).

-spec reusable_sessions(Process::pid() | atom()) ->
	   list({{IpAddress::tuple(), Port::integer}, Pid::pid}).

reusable_sessions(P) ->
    gen_server:call(P, reusable_sessions).

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
init([Port,Protos,Options,Module,Args] = _X) ->
    ?debug("~p: init(~p)~n", [?MODULE, _X]),
    Active = proplists:get_value(active, Options, true),
    ReuseMode = proplists:get_value(reuse_mode, Options, none),
    Options1 = proplists:delete(reuse_mode, proplists:delete(active, Options)),
    Reuse = case ReuseMode of
		none -> none;
		_ when ReuseMode==client; ReuseMode==server ->
		    {ok, RUSt} = Module:reuse_init(ReuseMode, Args),
		    #reuse{mode = ReuseMode,
			   port = Port,
			   state = RUSt}
	    end,
    case exo_socket:listen(Port,Protos,Options1) of
	{ok,Listen} ->
	    case exo_socket:async_accept(Listen) of
		{ok, Ref} ->
		    {ok, #state{ listen = Listen,
				 active = Active,
				 socket_reuse = Reuse,
				 ref=Ref,
				 module=Module,
				 args=Args
			       }};
		{error, Reason} ->
		    {stop,Reason}
	    end;
	{error,Reason} ->
	    {stop,Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages. <br>
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(),
		  From::{pid(), Tag::term()},
		  State::#state{}) ->
			 {reply, Reply::term(), State::#state{}} |
			 {noreply, State::#state{}} |
			 {stop, Reason::atom(), Reply::term(), State::#state{}}.

handle_call({get_session, Host, Port, Opts}, From,
	    #state{socket_reuse = Reuse} = State) ->
    Key = {Host, Port},
    case Reuse of
	none ->
	    {reply, connect, State};
	#reuse{mode = client, sessions = Sessions,
	       session_pids = Pids} = R ->
	    case dict:find(Key, Sessions) of
		error ->
		    ConnPid = start_connector(Host, Port, Opts, self(), State),
		    Sessions1 = dict:store(Key, {ConnPid, [From]}, Sessions),
		    Pids1 = dict:store(ConnPid, Key, Pids),
		    R1 = R#reuse{sessions = Sessions1,
				 session_pids = Pids1},
		    {noreply, State#state{socket_reuse = R1}};
		{ok, Pid} when is_pid(Pid) ->
		    {reply, Pid, State};
		{ok, {CPid, Pending}} ->
		    Sessions1 = dict:store(
				  Key, {CPid, [From|Pending]}, Sessions),
		    R1 = R#reuse{sessions = Sessions1},
		    {noreply, State#state{socket_reuse = R1}}
	    end;
	#reuse{mode = server, sessions = Sessions} ->
	    case dict:find(Key, Sessions) of
		error ->
		    %% server never initiates connection when in reuse mode
		    {reply, rejected, State};
		{ok, Pid} when is_pid(Pid) ->
		    {reply, Pid, State}
	    end
    end;
handle_call(reusable_sessions, _From, #state{socket_reuse = R} = State) ->
    case R of
	#reuse{sessions = Sessions} ->
	    {reply, dict:to_list(Sessions), State};
	_ ->
	    {reply, [], State}
    end;
handle_call(Request, _From, State) ->
    ?debug("~p: handle_call(~p) not implemented!!", [?MODULE, Request]),
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
handle_cast(_Msg, State) ->
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
handle_info({inet_async, LSocket, Ref, {ok,Socket}} = _Msg, State) when
      (State#state.listen)#exo_socket.socket =:= LSocket,
      Ref =:= State#state.ref ->
    ?debug("<-- ~p~n", [_Msg]),
    Listen = State#state.listen,
    AcceptTimeout = proplists:get_value(accept_timeout, Listen#exo_socket.opts, infinity),
    NewAccept = exo_socket:async_accept(Listen),
    case exo_socket:async_socket(Listen, Socket, [{delay_auth, true}]) of
	{ok, XSocket} ->
	    F = fun() ->
			exo_socket:accept(
			  XSocket, tl(XSocket#exo_socket.protocol), AcceptTimeout)
		end,
	    XSocketFun = {XSocket, F},
	    case exo_socket_session:start(XSocketFun,
					  State#state.module,
					  State#state.args) of
		{ok, Pid} ->
		    exo_socket:controlling_process(XSocket, Pid),
		    gen_server:cast(Pid, {activate,State#state.active});
		_Error ->
		    exo_socket:close(XSocket)
	    end;
	_Error ->
	    error
    end,
    case NewAccept of
	{ok, Ref1} ->
	    {noreply, State#state { ref = Ref1 }};
	{error, Reason} ->
	    {stop, Reason, State}
    end;
%% handle {ok,Socket} on bad ref ?
handle_info({inet_async, _LSocket, Ref, {error,Reason}}, State) when
      Ref =:= State#state.ref ->
    case exo_socket:async_accept(State#state.listen) of
	{ok,Ref} ->
	    {noreply, State#state { ref = Ref }};
	{error, Reason} ->
	    {stop, Reason, State}
	    %% {noreply, State#state { ref = undefined }}
    end;
handle_info({Pid, ?MODULE, connected, Host, Port},
	    #state{socket_reuse = #reuse{sessions = Sessions} = R} = State) ->
    Session = dict:fetch(Key = {Host, Port}, Sessions),
    case Session of
	{_, Pending} ->
	    [gen_server:reply(From, Pid) || From <- Pending];
	_ -> ok
    end,
    Sessions1 = dict:store(Key, Pid, Sessions),
    %% Pids = dict:store(Pid, {Host,Port}, R#reuse.session_pids),
    R1 = R#reuse{sessions = Sessions1},
    {noreply, State#state{socket_reuse = R1}};
handle_info({Pid, reuse, Config},
	    #state{socket_reuse = #reuse{mode = server,
					 sessions = Sessions,
					 session_pids = Pids} = R} = State) ->
    {_, Port} = lists:keyfind(port, 1, Config),
    case [H || {host, H} <- Config] of
	[Host|_] ->
	    %% we could possibly handle aliases, and thus multiple host names
	    Key = {Host, Port},
	    Sessions1 = dict:store(Key, Pid, Sessions),
	    Pids1 = dict:store(Pid, Key, Pids),
	    R1 = R#reuse{sessions = Sessions1, session_pids = Pids1},
	    {noreply, State#state{socket_reuse = R1}};
	_Other ->
	    ?error("strange reuse config: ~p~n", [_Other]),
	    {noreply, State}
    end;
handle_info({'DOWN', _, process, Pid, _},
	    #state{socket_reuse = #reuse{sessions = Sessions,
					 session_pids = Pids} = R} = State) ->
    ?debug("~p got DOWN - Pid = ~p~n"
	   "Sessions = ~p~n"
	   "Pids = ~p~n", [?MODULE, Pid, dict:to_list(Sessions),
			   dict:to_list(Pids)]),
    case dict:find(Pid, Pids) of
	error ->
	    {noreply, State};
	{ok, {_Host,_Port} = Key} ->
	    Session = dict:fetch(Key, Sessions),
	    case Session of
		{_, Pending} ->
		    [gen_server:reply(From, rejected) || From <- Pending];
		_ -> ok
	    end,
	    Sessions1 = dict:erase(Key, Sessions),
	    Pids1 = dict:erase(Pid, Pids),
	    R1 = R#reuse{sessions = Sessions1, session_pids = Pids1},
	    {noreply, State#state{socket_reuse = R1}}
    end;
handle_info(_Info, State) ->
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
terminate(_Reason, State) ->
    exo_socket:close(State#state.listen),
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


start_connector(Host, Port, ConnArgs, Parent,
		#state{module = M, args = Args, active = Active,
		       socket_reuse = #reuse{port = MyPort,
					     state = RUSt}}) ->
    F = fun() ->
		case open_reuse_connector(Host, Port, ConnArgs) of
		    {ok, XSocket} ->
			send_reuse_message(
			  Host, Port, Args, M, MyPort, XSocket, RUSt),
			case exo_socket_session:init(
			       [XSocket, M, Args]) of
			    {ok, XSt} ->
				{noreply, XSt1} =
				    exo_socket_session:handle_cast(
				      {activate, Active}, XSt),
				Parent ! {self(), ?MODULE, connected,
					  Host, Port},
				gen_server:enter_loop(
				  exo_socket_session, [], XSt1);
			    {error, InitError} ->
				exit({InitError, [{exo_socket_session,init}]})
			end;
		    {error, ConnectError} ->
			exit({ConnectError, [{exo_socket, connect}]})
		end
	end,
    Pid = proc_lib:spawn(F),
    erlang:monitor(process, Pid),
    Pid.

open_reuse_connector(Host, Port, ConnArgs) ->
    case ConnArgs of
	[Protos, Opts, Timeout] ->
	    exo_socket:connect(
	      Host, Port, Protos, Opts, Timeout);
	[Protos, Opts] ->
	    exo_socket:connect(Host, Port, Protos, Opts)
    end.

send_reuse_message(Host, Port, Args, M, MyPort, XSocket, RUSt) ->
    ReuseOpts = M:reuse_options(Host, Port, Args, RUSt),
    ReuseMsg = exo_socket_session:encode_reuse(
		 MyPort, ReuseOpts),
    exo_socket:send(XSocket, ReuseMsg).
