%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2006 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% File    : obex_tcp.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : OBEX TCP API
%%%
%%% Created : 31 May 2006 by Tony Rogvall <tony@PBook.local>
%%%-------------------------------------------------------------------
-module(obex_tcp).

-behaviour(gen_server).

%% SERVER
-export([server_link/2, server/2]).
-export([stop/1]).
-export([server_accept/3]).
-export([register/3, unregister/2, lookup/2]).

-export([open/3, close/1, listen/2, accept/1, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ifdef(debug).
-define(dbg(Fmt,As), io:format("~s:~w:" Fmt "\n", [?FILE,?LINE | As])).
-else.
-define(dbg(Fmt,As), ok).
-endif.


-record(state,
	{
	  channel,     %% listen channel
	  ref,         %% ListenRef
	  apid,        %% Current accept process
	  targets=[],  %% Targets associated
	  opts         %% Options
	 }).

%%====================================================================
%% API
%%====================================================================

open(Address, Port, Opts) ->
    gen_tcp:connect(Address, Port, Opts).

listen(Port, Opts) ->
    gen_tcp:listen(Port, Opts).

accept(Listen) ->
    gen_tcp:accept(Listen).

close(Tcp) ->
    gen_tcp:close(Tcp).

send(Tcp, Data) ->
    gen_tcp:send(Tcp, Data).

register(Server, Target, Module) ->
    gen_server:call(Server, {register, Target, Module}).

unregister(Server, Target) ->
    gen_server:call(Server, {register, Target}).

lookup(Server, Target) ->
    gen_server:call(Server, {lookup, Target}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

server_link(Port, Opts) ->
    gen_server:start_link(?MODULE, [Port,Opts], []).

server(Port, Opts) ->
    gen_server:start(?MODULE, [Port,Opts], []).

stop(Server) ->
    gen_server:call(Server, stop).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Port,Opts]) ->
    case listen(Port, [binary]) of
	{ok,ListenRef} ->
	    process_flag(trap_exit, true),
	    APid = proc_lib:spawn_link(?MODULE, server_accept, [self(), ListenRef,Opts]),
	    {ok, #state { channel=Port, 
			  ref = ListenRef, 
			  apid = APid,
			  targets=opt_targets(Opts),
			  opts = Opts }};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({register, Target, Module}, _From, State) ->
    Targets = State#state.targets,
    case lists:keysearch(Target, 1, Targets) of
	false ->
	    State1 = State#state { targets = [{Target,Module}|Targets]},
	    {reply, ok, State1};
	{value,_} ->
	    {reply, {error, already_registered}, State}
    end;
handle_call({unregister, Target}, _From, State) ->
    Targets = lists:keydelete(Target, 1, State#state.targets),
    {reply, ok, State#state { targets = Targets }};
handle_call({lookup, Target}, _From, State) ->
    case lists:keysearch(Target, 1, State#state.targets) of    
	false -> {reply, {error, not_found}, State};
	{value,{_, Module}} -> {reply, {ok, Module}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, bad_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({accept,Pid,{error,closed}}, State) when Pid == State#state.apid ->
    {stop, closed, State}; %% Listen socket has closed
handle_cast({accept,Pid,Reason}, State) when Pid == State#state.apid ->
    io:format("~s: accept ~p\n", [?MODULE, Reason]),
    unlink(State#state.apid),
    APid = proc_lib:spawn_link(?MODULE, server_accept, [self(), State#state.ref, State#state.opts]),
    {noreply, State#state { apid = APid }};
handle_cast({'EXIT',Pid,Reason}, State) when Pid == State#state.apid ->
    io:format("~s: EXIT ~p\n", [?MODULE, Reason]),
    APid = proc_lib:spawn_link(?MODULE, server_accept, [self(), State#state.ref, State#state.opts]),
    {noreply, State#state { apid = APid }};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

opt_targets(Opts) ->
    opt_targets(Opts,[]).

opt_targets([{target,{Target,Module}}|Opts], Ts) 
  when is_list(Target), is_atom(Module) ->
    opt_targets(Opts, [{Target,Module}|Ts]);
opt_targets([_|Opts], Ts) ->
    opt_targets(Opts, Ts);
opt_targets([], Ts) ->
    Ts.


server_accept(Server, ListenRef, Opts) ->
    case accept(ListenRef) of
	{ok,ARef} ->
	    gen_server:cast(Server, {accept, self(), ok}),
	    {ok,{Address,_Port}} = inet:peername(ARef),
	    {ok,{_Address,Port}} = inet:sockname(ListenRef),
	    obex:server_init(Server, ARef, Address, Port, ?MODULE, Opts);
	Error ->
	    gen_server:cast(Server, {accept, self(), Error})
    end.
