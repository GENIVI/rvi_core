%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%     Cache open sockets that can be reused (like http/https connections)
%%% @end
%%% Created : 16 Dec 2011 by Tony Rogvall <tony@rogvall.se>

-module(exo_socket_cache).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("exo_socket.hrl").

-define(DEFAULT_CACHE_SIZE, 256).

-compile(export_all).

-ifdef(debug).
-define(dbg(F, A), io:format((F), (A))).
-else.
-define(dbg(F, A), ok).
-endif.

%%--------------------------------------------------------------------
%% External exports
-export([start/0, start_link/0, stop/0]).
-export([open/4, open/5, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, 
	{
	  cache_size = ?DEFAULT_CACHE_SIZE,
	  ref_tab,  %% bag: {Proto,Vsn,Addr,Port} => Socket
	  sock_tab  %% set: Socket => {Proto,Vsn,Addr,Port},#exo_socket
	 }).

-define(SERVER, exo_socket_cache).

%%====================================================================
%% External functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

open(Proto, Vsn, Host, Port) ->
    open(Proto, Vsn, Host, Port, infinity).


open(Proto, Vsn, Host, Port, Timeout) ->
    start(),  %% fixme: add is supervisor
    case inet:getaddrs(Host, inet, Timeout) of
	{ok,IPs} ->
	    case gen_server:call(?SERVER,{alloc,Proto,Vsn,IPs,Port,self()}) of
		{ok, HS} ->
		    {ok,HS};
		{error,_} ->
		    connect(IPs,Port,Proto,Vsn,[{active,false}],
			    Timeout, undefined)
	    end;
	Error ->
	    Error
    end.

close(HS) ->
    exo_socket:setopts(HS, [{active,false}]),
    case sync_socket(HS) of
	true ->
	    exo_socket:controlling_process(HS,whereis(?SERVER)),
	    case gen_server:call(?SERVER, {release, HS}) of
		ok -> 
		    ?dbg("close: socket save ~p\n", [HS]),
		    ok;
		Error ->
		    ?dbg("close: socket ignored ~p: ~p\n", [HS,Error]),
		    exo_socket:close(HS),
		    Error
	    end;
	false ->
	    Error = {error,sync_error},
	    ?dbg("close: socket ignored ~p: ~p\n", [HS,Error]),
	    exo_socket:close(HS),
	    Error
    end.
	    
connect([IP|IPs],Port,Proto,Vsn,Opts,Timeout,_Err) ->
    case exo_socket:connect(IP, Port, Proto, Opts, Timeout) of
	{ok, S} ->
	    {ok, S#exo_socket { version = Vsn }};
	{error,Reason} ->
	    connect(IPs,Port,Proto,Vsn,Opts,Timeout,Reason)
    end;
connect([],_Port,_Proto,_Vsn,_Opts,_Timeout,Reason) ->
    {error, Reason}.
    
%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    RefTab = ets:new(exo_socket_cache_ref, [bag]),
    SockTab = ets:new(exo_socket_cache_sock, [set]),
    {ok, #state{ ref_tab = RefTab, sock_tab = SockTab }}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_call({alloc,Proto,Vsn,IPs,Port,Pid}, _From, State) ->
    case alloc(Proto,Vsn,IPs,Port,State) of
	{ok,HS} ->
	    exo_socket:controlling_process(HS, Pid),
	    {reply, {ok,HS}, State};
	Error ->
	    {reply, Error, State}
    end;

handle_call({release,HS}, _From, State) ->
    %% If any thing comes from the socket it will be closed, while
    %% in the session cache.
    exo_socket:setopts(HS, [{active,once},{packet,0}]),
    case exo_socket:peername(HS) of
	{ok, {Address, Port}} ->
	    #exo_socket{socket=S,version=Vsn,protocol=Proto} = HS,
	    Key = {Proto,Vsn,Address,Port},
	    ets:insert(State#state.sock_tab, {S,Key,HS}),
	    ets:insert(State#state.ref_tab,{Key,S}),
	    {reply, ok, State};
	Error ->
	    {reply, Error, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, bad_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%% Any kind of activity from a socket in the cache will
%% lead to it's death.
handle_info({tcp,S,_}, State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({tcp_closed,S}, State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({tcp_error,S,_},State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({ssl,S,_},State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({ssl_closed,S},State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({ssl_error,S,_},State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Locate a persistent socket connection to server IP:Port
%% using protocol Proto and version Vsn
alloc(Proto, Vsn,[IP|IPs], Port, State) ->
    Refs = ets:lookup(State#state.ref_tab,{Proto,Vsn,IP,Port}),
    alloc_ref(Refs,Proto,Vsn,IPs,Port,State);
alloc(_Proto, _Vsn, [], _Port, _State) ->
    {error, not_found}.

alloc_ref([{Key,S}|Refs],Proto,Vsn,IPs,Port,State) ->
    [{_,_,HS}] = ets:lookup(State#state.sock_tab, S),
    ets:delete_object(State#state.ref_tab, {Key,S}),
    ets:delete(State#state.sock_tab, S),
    exo_socket:setopts(HS, [{active, false}]),
    case sync_socket(HS) of
	false ->
	    ?dbg("alloc: socket unsynced ~p\n", [HS]),
	    alloc_ref(Refs,Proto,Vsn,IPs,Port,State);
	true ->
	    ?dbg("alloc: got socket socket ~p\n", [HS]),
	    {ok, HS}
    end;
alloc_ref([],Proto,Vsn,IPs,Port,State) ->
    alloc(Proto,Vsn,IPs,Port,State).

%%
%% Check that no events from the socket is received
%% (after passive is set to true)
%%
sync_socket(#exo_socket { socket=S, tags={Tag,Tag_closed,Tag_error}}) ->
    receive
	{Tag,S,_}       -> false;
	{Tag_closed,S}  -> false;
	{Tag_error,S,_} -> false
	%% 
    after 0 ->
	    true
    end.

delete_socket(S,State) ->
    case ets:lookup(State#state.sock_tab, S) of
	[] -> 
	    ok;
	[{_,Key,HS}] ->
	    ?dbg("socket ~p ~p delete\n", [Key,HS]),
	    exo_socket:close(HS),
	    ets:delete(State#state.sock_tab, S),
	    ets:delete_object(State#state.ref_tab,{Key,S}),
	    ok
    end.
