%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%
%%
%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.home>
%%% @copyright (C) 2014, magnus
%%% @doc
%%%
%%% @end
%%% Created : 12 Sep 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(dlink_tls_connmgr).

-behaviour(gen_server).
-include_lib("lager/include/log.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([add_connection/3]).
-export([delete_connection_by_pid/1]).
-export([delete_connection_by_address/2]).
-export([find_connection_by_pid/1]).
-export([find_connection_by_address/2]).
-export([connections/0]).

-define(SERVER, ?MODULE).

-record(st, {
	  conn_by_pid = undefined,
	  conn_by_addr = undefined
	 }).

%%%===================================================================
%%% API
%%%===================================================================

add_connection(IP, Port, Pid) ->
    gen_server:call(?SERVER, { add_connection, IP, Port, Pid}).

delete_connection_by_pid(Pid) ->
    gen_server:call(?SERVER, { delete_connection_by_pid, Pid } ).

delete_connection_by_address(IP, Port) ->
    gen_server:call(?SERVER, { delete_connection_by_address, IP, Port } ).

find_connection_by_pid(Pid) ->
    gen_server:call(?SERVER, { find_connection_by_pid, Pid } ).

find_connection_by_address(IP, Port) ->
    gen_server:call(?SERVER, { find_connection_by_address, IP, Port } ).

connections() ->
    gen_server:call(?SERVER, connections).

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
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #st{
	    conn_by_pid = dict:new(),  %% All managed connection stored by pid
	    conn_by_addr = dict:new()  %% All managed connection stored by address
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
handle_call({add_connection, IP, Port, Pid}, _From,
	    #st { conn_by_pid = ConPid,
		  conn_by_addr = ConAddr} = St) ->

    ?debug("~p:handle_call(add): Adding Pid: ~p, Address: ~p",
	     [ ?MODULE, Pid, { IP, Port }]),
    %% Store so that we can find connection both by pid and by address
    NConPid = dict:store(Pid, { IP, Port }, ConPid),
    NConAddr = dict:store({ IP, Port }, Pid, ConAddr),

    NSt = St#st { conn_by_pid = NConPid,
		  conn_by_addr = NConAddr },
    {reply, ok, NSt};

%% Delete connection by pid
handle_call({delete_connection_by_pid, Pid}, _From,
	    #st { conn_by_pid = ConPid,
		  conn_by_addr = ConAddr} = St) when is_pid(Pid)->

    %% Find address associated with Pid
    case dict:find(Pid, ConPid) of
	error ->
	    ?debug("~p:handle_call(del_by_pid): not found: ~p",
		   [ ?MODULE, Pid]),
	    { reply, not_found, St};

	{ok, Addr } ->
	    ?debug("~p:handle_call(del_by_pid): deleted Pid: ~p, Address: ~p",
		   [ ?MODULE, Pid, Addr]),

	    NConPid = dict:erase(Pid, ConPid),
	    NConAddr = dict:erase(Addr, ConAddr),

	    NSt = St#st { conn_by_pid = NConPid,
			  conn_by_addr = NConAddr },

	    {reply, ok, NSt}
    end;


%% Delete connection by address
handle_call({ delete_connection_by_address, IP, Port}, _From,
	    #st { conn_by_pid = ConPid,
		  conn_by_addr = ConAddr} = St) ->

    %% Find Pid associated with Address
    case dict:find({IP, Port}, ConAddr) of
	error ->
	    ?debug("~p:handle_call(del_by_addr): not found: ~p",
		   [ ?MODULE, {IP, Port}]),
	    { reply, not_found, St};

	{ok, Pid } ->
	    ?debug("~p:handle_call(del_by_addr): deleted Pid: ~p, Address: ~p",
		   [ ?MODULE, Pid, {IP, Port}]),
	    NConPid = dict:erase(Pid, ConPid),
	    NConAddr = dict:erase({ IP, Port }, ConAddr),
	    NSt = St#st { conn_by_pid = NConPid,
			  conn_by_addr = NConAddr },
	    {reply, ok, NSt}
    end;


%% Find connection by pid
handle_call({ find_connection_by_pid, Pid}, _From,
	    #st { conn_by_pid = ConPid} = St) when is_pid(Pid)->

    %% Find address associated with Pid
    case dict:find(Pid, ConPid) of
	error ->
	    ?debug("~p:handle_call(find_by_pid): not found: ~p",
		   [ ?MODULE, Pid]),
	    { reply, not_found, St};

	{ok, {IP, Port} } ->
	    ?debug("~p:handle_call(find_by_addr): Pid: ~p ->: ~p",
		   [ ?MODULE, Pid, {IP, Port}]),
	    {reply, {ok, IP, Port}, St}
    end;

%% Find connection by address
handle_call({find_connection_by_address, IP, Port}, _From,
	    #st { conn_by_addr = ConAddr} = St) ->

    %% Find address associated with Pid
    case dict:find({IP, Port}, ConAddr) of
	error ->
	    ?debug("~p:handle_call(find_by_addr): not found: ~p",
		   [ ?MODULE, {IP, Port}]),

	    { reply, not_found, St};

	{ok, Pid } ->
	    ?debug("~p:handle_call(find_by_addr): Addr: ~p ->: ~p",
		   [ ?MODULE, {IP, Port}, Pid]),
	    {reply, {ok, Pid}, St}
    end;

handle_call(connections, _From, #st{conn_by_addr = ConAddr} = St) ->
    {reply, [Addr || {Addr, _} <- dict:to_list(ConAddr)], St};

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
