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
-module(bt_connection_manager).

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

-define(SERVER, ?MODULE).

-record(st, {
	  cs,
	  conn_by_pid = undefined,
	  conn_by_addr = undefined
	 }).

%%%===================================================================
%%% API
%%%===================================================================

add_connection(BTAddr, Channel, Pid) ->
    gen_server:call(?SERVER, { add_connection, BTAddr, Channel, Pid}).

delete_connection_by_pid(Pid) ->
    gen_server:call(?SERVER, { delete_connection_by_pid, Pid } ).

delete_connection_by_address(BTAddr, Channel) ->
    gen_server:call(?SERVER, { delete_connection_by_address, BTAddr, Channel } ).

find_connection_by_pid(Pid) ->
    gen_server:call(?SERVER, { find_connection_by_pid, Pid } ).

find_connection_by_address(BTAddr, Channel) ->
    gen_server:call(?SERVER, { find_connection_by_address, BTAddr, Channel } ).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    ?debug("start_link()", []),
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
	    cs = rvi_common:get_component_specification(),
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
handle_call({add_connection, BTAddr, Channel, Pid}, _From,
	    #st { conn_by_pid = ConPid,
		  conn_by_addr = ConBTAddr} = St) ->

    ?debug("~p:handle_call(add): Adding Pid: ~p, BTAddress: ~p",
	     [ ?MODULE, Pid, { BTAddr, Channel }]),
    %% Store so that we can find connection both by pid and by address
    NConPid = dict:store(Pid, { BTAddr, Channel }, ConPid),
    NConBTAddr = dict:store({ BTAddr, Channel }, Pid, ConBTAddr),
    erlang:monitor(process, Pid),
    NSt = St#st { conn_by_pid = NConPid,
		  conn_by_addr = NConBTAddr },
    {reply, ok, NSt};

%% Delete connection by pid
handle_call({delete_connection_by_pid, Pid}, _From, St) when is_pid(Pid)->
    {Res, NSt} = delete_connection_by_pid_(Pid, St),
    {reply, Res, NSt};

%% Delete connection by address
handle_call({ delete_connection_by_address, BTAddr, Channel}, _From,
	    #st { conn_by_pid = ConPid,
		  conn_by_addr = ConBTAddr} = St) ->

    %% Find Pid associated with BTAddress
    case dict:find({BTAddr, Channel}, ConBTAddr) of
	error ->
	    ?debug("~p:handle_call(del_by_addr): not found: ~p",
		   [ ?MODULE, {BTAddr, Channel}]),
	    { reply, not_found, St};

	{ok, Pid } ->
	    ?debug("~p:handle_call(del_by_addr): deleted Pid: ~p, BTAddress: ~p",
		   [ ?MODULE, Pid, {BTAddr, Channel}]),
	    NConPid = dict:erase(Pid, ConPid),
	    NConBTAddr = dict:erase({ BTAddr, Channel }, ConBTAddr),
	    NSt = St#st { conn_by_pid = NConPid,
			  conn_by_addr = NConBTAddr },
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

	{ok, {BTAddr, Channel} } ->
	    ?debug("~p:handle_call(find_by_addr): Pid: ~p ->: ~p",
		   [ ?MODULE, Pid, {BTAddr, Channel}]),
	    {reply, {ok, BTAddr, Channel}, St}
    end;

%% Find connection by address
handle_call({find_connection_by_address, BTAddr, Channel}, _From,
	    #st { conn_by_addr = ConBTAddr} = St) ->

    %% Find address associated with Pid
    case dict:find({BTAddr, Channel}, ConBTAddr) of
	error ->
	    ?debug("~p:handle_call(find_by_addr): not found: ~p",
		   [ ?MODULE, {BTAddr, Channel}]),

	    { reply, not_found, St};

	{ok, Pid } ->
	    ?debug("~p:handle_call(find_by_addr): BTAddr: ~p ->: ~p",
		   [ ?MODULE, {BTAddr, Channel}, Pid]),
	    {reply, {ok, Pid}, St}
    end;


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
handle_info({'DOWN', _Ref, process, Pid, _}, St) ->
    {_, NSt} = delete_connection_by_pid_(Pid, St),
    {noreply, NSt};
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

delete_connection_by_pid_(Pid, #st{conn_by_pid = ConPid,
				   conn_by_addr = ConBTAddr,
				   cs = CS} = St) ->
    %% Find address associated with Pid
    case dict:find(Pid, ConPid) of
	error ->
	    ?debug("del_by_pid: not found: ~p", [Pid]),
	    {not_found, St};

	{ok, BTAddr } ->
	    ?debug("del_by_pid: deleted Pid: ~p, BTAddress: ~p", [Pid, BTAddr]),

	    NConPid = dict:erase(Pid, ConPid),
	    NConBTAddr = dict:erase(BTAddr, ConBTAddr),

	    NSt = St#st { conn_by_pid = NConPid,
			  conn_by_addr = NConBTAddr },
	    authorize_rpc:remove_connection(CS, BTAddr),
	    {ok, NSt}
    end.
