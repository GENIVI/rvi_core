%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%
-module(rvi_netlink).
-behaviour(gen_server).

-export([is_network_up/0,
         subscribe/0]).

-export([start_link/0]).

%% Gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(st, {connected = false,
             subscribers = []}).

-define(BADARG, {?MODULE, '__BADARG__'}).

is_network_up() ->
    call(is_network_up).

subscribe() ->
    call(subscribe).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #st{}}.

handle_call(is_network_up, _From, #st{connected = Conn} = St) ->
    {reply, Conn, St};
handle_call(subscribe, {Pid, _}, #st{subscribers = Subs} = St) ->
    Ref = erlang:monitor(process, Pid),
    {reply, ok, St#st{subscribers = [{Pid, Ref}|Subs]}};
handle_call(_, _From, St) ->
    {reply, ?BADARG, St}.

handle_cast(_, St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

call(Req) ->
    case gen_server:call(?MODULE, Req) of
        ?BADARG ->
            error(badarg);
        Reply ->
            Reply
    end.
