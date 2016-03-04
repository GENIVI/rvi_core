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
         subscribe/0, subscribe/1, subscribe/2]).

-export([start_link/0]).

%% Gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(iface, {name,
                status = down,
                opts = []}).

-record(sub, {name, field, pid, ref}).

-record(st, {connected = false,
             ifs = [],
             subscribers = [],
             poll_ref}).

-define(BADARG, {?MODULE, '__BADARG__'}).

is_network_up() ->
    call(is_network_up).

subscribe() ->
    subscribe(all, operstate).

subscribe(Iface) ->
    subscribe(Iface, operstate).

subscribe(Iface, Field) ->
    call({subscribe, Iface, Field}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    Ref = case code:is_loaded(netlink_drv) of
              false ->
                  %% Must fake event mechanism through polling
                  start_poll_timer();
              {file, _} ->
                  netlink:subscribe("", [operstate], [flush]),
                  undefined
          end,
    Interfaces = get_interfaces(),
    {ok, #st{ifs = Interfaces,
             poll_ref = Ref}}.

handle_call(is_network_up, _From, #st{connected = Conn} = St) ->
    {reply, Conn, St};
handle_call({subscribe, Iface, Field}, {Pid, _},
            #st{subscribers = Subs} = St) ->
    Ref = erlang:monitor(process, Pid),
    {reply, ok, St#st{subscribers = [#sub{name = Iface,
                                          field = Field,
                                          pid = Pid,
                                          ref = Ref}|Subs]}};
handle_call(_, _From, St) ->
    {reply, ?BADARG, St}.

handle_cast(_, St) ->
    {noreply, St}.

handle_info({timeout, _Ref, poll}, #st{ifs = Ifs} = S) ->
    NewPollRef = start_poll_timer(),
    NewIfs = get_interfaces(),
    Diffs = lists:foldr(
              fun(#iface{name = Name, status = St}, Acc) ->
                      case lists:keyfind(Name, #iface.name, Ifs) of
                          #iface{status = St0} when St0 =/= St ->
                              [{Name, operstate, St0, St}|Acc];
                          _ ->
                              Acc
                      end
              end, [], NewIfs),
    {noreply, tell_subscribers(Diffs, S#st{ifs = NewIfs,
                                           poll_ref = NewPollRef})};
handle_info({netlink, NRef, Iface, Field, Prev, New}, St) ->
    {Prev1, New1} = adjust_status(IFace, Field, Prev, New),
    {noreply, tell_subscribers([{Iface, Field, Prev1, New1}], St)};
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

tell_subscribers(Evts, #st{subscribers = Subs} = St) ->
    lists:foreach(
      fun({Name, Field, Old New}) ->
              [Pid ! {rvi_netlink, Ref, Name, Field, Old New}
               || #sub{name = N, pid = Pid, ref = Ref} <- Subs,
                  match_name(N, Name)]
      end, Evts),
    St.

get_interfaces() ->
    case inet:getifaddrs() of
        {ok, IFs} ->
            [if_entry(I) || {_, Flags} <- IFs];
        Error ->
            ?error("getifaddrs() -> ~p", [Error]),
            []
    end.

if_entry({Name, Opts}) ->
    #iface{name = Name,
        status = if_status(Opts),
        opts = Opts}.

if_status(Opts) ->
    case lists:member(up, opt(flags, Opts, [])) of
        true -> up;
        false -> down
    end.

adjust_status(IF, operstate, A, B) ->
    {adjust_operstate(A, IF), adjust_operstate(B, IF)};
adjust_status(_, A, B) ->
    {A, B}.

adjust_operstate(undefined,  _) -> down;
adjust_operstate(unknown, "lo") -> up;
adjust_operstate(State,      _) -> State.

opt(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Val} ->
            Val;
        false ->
            Default
    end.

match_name(_, all) -> true;
match_name(N, N  ) -> true;
match_name(A, B) when is_binary(A), is_list(B) ->
    binary_to_list(A) == B;
match_name(_, _) ->
    false.
            
start_poll_timer() ->
    erlang:start_timer(?POLL_INTERVAL, self(), poll).
