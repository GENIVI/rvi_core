%% -*- mode: erlang; indent-tabs-mode: nil; -*-
%%=============================================================================
%%
%% Copyright (C) 2015, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%
-module(rvi_msgpack_rpc).

-export([start_link/1,
         start_link/3,
         start_link/4,
         start_link/5]).

-export([call/3,
         async_call/3,
         notify/3,
         join/2]).

-export([control/4]).

-include("rvi_msgpack_rpc.hrl").
-include_lib("lager/include/log.hrl").

-record(st, {pending = [], id = 1, opts, buf = <<>>,
             msgpack_opts = []}).

start_link(Opts) ->
    [IP, Port, ExoOpts, Rest] = rvi_common:take([ip, port, {exo, []}], Opts),
    start_link(IP, Port, Rest, ExoOpts).

start_link(Host, Port, Opts) ->
    [Exo, Rest] = rvi_common:take([{exo, []}], Opts),
    start_link(Host, Port, Rest, Exo).

start_link(Host, Port, Opts, ExoOpts) ->
    start_link(Host, Port, Opts, protos(ExoOpts), ExoOpts).

start_link(Host, Port, Opts, Protos, ExoOpts) ->
    ConnectTimeout = opt([connect_timeout, timeout], Opts, ?CONNECT_TIMEOUT),
    case exo_socket:connect(Host, Port, Protos, ExoOpts, ConnectTimeout) of
        {ok, Socket} ->
            {ok, Pid} = exo_socket_session:start_link(Socket, ?MODULE, {Host, Port, Opts}),
            exo_socket:controlling_process(Socket, Pid),
            gen_server:cast(Pid, {activate, once}),
            {ok, Pid};
        Error ->
            Error
    end.

call(Pid, Method, Args) ->
    call(Pid, Method, Args, ?CALL_TIMEOUT).

call(Pid, Method, Args, Timeout) ->
    gen_server:call(Pid, {call, Method, Args}, Timeout).

async_call(Pid, Method, Args) ->
    Ref = erlang:monitor(process, Pid),
    ok = gen_server:call(Pid, {async_call, {self(), Ref}, Method, Args}),
    Ref.

notify(Pid, Method, Args) ->
    gen_server:call(Pid, {notify, Method, Args}).

join(Ref) ->
    join(Ref, ?CALL_TIMEOUT).

join(Ref, Timeout) ->
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref),
            Reply;
        {'DOWN', Ref, _, _, Reason} ->
            error(Reason)
    after Timeout ->
            error(timeout)
    end.

init({Host, Port, Opts}) ->
    MsgPackOpts = opt([msgpack], Opts, rvi_msgpack_rpc_server:msgpack_options()),
    gproc:reg({p,l,msgpack_rpc_client}, {Host, Port}),
    case lists:keyfind(gproc, 1, Opts) of
        {_, Reg} -> gproc:reg(Reg, {Host, Port});
        false    -> ok
    end,
    {ok, #st{opts = Opts,
             msgpack_opts = MsgPackOpts}}.

control(XSocket, Req, From, St) ->
    try control_(XSocket, Req, From, St)
    catch
        error:Reason ->
            {reply, {error, Reason}, St}
    end.

control_(XSocket, {call, Method, Args}, From,
        #st{pending = Pending, id = ID, msgpack_opts = MOpts} = St) ->
    pack_and_send(XSocket, [?TYPE_REQUEST, ID, Method, Args], MOpts),
    {noreply, St#st{pending = [{ID, From}|Pending], id = ID+1}};
control_(XSocket, {async_call, From, Method, Args}, _,
         #st{pending = Pending, msgpack_opts = MOpts, id = ID} = St) ->
    pack_and_send(XSocket, [?TYPE_REQUEST, ID, Method, Args], MOpts),
    {reply, ok, St#st{pending = [{ID, From}|Pending], id = ID+1}};
control_(XSocket, {notify, Method, Args}, _, #st{msgpack_opts = MOpts} = St) ->
    pack_and_send(XSocket, [?TYPE_NOTIFY, Method, Args], MOpts),
    {reply, ok, St};
control_(_, _, _, St) ->
    {reply, {error, unsupported}, St}.

pack_and_send(XSocket, Msg, MOpts) ->
        Data = msgpack:pack(Msg, MOpts),
        exo_socket:send(XSocket, Data).

data(XSocket, Data, #st{buf = Buf, pending = Pending,
                        msgpack_opts = MOpts} = St) ->
    Buf1 = <<Buf/binary, Data/binary>>,
    try msgpack:unpack_stream(Buf1, MOpts) of
        {[?TYPE_RESPONSE, ID, Error, Result], Rest} ->
            case lists:keytake(ID, 1, Pending) of
                {value, {_, From}, Pending1} ->
                    Reply = case Error of
                                null -> {ok, Result};
                                _    -> {error, Error}
                            end,
                    gen_server:reply(From, Reply),
                    {ok, St#st{pending = Pending1, buf = Rest}};
                false ->
                    {ok, St#st{buf = Rest}}
            end;
        {error, incomplete} ->
            {ok, St#st{buf = Buf1}};
        {error, Reason} ->
            {ok, St#st{buf = <<>>}}
    catch
        error:Reason ->
            ?debug("unpack CRASH: ~p", [Reason]),
            {ok, St#st{buf = <<>>}}
    end.

opt([H|T], Opts, Default) ->
    case lists:keyfind(H, 1, Opts) of
        {_, Value}       -> Value;
        false when T==[] -> Default;
        false ->
            opt(T, Opts, Default)
    end.


protos(Opts) ->
    case [1 || {K,_} <- Opts, lists:member(K, ssl_connect_opts())] of
        [] ->
            [tcp];
        [_|_] ->
            [tcp, ssl]
    end.

%% Copied from exo_socket.erl
ssl_connect_opts() ->
    [versions, verify, verify_fun,
     fail_if_no_peer_cert,
     depth, cert, certfile, key, keyfile,
     password, cacerts, cacertfile, dh, dhfile, cihpers,
     debug].
