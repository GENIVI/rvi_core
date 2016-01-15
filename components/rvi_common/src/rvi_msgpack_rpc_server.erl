%% -*- mode: erlang; indent-tabs-mode: nil; -*-
%%=============================================================================
%%
%% Copyright (C) 2015, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/

-module(rvi_msgpack_rpc_server).

-behaviour(exo_socket_server).

-include("rvi_msgpack_rpc.hrl").
-include_lib("lager/include/log.hrl").

-record(state,
	{
	  callback,
	  msgpack_opts = msgpack_options(),
	  buf = <<>>
	}).

-export([init/2, data/3, close/2, error/3]).

-export([start/1, start/3, start/4]).
-export([start_link/1,
	 start_link/4]).
-export([start_ssl/1, start_ssl/3, start_ssl/4]).
-export([start_link_ssl/4]).
-export([control/4]).

-export([msgpack_options/0]).


msgpack_options() ->
    [{allow_atom, pack},
     {enable_str, true},
     jsx].

start(Callback) ->
    start(?RPC_PORT, Callback, []).

start(Port, Callback, Options) ->
    start(Port, Callback, Options, []).

start(Port, Callback, Options, ExoOptions) ->
    do_start(Port, Callback, Options, ExoOptions, start).

start_link(Opts) ->
    [Port, Callback, Exo, Rest] = rvi_common:take([port, callback, {exo, []}], Opts),
    start_link(Port, Callback, Rest, Exo).

start_link(Port, Callback, Options, ExoOptions) ->
    do_start(Port, Callback, Options, ExoOptions, start_link).

do_start(Port, Callback, Options, ExoOptions, StartF) when StartF==start;
						 StartF==start_link ->
    ?debug("do_start(~p, ~p, ~p, ~p, ~p)", [Port, Callback, Options, ExoOptions, StartF]),
    case lists:keymember(ssl, 1, Options) of
	{_, true} ->
	    start_ssl(Port, Options, ExoOptions);
	_ ->
	    exo_socket_server:StartF(Port,[tcp],
				     [{active,once},{packet,0},binary,
				      {reuseaddr,true} | ExoOptions],
				     ?MODULE, {Callback, Options})
    end.

start_ssl(Callback) ->
    start_ssl(?RPC_PORT, Callback, []).

start_ssl(Port, Callback, Options) ->
    start_ssl(Port, Callback, Options, []).

start_ssl(Port, Callback, Options, ExoOptions) ->
    do_start_ssl(Port, Callback, Options, ExoOptions, start).

start_link_ssl(Port, Callback, Options, ExoOptions) ->
    do_start_ssl(Port, Callback, Options, ExoOptions, start_link).

do_start_ssl(Port, Callback, Options, ExoOptions, StartF) when
      StartF == start; StartF == start_link ->
    KeyAndCert = key_and_cert(ExoOptions),
    Verify = proplists:get_value(verify, ExoOptions, verify_none),
    Debug = proplists:get_value(debug, ExoOptions, true),
    exo_socket_server:StartF(Port,[tcp,probe_ssl],
			     KeyAndCert ++
				 [{active,once},{packet,0},binary,
				  {debug, Debug},
				  {verify, Verify}, %% no client cert required
				  {reuseaddr,true} | ExoOptions], ?MODULE, {Callback, Options}).

key_and_cert(Opts) ->
    Dir = code:priv_dir(rvi_common),
    [{keyfile, opt(keyfile, Opts, filename:join(Dir, "host.key"))},
     {certfile, opt(certfile, Opts, filename:join(Dir, "host.cert"))}].

opt(K, Opts, Default) ->
    case lists:keyfind(K, 1, Opts) of
	{_, Value} ->
	    Value;
	false ->
	    Default
    end.

init(Socket, {Callback, Options}) ->
    ?debug("init(~p, ~p)", [Socket, {Callback, Options}]),
    {ok,{IP,Port}} = exo_socket:peername(Socket),
    ?debug("connection from: ~p : ~p", [IP, Port]),
    gproc:reg({p,l,msgpack_rpc_server}, {IP,Port, Callback}),
    case lists:keyfind(gproc, 1, Options) of
	{_, Reg} ->
	    ?debug("registering with gproc: ~p", [Reg]),
	    gproc:reg(Reg, {IP, Port, Callback});
	false ->
	    ?debug("not registering name with gproc", []),
	    ok
    end,
    MsgPackOpts = opt(msgpack, Options, msgpack_options()),
    {ok, #state{callback = Callback, msgpack_opts = MsgPackOpts}}.

data(Socket, Data, #state{buf = Buf, msgpack_opts = Opts} = State) ->
    Buf1 = <<Buf/binary, Data/binary>>,
    try Dec = msgpack:unpack_stream(Buf1, Opts),
	 ?debug("decoded: ~p", [Dec]),
	 case Dec of
	     {[?TYPE_REQUEST, ID, Method, Args], Rest} ->
		 handle_call_request(Socket, ID, Method, Args, State#state{buf = Rest});
	     {[?TYPE_NOTIFY, Method, Args], Rest} ->
		 handle_notify_request(Socket, Method, Args, State#state{buf = Rest});
	     {error, incomplete} ->
		 {ok, State#state{buf = Buf1}};
	     {error, Reason} ->
		 ?debug("error parsing stream: ~p", [Reason]),
		 {ok, State#state{buf = <<>>}}
	 end
    catch
	error:_Error ->
	    ?debug("decode error: ~p", [_Error]),
	    {ok,State}
    end.

control(_XSocket, _Request, _From, St) ->
    {reply, {error, unsupported}, St}.

%%
%% close - retrieve statistics
%% transport socket SHOULD still be open, but ssl may not handle this!
%%
close(Socket, State) ->
    case exo_socket:getstat(Socket, exo_socket:stats()) of
	{ok,_Stats} ->
	    ?debug("~w: close, stats=~w", [?MODULE, _Stats]),
	    {ok, State};
	{error,_Reason} ->
	    ?debug("~w: close, stats error=~w", [?MODULE, _Reason]),
	    {ok, State}
    end.

error(_Socket,Error,State) ->
    ?debug("bert_rpc_exec: error = ~p\n", [Error]),
    {stop, Error, State}.

%%
%% Internal
%%
handle_call_request(Socket, ID, Method, Args,
		    #state{callback = CB,
			   msgpack_opts = Opts} = State) ->
    try Res = apply(CB, binary_to_existing_atom(Method, latin1), Args),
	 Msg = msgpack:pack([?TYPE_RESPONSE, ID, null, Res], Opts),
	 exo_socket:send(Socket, Msg)
    catch
	error:Reason ->
	    ?debug("caught ~s ~p -> error:~p", [Method, Args, Reason]),
	    ReasonStr = lists:flatten(io_lib:fwrite("error:~w", [Reason])),
	    ErrMsg = msgpack:pack([?TYPE_RESPONSE, ID, ReasonStr, null]),
	    exo_socket:send(Socket, ErrMsg)
    end,
    {ok, State#state{buf = <<>>}}.

handle_notify_request(_Socket, Method, Args, #state{callback = CB} = State) ->
    apply(CB, binary_to_existing_atom(Method, latin1), Args),
    {ok, State}.
