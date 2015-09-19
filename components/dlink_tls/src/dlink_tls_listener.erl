%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

%% Setup a listen socket and manage connections to remote parties.
%% Can also retrieve connections by peer address.
-module(dlink_tls_listener).

-include_lib("lager/include/log.hrl").

-export([start_link/0,
         add_listener/4,
         remove_listener/3]).

-export([init/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, sock_opts/0, new_connection/4]).

-behavior(gen_nb_server).

start_link() ->
    gen_nb_server:start_link(?MODULE, []).

add_listener(Pid, IpAddr, Port, CompSpec) ->
    gen_server:call(Pid, {add_listener, IpAddr, Port, CompSpec}).

remove_listener(Pid, IpAddr, Port) ->
    gen_server:call(Pid, {remove_listener, IpAddr, Port}).

init([], State) ->
    {ok, State}.

handle_call({add_listener, IpAddr, Port, CompSpec}, _From, State) ->
    case gen_nb_server:add_listen_socket({IpAddr, Port}, State) of
        {ok, State1} ->
            {reply, ok, gen_nb_server:store_cb_state( CompSpec, State1 )};

        Error ->
            {reply, Error, gen_nb_server:store_cb_state( CompSpec, State )}
    end;

handle_call({remove_listener, IpAddr, Port}, _From, State) ->
    case gen_nb_server:remove_listen_socket({IpAddr, Port}, State) of
        {ok, State1} ->
            {reply, ok, State1};
        Error ->
            {reply, Error, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

sock_opts() ->
    [list, {active, once}, {packet, 0}].

new_connection(IP, Port, Sock, State) ->
    ?debug("~p:new_connection(): Peer IP:    ~p (ignored)", [?MODULE,IP]),
    ?debug("~p:new_connection(): Peer Port:  ~p (ignored)", [?MODULE,Port]),
    ?debug("~p:new_connection(): Sock:       ~p", [?MODULE,Sock]),

    %% IP and Port are garbage. We'll grab peername when we get our
    %% first data.
    %% Provide component spec as extra arg.
    {ok, _P} = dlink_tls_conn:setup(
		 undefined, 0, Sock,
		 dlink_tls_rpc,
		 handle_socket, [gen_nb_server:get_cb_state(State)]),
    {ok, State}.
