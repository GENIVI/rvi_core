%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

%% Setup a listen socket and manage connections to remote parties.
%% Can also retrieve connections by peer address.
-module(listener).

-include_lib("lager/include/log.hrl").

-export([start_link/0,
         add_listener/3,
         remove_listener/3]).

-export([init/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, sock_opts/0, new_connection/4]).

-behavior(gen_nb_server).

start_link() ->
    gen_nb_server:start_link(?MODULE, []).

add_listener(Pid, IpAddr, Port) ->
    gen_server:call(Pid, {add_listener, IpAddr, Port}).

remove_listener(Pid, IpAddr, Port) ->
    gen_server:call(Pid, {remove_listener, IpAddr, Port}).

init([], State) ->
    {ok, State}.

handle_call({add_listener, IpAddr, Port}, _From, State) ->
    case gen_nb_server:add_listen_socket({IpAddr, Port}, State) of
        {ok, State1} ->
            {reply, ok, State1};
        Error ->
            {reply, Error, State}
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
    [binary, {active, once}, {packet, 0}].

new_connection(IP, Port, Sock, State) ->
    ?debug("listener:new_connection(): Peer IP:    ~p (ignored)", [IP]),
    ?debug("listener:new_connection(): Peer Port:  ~p (ignored)", [Port]),
    ?debug("listener:new_connection(): Sock:       ~p", [Sock]),

    %% IP and Port are garbage. We'll grab peername when we get our
    %% first data.
    {ok, _P} = connection:setup(undefined, 0, Sock, 
				data_link_bert_rpc_rpc, 
				handle_socket, []),
    {ok, State}.

