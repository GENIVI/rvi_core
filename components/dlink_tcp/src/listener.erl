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
         remove_listener/2]).

-export([init/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, sock_opts/0, new_connection/4]).

-behavior(gen_nb_server).

-define(TAB, dlink_tcp_listener_tab).

start_link() ->
    create_ets(),
    gen_nb_server:start_link({local, ?MODULE}, ?MODULE, []).

create_ets() ->
    case ets:info(?TAB, name) of
	undefined -> ets:new(?TAB, [public, named_table, set]);
	_ ->         ?TAB
    end.

add_listener(IpAddr, Port, CompSpec) ->
    gen_server:call(?MODULE, {add_listener, IpAddr, Port, CompSpec}).

remove_listener(IpAddr, Port) ->
    gen_server:call(?MODULE, {remove_listener, IpAddr, Port}).

init([], State) ->
    case ets_select(?TAB, [{ '_', [], ['$_'] }]) of
	[] ->
	    {ok, State};
	Addrs ->
	    lists:foldl(
	      fun({{_, _} = Addr}, Acc) ->
		      case gen_nb_server:add_listen_socket(Addr, Acc) of
			  {ok, Acc1} ->
			      Acc1;
			  _Error ->
			      ets_delete(?TAB, Addr),
			      Acc
		      end;
		 ({cs, CS}, Acc) ->
		      gen_nb_server:store_cb_state(CS, Acc)
	      end, State, Addrs)
    end.

handle_call({add_listener, IpAddr, Port, CompSpec}, _From, State) ->
    ets_insert(?TAB, {cs, CompSpec}),
    case gen_nb_server:add_listen_socket({IpAddr, Port}, State) of
        {ok, State1} ->
	    ets_insert(?TAB, {{IpAddr,Port}}),
            {reply, ok, gen_nb_server:store_cb_state( CompSpec, State1 )};

        Error ->
            {reply, Error, gen_nb_server:store_cb_state( CompSpec, State )}
    end;

handle_call({remove_listener, IpAddr, Port}, _From, State) ->
    case gen_nb_server:remove_listen_socket({IpAddr, Port}, State) of
        {ok, State1} ->
	    ets_delete(?TAB, {IpAddr, Port}),
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
    %% Provide component spec as extra arg.
    {ok, _P} = connection:setup(server, undefined, 0, Sock,
				dlink_tcp_rpc,
				handle_socket, gen_nb_server:get_cb_state(State)),
    {ok, State}.

ets_insert(Tab, Obj) ->
    ets:insert(Tab, Obj).

ets_delete(Tab, Key) ->
    ets:delete(Tab, Key).

ets_select(Tab, Pat) ->
    ets:select(Tab, Pat).
