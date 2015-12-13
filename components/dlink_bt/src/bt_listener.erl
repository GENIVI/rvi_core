%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

%% Setup a listen socket and manage connections to remote parties.
%% Can also retrieve connections by peer address.
-module(bt_listener).

-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").
-export([start_link/1,
         add_listener/1,
         remove_listener/1,
	 accept_ack/4]).
-export([sock_opts/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2]).

-record(st, {listeners = [],
	     acceptors = [],
	     mode = bt,
	     cs = #component_spec{}
	    }).


start_link(Mode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Mode, []).

add_listener(Channel) ->
    gen_server:call(?MODULE, {add_listener, Channel}).

remove_listener(Channel) ->
    gen_server:call(?MODULE, {remove_listener, Channel}).

accept_ack(Result, LRef, Addr, Chan) ->
    ?MODULE ! {accept, self(), LRef, Addr, Chan, Result},
    ok.

sock_opts() ->
    [{reuseaddr, true}, binary, {active, once}, {packet, 0}].

init(Mode) ->

    {ok, #st {
	    listeners = [],
	    acceptors = [],
	    mode = Mode,
	    cs = rvi_common:set_value(bt_mode, Mode, rvi_common:get_component_specification())
	   }
    }.


handle_call({add_listener, Channel}, _From, #st{mode = Mode,
						listeners = Ls} = St) ->
    ?info("bt_listener:add_listener(): Setting up listener on channel ~p", [ Channel]),

    case listen(Mode, Channel) of
        {ok, ListenRef} ->
	    ?info("bt_listener:add_listener(): ListenRef: ~p", [ ListenRef]),
	    St1 = St#st{listeners = [{ListenRef, Channel}|Ls]},
	    {reply, ok, start_acceptor(ListenRef, Channel, St1)};

	Err ->
	    ?info("bt_listener:add_listener(): Failed: ~p", [ Err]),
	    {reply, Err, St}

    end;

handle_call({remove_listener, _Channel}, _From, State) ->
    ?warning("FIXME: bt_listener:remove_listener()"),
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({accept, Pid, ListenRef, BTAddr, Channel, ok},
	    #st{listeners = Ls,
		acceptors = As} = St) ->
    %% Fire up a new process to handle the
    %% future incoming connection.
    ?info("bt_listener:accept(): ListenRef: ~p", [ ListenRef]),
    ?info("bt_listener:accept(): Remote: ~p-~p", [BTAddr, Channel ]),

    case lists:keyfind(ListenRef, 1, Ls) of
	{_, Channel} ->
	    As1 = lists:keydelete(Pid, 2, As),
	    {noreply, start_acceptor(ListenRef, Channel,
				     St#st{acceptors = As1})};
    %% Must fix multiple acceptors in bt_linux_drv.c
    %% {ok, ConnPid} = bt_connection:accept(Channel,
    %%  					 ListenRef,
    %%  					 dlink_bt_rpc,
    %% 					 handle_socket,
    %%  					 []),


    %% {noreply, St#st {acceptors = [ { Channel, ConnPid } | St#st.acceptors ]}};
	_ ->
	    {noreply, St }
    end;

handle_info(_Msg, State) ->
    ?info("bt_listener:handle_info(): Unknown: ~p", [ _Msg]),

    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


listen(bt, Channel) ->
    rfcomm:listen(Channel);
listen(tcp, Port) ->
    gen_tcp:listen(Port, sock_opts()).

start_acceptor(ListenRef, Channel, #st{mode = Mode,
				       acceptors = As,
				       cs = CS} = St) ->
    ?debug("start acceptor(~p, ~p, St)", [ListenRef, Channel]),
    {ok, ConnPid} = bt_connection:accept(Channel,
					 ListenRef,
					 Mode,
					 dlink_bt_rpc,
					 handle_socket,
					 CS),
    St#st{acceptors = [{Channel, ConnPid}|As]}.
