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

-export([start_link/0,
         add_listener/1,
         remove_listener/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2]).

-record(st, {listeners = [],
	     acceptors = []}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_listener(Channel) ->
    gen_server:call(?MODULE, {add_listener, Channel}).

remove_listener(Channel) ->
    gen_server:call(?MODULE, {remove_listener, Channel}).

init([]) ->
    {ok, #st { listeners = [] }}.


handle_call({add_listener, Channel}, _From, State) ->
    ?info("bt_listener:add_listener(): Setting up listener on channel ~p", [ Channel]),

    case rfcomm:listen(Channel) of
        {ok, ListenRef} ->
	    ?info("bt_listener:add_listener(): ListenRef: ~p", [ ListenRef]),
	    { noreply, NSt} = handle_info({accept, ListenRef, Channel, ok}, State),

	    { reply, 
	      ok, 
	      NSt#st { 
		listeners = [ { ListenRef, Channel } | NSt#st.listeners ]
	       }
	    };

	Err ->
	    ?info("bt_listener:add_listener(): Failed: ~p", [ Err]),
	    {reply, Err, State}

    end;

handle_call({remove_listener, _Channel}, _From, State) ->
    ?warning("FIXME: bt_listener:remove_listener()"),
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({accept, ListenRef, Channel, ok} , St) ->
    %% Fire up a new process to handle the
    %% future incoming connection.
    ?info("bt_listener:accept(): ListenRef: ~p", [ ListenRef]),

    {ok, ConnPid} = bt_connection:accept(Channel, 
					 ListenRef, 
					 self(), 
					 dlink_bt_rpc, 
					 handle_socket, 
					 nil),

    
    {noreply, St#st {acceptors = [ { Channel, ConnPid } | St#st.acceptors ]}};

handle_info(_Msg, State) ->
    
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

 

