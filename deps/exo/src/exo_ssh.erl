%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    exo client direct tcpip socket
%%% @end
%%% Created : 24 May 2012 by Tony Rogvall <tony@rogvall.se>

-module(exo_ssh).

-behaviour(ssh_channel).

%% ssh_channel callbacks
-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

%% external api
-export([connect/3]).
-export([send/2]).
-export([call/4]).

-record(state, 
	{
	  owner,
	  channel, %% Id of the ssh channel
	  cm       %% Ssh connection manager
	 }
       ).

%% -include_lib("ssh/src/ssh_connect.hrl").
%% fixme: ?
-define(SSH_EXTENDED_DATA_DEFAULT, 0).
-define(SSH_EXTENDED_DATA_STDERR,  1).


init() ->
    application:start(crypto),    
    application:start(ssh).
    
%%
%% On mac osx the {ip_v6_disabled, true} is buggy and causes
%% problem 
connect(Host, Port, Options0) ->
    init(),
    Options =
	case os:type() of
	    {unix,darwin} -> [{ip_v6_disabled, true}|Options0];
	    _ -> Options0
	end,
    Pid = spawn_channel_proc(Host, 22, "localhost", Port, Options),
    receive
	{Pid,Result} ->
	    Result
    end.

spawn_channel_proc(Host, Port, RHost, RPort, Options) ->
    Caller = self(),
    proc_lib:spawn_link(
      fun() -> 
	      case ssh:connect(Host, Port, Options) of
		  {ok, Cm} ->
		      io:format("ssh connected to ~s:~w\n", [Host,Port]),
		      case ssh_connection:direct_tcpip(Cm, 
						       RHost, RPort,
						       {127,0,0,1}, 1234,
						       5000) of
			  {ok, Ci} ->
			      io:format("ssh: ~w connected to port ~w\n",
					[Ci, Port]),
			      Args = [{channel_cb, ?MODULE}, 
				      {init_args,[Cm, Ci, Caller]},
				      {cm, Cm}, {channel_id, Ci}],
			      case ssh_channel:init([Args]) of
				  {ok, State} ->
				      Caller ! {self(), {ok, self()}},
				      io:format("ssh: enter loop\n"),
				      ssh_channel:enter_loop(State);
				  Error ->
				      Caller ! {self(), Error}
			      end;
			  Error ->
			      Caller ! {self(), Error}
		      end;
		  Error ->
		      Caller ! {self(), Error}
	      end
      end).
    
send(ChannelPid, Data) ->
    ChannelPid ! {send, Data},
    Data.

%% BERT call over SSH
call(ChannelPid, M, F, A) ->
    Bin = term_to_binary({call,M,F,A}),
    Sz = byte_size(Bin), 
    send(ChannelPid, <<Sz:32, Bin/binary>>),
    receive
	{ChannelPid,{data,<<RSz:32, Reply:RSz/binary>>}} ->
	    binary_to_term(Reply);
	{ChannelPid,Other} ->
	    {error, Other}
    end.
    
		    
init([ConnectionManager, ChannelId, Owner] = _Args) ->
    io:format("ssh: init loop\n"),
    {ok, #state{ owner = Owner,
		 channel = ChannelId, 
		 cm = ConnectionManager}}.

%%--------------------------------------------------------------------
%% Function: handle_ssh_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles channel messages received on the ssh-connection.
%%--------------------------------------------------------------------
handle_ssh_msg({ssh_cm, _, {data, _ChannelId, 0, Data}}, State) ->
    State#state.owner ! {self(), {data, Data}},
    {ok, State};

handle_ssh_msg({ssh_cm, _, 
		{data, _ChannelId, ?SSH_EXTENDED_DATA_STDERR, Data}},
	       State) ->
    io:put_chars(Data),
    {ok, State};

handle_ssh_msg({ssh_cm, _, {eof, _ChannelId}}, State) ->
    State#state.owner ! {self(), eof},
    {ok, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, Error, _}}, State) ->
    io:put_chars("Connection closed by peer"),
    io:put_chars(Error),
    State#state.owner ! {self(), {error,Error}},
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, 0}}, State) ->
    io:put_chars("logout"),
    io:put_chars("Connection closed"),
    State#state.owner ! {self(), closed},
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, Status}}, State) ->
    io:put_chars("Connection closed by peer"),
    io:put_chars("Status: " ++ integer_to_list(Status)),
    State#state.owner ! {self(), {error,Status}},
    {stop, ChannelId, State}.

%%--------------------------------------------------------------------
%% Function: handle_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles other channel messages
%%--------------------------------------------------------------------
handle_msg({ssh_channel_up, ChannelId, ConnectionManager},
	   #state{channel = ChannelId,
		  cm = ConnectionManager} = State) ->
    {ok,  State};

handle_msg({send, Data}, #state{ channel = ChannelId, 
				 cm = ConnectionManager} = State) ->
    ssh_connection:send(ConnectionManager, ChannelId, Data),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reasons, State) -> _
%%                        
%% Description: Cleanup when shell channel is terminated
%%--------------------------------------------------------------------
terminate(_Reason, #state{}) ->
    ok.

