%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%   EXO TCP session
%%% @end
%%% Created : 22 Aug 2011 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(exo_socket_session).

-behaviour(gen_server).

%% API
-export([start/3,
	 start_link/3]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([encode_reuse/2,
	 decode_reuse_config/1]).

-define(SERVER, ?MODULE).

-record(state, {
	  module,
	  args,
	  socket,
	  active,
	  state,
	  pending = [],
	  idle_timeout
	 }).

-include("exo_socket.hrl").

-include("log.hrl").
-define(dbg(F, A), ?debug("~p " ++ F, [self()|A])).

-type exo_socket() :: #exo_socket {}.
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------

-spec start_link(Socket::exo_socket(), Module::atom(), Args::[term()]) ->
			{ok, pid()} | ignore | {error, Error::term()}.

start_link(XSocket,Module,Args) ->
    gen_server:start_link(?MODULE, [XSocket,Module,Args, []], []).

-spec start(Socket::exo_socket(), Module::atom(), Args::[term()]) ->
		   {ok, pid()} | ignore | {error, Error::term()}.

start(XSocket, Module, Args) ->
    gen_server:start(?MODULE, [XSocket,Module,Args], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([XSocket, Module, Args]) ->
    ?debug("init(~p, ~p, ~p)", [XSocket, Module, Args]),
    {ok, #state{ socket=XSocket,
		 module=Module,
		 args=Args,
		 state=undefined}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages.<br>
%% First take care of messages specific for exo_socket_session,
%% then call control in #state.module.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(),
		  From::{pid(), Tag::term()},
		  State::#state{}) ->
			 {reply, Reply::term(), State::#state{}} |
			 {noreply, State::#state{}} |
			 {stop, Reason::atom(), Reply::term(), State::#state{}}.

%% No 'local' handle_call
handle_call(Request, From,
	    State=#state{module = M, state = MSt, socket = Socket}) ->
    ?dbg("handle_call: ~p", [Request]),
    try M:control(Socket, Request, From, MSt) of
	Result ->
	    ?dbg("handle_call: reply ~p", [Result]),
	    mod_reply(Result, From, State)
    catch
	error:_Error ->
	    ?dbg("handle_call: catch reason  ~p", [_Error]),
	    ret({reply, {error, unknown_call}, State})
    end.

mod_reply({data, Data, MSt}, _, State) ->
    %% This case is when data has come from an external source
    %% that needs an additonal ok reply
    case handle_socket_data(Data, State#state{state = MSt}) of
	{noreply, NewState} -> {reply, ok, NewState};
	Other -> Other
    end;
mod_reply({ignore, MSt}, _, State) ->
    ret({noreply, State#state{state = MSt}});
mod_reply({ignore, MSt, Timeout}, _, State) ->
    ret({noreply, State#state{state = MSt}, Timeout});
mod_reply({reply, Reply, MSt}, _, State) ->
    ret({reply, Reply, State#state{state = MSt}});
mod_reply({reply, Reply, MSt, Timeout}, _, State) ->
    ret({reply, Reply, State#state{state = MSt}, Timeout});
mod_reply({send, Bin, MSt}, From, State) ->
    State1 = send_(Bin, From, State#state{state = MSt}),
    ret({noreply, State1});
mod_reply({send, Bin, MSt, Timeout}, From, State) ->
    State1 = send_(Bin, From, State#state{state = MSt}),
    ret({noreply, State1, Timeout});
mod_reply({stop, Reason, Reply, _MSt}, _From, State) ->
    %% Terminating
    ?dbg("handle_call: stopping ~p with reason ~p", [self(), Reason]),
    {stop, Reason, Reply, State}.

send_(Bin, From, #state{socket = S, pending = P} = State) ->
    P1 = if P == [] ->
		 exo_socket:send(S, Bin),
		 ?dbg("send: bin sent to ~p", [S]),
		 [{From,Bin}|P];
	    true -> P
	 end,
    State#state{pending = P1}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({activate,Active}, #state{socket = XSocket0} = State0) ->
    ?dbg("activate~n", []),
    case XSocket0 of
	{#exo_socket{} = X, Fun} when is_function(Fun, 1) ->
	    try Fun(X) of
		{ok, XSocket} ->
		    activate_(Active, State0#state{socket = XSocket});
		{error, _} = Error ->
		    ?debug("socket fun -> ~p", [Error]),
		    {stop, Error, State0}
	    catch
		Cat:Exception ->
		    ?debug("caught ~p:~p from socket fun", [Cat, Exception]),
		    {stop, Exception, State0}
	    end;
	#exo_socket{}->
	    activate_(Active, State0)
    end;
handle_cast(_Msg, State) ->
    ret({noreply, State}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    exo_socket:shutdown(State#state.socket, write),
    ?dbg("exo_socket_session: idle_timeout~p~n", [self()]),
    {stop, normal, State};
handle_info({Tag,Socket,Data0}, State) when
      %% FIXME: put socket tag in State for correct matching
      (Tag =:= tcp orelse Tag =:= ssl orelse Tag =:= http),
      Socket =:= (State#state.socket)#exo_socket.socket ->
    ?dbg("exo_socket_session: got data ~p\n", [{Tag,Socket,Data0}]),
    try exo_socket:auth_incoming(State#state.socket, Data0) of
	<<"reuse%", Rest/binary>> ->
	    handle_reuse_data(Rest, State);
	Data ->
	    handle_socket_data(Data, State)
    catch
	error:_ ->
	    exo_socket:shutdown(State#state.socket, write),
	    ret({noreply, State})
    end;
handle_info({Tag,Socket}, State) when
      (Tag =:= tcp_closed orelse Tag =:= ssl_closed),
      Socket =:= (State#state.socket)#exo_socket.socket ->
    ?dbg("exo_socket_session: got tag ~p\n", [{Tag,Socket}]),
    CSt0 = State#state.state,
    case apply(State#state.module, close, [State#state.socket,CSt0]) of
	{ok,CSt1} ->
	    {stop, normal, State#state { state = CSt1 }}
    end;
handle_info({Tag,Socket,Error}, State) when
      (Tag =:= tcp_error orelse Tag =:= ssl_error),
      Socket =:= (State#state.socket)#exo_socket.socket ->
    ?dbg("exo_socket_session: got error ~p\n", [{Tag,Socket,Error}]),
    CSt0 = State#state.state,
    case apply(State#state.module, error, [State#state.socket,Error,CSt0]) of
	{ok,CSt1} ->
	    ret({noreply, State#state { state = CSt1 }});
	{stop,Reason,CSt1} ->
	    {stop, Reason, State#state { state = CSt1 }}
    end;

handle_info(_Info, State) ->
    ?dbg("Got info: ~p\n", [_Info]),
    ret({noreply, State}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    socket_close(State#state.socket),
    ok.

socket_close({#exo_socket{} = S, _}) ->
    exo_socket:close(S);
socket_close(#exo_socket{} = S) ->
    exo_socket:close(S).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

activate_(Active, State0) ->
    try exo_socket:authenticate(State0#state.socket) of
	{ok, S} ->
	    ?dbg("authentication done~n", []),
	    State = State0#state{socket = S},
	    case apply(State#state.module, init,
		       [State#state.socket,State#state.args]) of
		Ok when element(1, Ok) == ok ->
		    CSt0 = element(2, Ok),
		    %% enable active mode here (if ever wanted) once is handled,
		    %% automatically anyway. exit_on_close is default and
		    %% allow session statistics retrieval in the close callback
		    SessionOpts = [{active,Active},{exit_on_close, false}],

		    _Res = exo_socket:setopts(State#state.socket, SessionOpts),
		    ?dbg("exo_socket:setopts(~w) = ~w\n", [SessionOpts, _Res]),
		    State1 = State#state { active = Active, state = CSt0 },
		    case Ok of
			{_, _, Timeout} ->
			    ret({noreply, State1, Timeout});
			{_, _} ->
			    ret({noreply, State1})
		    end;
		{stop,Reason,CSt1} ->
		    {stop, Reason, State#state { state = CSt1 }}
	    end;
	{error, Reason} ->
	    {stop, {auth_failure, Reason}, State0}
    catch
	error:Crash ->
	    {stop, {auth_failure, Crash}, State0}
    end.


ret({noreply, #state{idle_timeout = T} = S}) ->
    if T==undefined -> {noreply, S};
       true         -> {noreply, S, T}
    end;
ret({reply, R, #state{idle_timeout = T} = S}) ->
    if T==undefined -> {reply, R, S};
       true         -> {reply, R, S, T}
    end;
ret({noreply, S, T}) ->
    S1 = S#state{idle_timeout = T},
    if T==undefined -> {noreply, S1};
       true         -> {noreply, S1, T}
    end;
ret({reply, R, S, T}) ->
    S1 = S#state{idle_timeout = T},
    if T==undefined -> {reply, R, S1};
       true         -> {reply, R, S1, T}
    end;
ret(R) ->
    R.


%% continued from handle_info/2
handle_reuse_data(Rest, #state{module = M, state = MSt} = State) ->
    Config = decode_reuse_config(Rest),
    ?dbg("Decoded reuse config: ~p~n"
	 "State = ~p~n", [Config, State]),
    {ok, MSt1} = M:received_reuse_info(Config, MSt),
    State1 = State#state{state = MSt1},
    {ok, {Host,_}} = exo_socket:peername(State1#state.socket),
    get_parent(State1) ! {self(), reuse, [{host, Host}|Config]},
    if State1#state.active == once ->
	    exo_socket:setopts(State#state.socket, [{active,once}]);
       true ->
	    ok
    end,
    ret({noreply, State1}).

handle_socket_data(Data, State) ->
    CSt0 = State#state.state,
    ModResult = apply(State#state.module, data,
		      [State#state.socket,Data,CSt0]),
    ?dbg("handle_socket_data: result ~p", [ModResult]),
    handle_module_result(ModResult, State).

handle_module_result({ok,CSt1}, State) ->
    if State#state.active == once ->
	    exo_socket:setopts(State#state.socket, [{active,once}]);
       true ->
	    ok
    end,
    ret({noreply, State#state { state = CSt1 }});
handle_module_result({close, CSt1}, State) ->
    exo_socket:shutdown(State#state.socket, write),
    ret({noreply, State#state { state = CSt1 }});
handle_module_result({stop,Reason,CSt1}, State) ->
    {stop, Reason, State#state { state = CSt1 }};
handle_module_result({reply, Rep, CSt1}, State) ->
    if State#state.active == once ->
	    exo_socket:setopts(State#state.socket, [{active,once}]);
       true ->
	    ok
    end,
    case State#state.pending of
	[{From,_}|Rest] ->
	    gen_server:reply(From, Rep),
	    send_next(Rest, State#state.socket),
	    ret({noreply, State#state { pending = Rest, state = CSt1 }});
	[] ->
	    %% huh?
	    ret({noreply, State#state { state = CSt1 }})
    end.

send_next([{_From, Msg}|_], Socket) ->
    exo_socket:send(Socket, Msg);
send_next([], _) ->
    ok.

get_parent(_) ->
    hd(get('$ancestors')).

%% The reuse service message must at a minimum include Port.
%% Other options can be passed, as [{Module, Key, Value}]. These are
%% encoded at Module:encode({Key, Value}) -> Bin, and decoded on the other
%% end as Module:decode(Bin) -> {Key, Value}.
%%
encode_reuse(Port, Opts) when is_integer(Port), is_list(Opts) ->
    Custom = lists:map(
	       fun({M,K,V}) ->
		       Bin = M:encode({K,V}),
		       Sz = byte_size(Bin),
		       <<"%|", (atom_to_binary(M,latin1))/binary, "|",
			 Sz:16/integer, Bin/binary>>
	       end, Opts),
    <<"reuse%port:", (list_to_binary(integer_to_list(Port)))/binary,
      (iolist_to_binary(Custom))/binary>>.

decode_reuse_config(Bin) ->
    decode_reuse_config(Bin, []).

decode_reuse_config(<<"port:", Rest/binary>>, Acc) ->
    {P, Rest1} = decode_get_value(Rest),
    Port = list_to_integer(binary_to_list(P)),
    decode_reuse_config(Rest1, [{port, Port}|Acc]);
decode_reuse_config(<<"|", Rest/binary>>, Acc) ->
    {M, <<Sz:16/integer, Rest1/binary>>} = decode_get_value($|, Rest),
    <<Data:Sz/binary, Rest2/binary>> = Rest1,
    {Key, Value} = (binary_to_atom(M,latin1)):decode(Data),
    decode_reuse_config(parse_next(Rest2), [{Key,Value}|Acc]);
decode_reuse_config(<<>>, Acc) ->
    lists:reverse(Acc).

parse_next(<<"%", Rest/binary>>) -> Rest;
parse_next(<<>>) -> <<>>.

decode_get_value(Bin) ->
    decode_get_value($%, Bin).

decode_get_value(Delim, Bin) ->
    case re:split(Bin, <<$\\,Delim>>, [{return,binary},{parts,2}]) of
	[V] ->
	    {V, <<>>};
	[V, Rest] ->
	    {V, Rest}
    end.

%% decode_reuse_config(Bin) ->
%%     Items = re:split(Bin, "%", [{return, list}]),
%%     lists:map(
%%       fun(I) ->
%% 	      case re:split(I, ":", [{return, list}]) of
%% 		  ["host", Host] ->
%% 		      {host, Host};
%% 		  ["port", P] ->
%% 		      {port, list_to_integer(P)}
%% 	      end
%%       end, Items).
