%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.home>
%%% @copyright (C) 2014, magnus
%%% @doc
%%%
%%% @end
%%% Created : 12 Sep 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(sms_connection).

-behaviour(gen_server).
-include_lib("lager/include/log.hrl").


%% API

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([setup/4, setup/5]).
-export([send/2, send_auth/2]).
-export([is_auth_sent/1]).
-export([is_connection_up/1]).
-export([terminate_connection/1]).

-include_lib("gsms/include/gsms.hrl").

-define(SERVER, ?MODULE).

-record(st, {
	  addr = "",
	  mod = undefined,
	  func = undefined,
	  args = undefined,
	  pst = undefined, %%  Payload state
	  auth_sent = false
	 }).

%%%===================================================================
%%% API
%%%===================================================================
%% MFA is to deliver data received on the socket.

setup(Addr, Mod, Fun, Arg) ->
    setup(Addr, [], Mod, Fun, Arg).

setup(Addr, Msgs, Mod, Fun, Arg) ->
    gen_server:start_link(?MODULE, {Addr, Msgs, Mod, Fun, Arg},[]).

send(Conn, Data) ->
    send(Conn, Data, false).

send_auth(Conn, Data) ->
    send(Conn, Data, true).

send(Pid, Data, IsAuth) when is_pid(Pid), is_boolean(IsAuth) ->
    gen_server:cast(Pid, {send, "RVI:" ++ Data, IsAuth});
send(Addr, Data, IsAuth) when is_list(Addr), is_boolean(IsAuth) ->
    case sms_connection_manager:find_connection_by_address(Addr) of
	{ok, Pid} ->
	    gen_server:cast(Pid, {send, "RVI:" ++ Data, IsAuth});

	_Err ->
	    ?info("connection:send(): Connection ~p not found for data: ~p",
		  [Addr, Data]),
	    not_found

    end.

is_auth_sent(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, is_auth_sent).

terminate_connection(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, terminate_connection);
terminate_connection(Addr) when is_list(Addr) ->
    case sms_connection_manager:find_connection_by_address(Addr) of
	{ok, Pid} ->
	    gen_server:call(Pid, terminate_connection);

	_Err -> not_found
    end.

is_connection_up(Pid) when is_pid(Pid) ->
    is_process_alive(Pid);
is_connection_up(Addr) when is_list(Addr) ->
    case sms_connection_manager:find_connection_by_address(Addr) of
	{ok, Pid} ->
	    is_connection_up(Pid);

	_Err ->
	    false
    end.

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
%% MFA used to handle socket closed, socket error and received data
%% When data is received, a separate process is spawned to handle
%% the MFA invocation.
init({Addr, Msgs, Mod, Fun, Arg}) ->
    [self() ! M || M <- Msgs],
    case Addr of
	undefined -> ok;
	_ ->
	    sms_connection_manager:add_connection(Addr, self())
    end,
    ?debug("connection:init(): self():   ~p", [self()]),
    ?debug("connection:init(): Addr:     ~p", [Addr]),
    ?debug("connection:init(): Msgs:     ~p", [Msgs]),
    ?debug("connection:init(): Module:   ~p", [Mod]),
    ?debug("connection:init(): Function: ~p", [Fun]),
    ?debug("connection:init(): Arg:      ~p", [Arg]),
    gsms:subscribe([{anumber, Addr}]),
    {ok, #st{
	    addr = Addr,
	    mod = Mod,
	    func = Fun,
	    args = Arg,
	    pst = undefined
	   }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------


handle_call(terminate_connection, _From,  #st{addr = Addr,
					      mod = Mod,
					      func = Fun,
					      args = Arg} = St) ->
    ?debug("~p:handle_call(terminate_connection): Terminating: ~p",
	   [?MODULE, St#st.addr]),
    Mod:Fun(self(), Addr, closed, Arg),
    sms_connection_manager:delete_connection_by_pid(self()),
    {stop, normal, ok, St};

handle_call(is_auth_sent, _From, #st{auth_sent = AuthSent} = St) ->
    {reply, AuthSent, St};

handle_call(_Request, _From, State) ->
    ?warning("~p:handle_call(): Unknown call: ~p", [ ?MODULE, _Request]),
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({send, Data, IsAuth},  #st{addr = A} = St) ->
    ?debug("~p:handle_call(send): Sending: ~p",
	   [ ?MODULE, Data]),
    gsms:send([{addr, A}], Data),
    {noreply, St#st{auth_sent = St#st.auth_sent or IsAuth}};

handle_cast({activate_socket, Sock}, State) ->
    Res = inet:setopts(Sock, [{active, once}]),
    ?debug("connection:activate_socket(): ~p", [Res]),
    {noreply, State};


handle_cast(_Msg, State) ->
    ?warning("~p:handle_cast(): Unknown call: ~p", [ ?MODULE, _Msg]),
    {noreply, State}.

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

%% Fill in peername if empty.
handle_info({gsms, _Ref, #gsms_deliver_pdu{addr = #gsms_addr{addr = Addr},
					   ud = "RVI:" ++ Data}},
	    #st{addr = Addr, pst = PST,
		mod = Mod, func = Fun, args = Arg} = State) ->
    ?debug("~p:handle_info(data): Data: ~p", [?MODULE, Data]),
    ?debug("~p:handle_info(data): From: ~p", [?MODULE, Addr]),

    case rvi_common:extract_json(Data, PST) of
	{[], NPST} ->
	    ?debug("~p:handle_info(data incomplete)", [?MODULE]),
	    {noreply, State#st {pst = NPST}};
	{JSONElements, NPST} ->
	    ?debug("~p:handle_info(data complete): Processed: ~p",
		   [?MODULE, JSONElements]),
	    FromPid = self(),
	    spawn(fun() ->
			  [Mod:Fun(FromPid, Addr, data, SingleElem, Arg)
			   || SingleElem <- JSONElements]
		  end),
	    {noreply, State#st{pst = NPST}}
    end;

handle_info(_Info, State) ->
    ?warning("~p:handle_cast(): Unknown info: ~p", [ ?MODULE, _Info]),
    {noreply, State}.

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
terminate(_Reason, _State) ->
    ?debug("~p:terminate(): Reason: ~p ", [ ?MODULE, _Reason]),
    ok.

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
