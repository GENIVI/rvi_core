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
-module(bt_connection).

-behaviour(gen_server).
-include_lib("lager/include/log.hrl").

%% API

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([connect/6]).
-export([accept/6]).
-export([send/2]).
-export([send/3]).
-export([is_connection_up/1]).
-export([is_connection_up/2]).
-export([terminate_connection/1]).
-export([terminate_connection/2]).


-define(SERVER, ?MODULE).
-define(PACKET_MOD, dlink_data_json).

-record(st, {
	  remote_addr = "00:00:00:00:00:00",
	  channel = 0,
	  rfcomm_ref,
	  listen_ref,
	  accept_ref,
	  mode = bt,
	  packet_mod = ?PACKET_MOD,
	  packet_st = [],
	  mod,
	  func,
	  args
	 }).

%%%===================================================================
%%% API
%%%===================================================================
%% MFA is to deliver data received on the socket.

connect(Addr, Channel, Mode, Mod, Fun, Arg) ->
    gen_server:start_link(?MODULE,
			  {connect, Addr, Channel, Mode, Mod, Fun, Arg },
			  []).

accept(Channel, ListenRef, Mode, Mod, Fun, Arg) ->
    gen_server:start_link(?MODULE, {accept,
				    Channel,
				    ListenRef,
				    Mode,
				    Mod,
				    Fun,
				    Arg},[]).

send(Pid, Data) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Data}).

send(Addr, Channel, Data) ->
    case bt_connection_manager:find_connection_by_address(Addr, Channel) of
	{ok, Pid} ->
	    gen_server:cast(Pid, {send, Data});

	_Err ->
	    ?info("connection:send(): Connection ~p:~p not found for data: ~p",
		  [ Addr, Channel, Data]),
	    not_found

    end.

terminate_connection(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, terminate_connection).

terminate_connection(Addr, Channel) ->
    case bt_connection_manager:find_connection_by_address(Addr, Channel) of
	{ok, Pid} ->
	    gen_server:call(Pid, terminate_connection);

	_Err -> not_found
    end.


is_connection_up(Pid) when is_pid(Pid) ->
    is_process_alive(Pid).

is_connection_up(Addr, Channel) ->
    case bt_connection_manager:find_connection_by_address(Addr, Channel) of
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
init({connect, BTAddr, Channel, Mode, Mod, Fun, CS}) ->

    %% connect will block on rfcomm:open, so cast to self
    %% in order to let init return.
    ?debug("init({connect, ~p, ~p, ~p, ~p, ~p, CS})",
	   [BTAddr, Channel, Mode, Mod, Fun]),
    gen_server:cast(self(), connect),
    {ok, PktMod} = get_module_config(packet_mod, ?PACKET_MOD, CS),
    PktSt = PktMod:init(CS),
    {ok, #st{
	    remote_addr = BTAddr,
	    channel = Channel,
	    rfcomm_ref = undefined,
	    mode = Mode,
	    mod = Mod,
	    func = Fun,
	    args = CS,
	    packet_mod = PktMod,
	    packet_st = PktSt
	   }};



init({accept, Channel, ListenRef, Mode, Mod, Fun, CS}) ->
    ?debug("init({accept, ~p, ~p, ~p, ~p, ~p, CS})",
	   [Channel, ListenRef, Mode, Mod, Fun]),
    ?debug("bt_connection:init(accept): self():    ~p", [self()]),
    ?debug("bt_connection:init(accept): Channel:   ~p", [Channel]),
    ?debug("bt_connection:init(accept): ListenRef: ~p", [ListenRef]),
    ?debug("bt_connection:init(accept): Module:    ~p", [Mod]),
    ?debug("bt_connection:init(accept): Function:  ~p", [Fun]),
    ?debug("bt_connection:init(accept): Arg:       ~p", [CS]),
    {ok, PktMod} = get_module_config(packet_mod, ?PACKET_MOD, CS),
    PktSt = PktMod:init(CS),
    ARef = case Mode of
	       bt ->
		   %% Expected message is
		   %% {rfcomm, ARef, {accept, Addr, Chan}}
		   {ok, R} = rfcomm:accept(ListenRef, infinity, self()),
		   R;
	       tcp ->
		   %% -1 represents infinity
		   %% Expected message is
		   %% {inet_async,LSock,Ref,{ok,Socket}}
		   {ok, R} = prim_inet:async_accept(ListenRef, -1),
		   R
	   end,
    {ok, #st{
	    channel = Channel,
	    rfcomm_ref = undefined,
	    accept_ref = ARef,
	    mode = Mode,
	    mod = Mod,
	    func = Fun,
	    args = CS,
	    packet_mod = PktMod,
	    packet_st = PktSt
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
handle_call(terminate_connection, _From,  St) ->
    ?debug("~p:handle_call(terminate_connection): Terminating: ~p",
	     [ ?MODULE, {St#st.remote_addr, St#st.channel}]),

    {stop, Reason, NSt} = handle_info({tcp_closed, St#st.rfcomm_ref}, St),
    {stop, Reason, ok, NSt};

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
handle_cast(connect,  #st {
			 remote_addr = BTAddr,
			 channel = Channel,
			 mode = Mode,
			 mod = Mod,
			 func = Fun,
			 args = Arg
			} = St) ->

    %% Looong call that blocks for ever.
    ConnRes = case Mode of
		  bt ->
		      rfcomm:open(BTAddr, Channel);
		  tcp ->
		      gen_tcp:connect(BTAddr, Channel, bt_listener:sock_opts())
	      end,
    case ConnRes of
	{ok, ConnRef} ->
	    ?debug("bt_connection:init(connect): self():   ~p", [self()]),
	    ?debug("bt_connection:init(connect): BTAddr:   ~p", [BTAddr]),
	    ?debug("bt_connection:init(connect): Channel:  ~p", [Channel]),
	    ?debug("bt_connection:init(connect): Ref:      ~p", [ConnRef]),
	    ?debug("bt_connection:init(connect): Module:   ~p", [Mod]),
	    ?debug("bt_connection:init(connect): Function: ~p", [Fun]),
	    ?debug("bt_connection:init(connect): Arg:      ~p", [Arg]),

	    %% Add to managed connections,
	    bt_connection_manager:add_connection(BTAddr, Channel, self()),

	    Mod:Fun(self(), BTAddr, Channel, connected, Arg),

	    %% Update state with new connection
	    {noreply, St#st{
			rfcomm_ref = ConnRef
		       }};

	{ error, Error } ->
	    ?info("Failed to connect to ~p-~p", [ BTAddr, Channel]),
	    { stop, { connect_failed, Error}, St }
    end;

handle_cast({send, Data}, #st{mode = Mode,
			      rfcomm_ref = Sock,
			      packet_mod = PMod, packet_st = PSt} = St) ->
    ?debug("handle_cast(send): Sending: ~p", [Data]),
    {ok, Encoded, PSt1} = PMod:encode(Data, PSt),
    ?debug("Encoded = ~p", [Encoded]),
    Res = case Mode of
	      bt  -> rfcomm:send(Sock, Encoded);
	      tcp -> gen_tcp:send(Sock, Encoded)
	  end,
    ?debug("send Res = ~p", [Res]),
    {noreply, St#st{packet_st = PSt1}};

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

%% An accept reference we've setup now has accetpted an
%% incoming connection.
handle_info({rfcomm, ARef, { accept, BTAddr, _ } },
	    #st { mod = Mod,
		  func = Fun,
		  args = Arg,
		  listen_ref = LRef,
		  accept_ref = ARef,
		  channel = Channel } = St)  ->

    ?info("~p:handle_info(): bt_connection from ~w:~w\n",
	  [?MODULE, BTAddr,Channel]),
    bt_listener:accept_ack(ok, LRef, BTAddr, Channel),
    Mod:Fun(self(), BTAddr, Channel, accepted, Arg),
    { noreply, St#st { remote_addr = BTAddr,
		       channel = Channel,
		       accept_ref = undefined} };


handle_info({rfcomm, _ConnRef, {data, Data}},
	    #st { remote_addr = BTAddr,
		  channel = Channel,
		  packet_mod = PMod,
		  packet_st = PSt,
		  mod = Mod,
		  func = Fun } = State) ->
    ?debug("~p:handle_info(data): Data: ~p", [ ?MODULE, Data]),
    ?info("~p:handle_info(data): From: ~p:~p ", [ ?MODULE, BTAddr, Channel]),
    ?info("~p:handle_info(data): ~p:~p -> ~p:~p",
	  [ ?MODULE, BTAddr, Channel, Mod, Fun]),
    case PMod:decode(Data, fun(Elems) ->
				   handle_elements(Elems, State)
			   end, PSt) of
	{ok, PSt1} ->
	    {noreply, State#st{packet_st = PSt1}};
	{error, Reason} ->
	    ?error("decode failed: ~p", [Reason]),
	    {stop, Reason, State}
    end;


handle_info({rfcomm, ConnRef, closed},
	    #st { remote_addr = BTAddr,
		  channel = Channel,
		  listen_ref = ListenRef,
		  mod = Mod,
		  func = Fun,
		  args = Arg } = State) ->
    ?debug("~p:handle_info(bt_closed): BTAddr: ~p:~p ", [ ?MODULE, BTAddr, Channel]),
    Mod:Fun(self(), BTAddr, Channel, closed, Arg),
    bt_connection_manager:delete_connection_by_pid(self()),
    rfcomm:close(ConnRef),

    %% Fire up a new accept process to take care of the next incomign connectionX
    gen_server:start_link(?MODULE, {accept,
				    Channel,
				    ListenRef,
				    Mod,
				    Fun,
				    Arg},[]),

    %% Stop this process.
    {stop, normal, State};


handle_info({rfcomm, ConnRef, error},
	    #st { remote_addr = BTAddr,
		  channel = Channel,
		  mod = Mod,
		  func = Fun,
		  args = Arg} = State) ->

    ?debug("~p:handle_info(tcp_error): BTAddr: ~p:~p ", [ ?MODULE, BTAddr, Channel]),
    Mod:Fun(self(), BTAddr, Channel, error, Arg),
    rfcomm:close(ConnRef),
    bt_connection_manager:delete_connection_by_pid(self()),
    {stop, normal, State};

handle_info({tcp, Sock, Data}, #st{remote_addr = IP,
				   channel = Port,
				   rfcomm_ref = Sock,
				   packet_mod = PMod,
				   packet_st = PSt} = St) ->
    ?debug("handle_info(data): From: ~p:~p", [IP, Port]),
    case PMod:decode(Data, fun(Elems) ->
				   handle_elements(Elems, St)
			   end, PSt) of
	{ok, PSt1} ->
	    inet:setopts(Sock, [{active, once}]),
	    {noreply, St#st{packet_st = PSt1}};
	{error, Reason} ->
	    ?error("decode failed, Reason = ~p", [Reason]),
	    {stop, Reason, St}
    end;
handle_info({inet_async, _L, _Ref, {ok, Sock}} = Msg, #st{mod = Mod,
							  func = Fun,
							  args = Arg} = St) ->
    ?debug("~p:handle_info(~p)", [?MODULE, Msg]),
    inet_db:register_socket(Sock, inet_tcp),
    inet:setopts(Sock, [{active, once}]),
    {ok, {BTAddr, Channel}} = inet:peername(Sock),
    Mod:Fun(self(), BTAddr, Channel, accepted, Arg),
    {noreply, St#st{rfcomm_ref = Sock,
		    remote_addr = BTAddr}};

handle_info(_Info, State) ->
    ?warning("~p:handle_info(): Unknown info: ~p", [ ?MODULE, _Info]),
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
get_module_config(Key, Default, CS) ->
    rvi_common:get_module_config(dlink_tcp, dlink_tcp_rpc, Key, Default, CS).

handle_elements(Elements, #st{remote_addr = BTAddr,
			      channel = Channel,
			      mod = Mod,
			      func = Fun,
			      args = Arg}) ->
    ?debug("data complete; processed: ~p",
	   [authorize_keys:abbrev(Elements)]),
    Mod:Fun(self(), BTAddr, Channel, data, Elements, Arg).
