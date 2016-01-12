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
-module(connection).

-behaviour(gen_server).
-include_lib("lager/include/log.hrl").


%% API

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([setup/7]).
-export([send/2]).
-export([send/3]).
-export([send_data/2]).
-export([is_connection_up/1]).
-export([is_connection_up/2]).
-export([terminate_connection/1]).
-export([terminate_connection/2]).
-export([get_source_address/1]).


-define(SERVER, ?MODULE).
-define(PACKET_MOD, dlink_data_json).

-record(st, {
	  ip = {0,0,0,0},
	  port = 0,
	  sock = undefined,
	  mod = undefined,
	  func = undefined,
	  args = undefined,
	  packet_mod = ?PACKET_MOD,
	  packet_st = [],
	  decode_st = <<>>,
	  frag_opts = [],
	  cs
	 }).

%%%===================================================================
%%% API
%%%===================================================================
%% MFA is to deliver data received on the socket.

setup(Role, IP, Port, Sock, Mod, Fun, CS) when Role==client; Role==server ->
    ?debug("setup(~p, ~p, Sock, ~p, ~p, ~p)", [IP, Port, Mod, Fun, CS]),
    case gen_server:start_link(connection, {Role, IP, Port, Sock, Mod, Fun, CS},[]) of
	{ ok, GenSrvPid } = Res ->
	    gen_tcp:controlling_process(Sock, GenSrvPid),
	    gen_server:cast(GenSrvPid, {activate_socket, Sock}),
	    Res;

	Err ->
	    Err
    end.

send(Pid, Data) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Data}).

send(Pid, Data, Opts) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Data, Opts});
send(IP, Port, Data) ->
    case connection_manager:find_connection_by_address(IP, Port) of
	{ok, Pid} ->
	    gen_server:cast(Pid, {send, Data});

	_Err ->
	    ?info("connection:send(): Connection ~p:~p not found for data: ~p",
		  [ IP, Port, Data]),
	    not_found

    end.

send_data(Pid, Data) ->
    gen_server:cast(Pid, {send_data, Data}).

terminate_connection(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, terminate_connection).

terminate_connection(IP, Port) ->
    case connection_manager:find_connection_by_address(IP, Port) of
	{ok, Pid} ->
	    gen_server:call(Pid, terminate_connection);

	_Err -> not_found
    end.


is_connection_up(Pid) when is_pid(Pid) ->
    is_process_alive(Pid).

is_connection_up(IP, Port) ->
    case connection_manager:find_connection_by_address(IP, Port) of
	{ok, Pid} ->
	    is_connection_up(Pid);

	_Err ->
	    false
    end.

get_source_address(Pid) ->
    gen_server:call(Pid, get_source_address).

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
init({Role, IP, Port, Sock, Mod, Fun, CompSpec}) ->
    ?debug("connection:init(): self():   ~p", [self()]),
    ?debug("connection:init(): IP:       ~p", [IP]),
    ?debug("connection:init(): Port:     ~p", [Port]),
    ?debug("connection:init(): Sock:     ~p", [Sock]),
    ?debug("connection:init(): Module:   ~p", [Mod]),
    ?debug("connection:init(): Function: ~p", [Fun]),
    {ok, PktMod} = get_module_config(packet_mod, ?PACKET_MOD, CompSpec),
    PktSt = PktMod:init(CompSpec),
    {ok, FragOpts} = get_module_config(
		       frag_opts, [{packet_mod, {PktMod, PktSt}}], CompSpec),
    {ok, #st{
	    ip = IP,
	    port = Port,
	    sock = Sock,
	    mod = Mod,
	    func = Fun,
	    packet_mod = PktMod,
	    packet_st = PktSt,
	    frag_opts = FragOpts,
	    cs = rvi_common:save_source_address(
		   Role, Sock, rvi_common:set_value(role, Role, CompSpec))
	   }}.

get_module_config(Key, Default, CS) ->
    rvi_common:get_module_config(dlink_tcp, dlink_tcp_rpc, Key, Default, CS).

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

handle_call(get_source_address, _, #st{cs = CS} = St) ->
    {reply, rvi_common:get_source_address(CS), St};
handle_call(terminate_connection, _From, St) ->
    ?debug("~p:handle_call(terminate_connection): Terminating: ~p",
	   [ ?MODULE, {St#st.ip, St#st.port}]),

    {stop, Reason, NSt} = handle_info({tcp_closed, St#st.sock}, St),
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
handle_cast({send, Data},  #st{packet_mod = PMod, packet_st = PSt} = St) ->
    ?debug("~p:handle_cast(send): Sending: ~p",
	   [ ?MODULE, Data]),
    {ok, Encoded, PSt1} = PMod:encode(Data, PSt),
    gen_tcp:send(St#st.sock, Encoded),

    {noreply, St#st{packet_st = PSt1}};
handle_cast({send, Data, Opts}, #st{sock = Socket,
				    packet_mod = PMod,
				    packet_st = PSt,
				    frag_opts = FragOpts} = St) ->
    ?debug("handle_cast({send, Data, ~p, ...), FragOpts = ~p",
	   [Opts, FragOpts]),
    {ok, Bin, PSt1} = PMod:encode(Data, PSt),
    St1 = St#st{packet_st = PSt1},
    rvi_frag:send(Bin, Opts ++ FragOpts, ?MODULE,
		  fun() ->
			  gen_tcp:send(Socket, Bin)
		  end),
    {noreply, St1};
handle_cast({send_data, Data}, #st{sock = Sock} = St) ->
    gen_tcp:send(Sock, Data),
    {noreply, St};
handle_cast({activate_socket, Sock}, State) ->
    Res = inet:setopts(Sock, [{active, once}]),
    ?debug("connection:activate_socket(): ~p", [Res]),
    {noreply, State};


handle_cast(_Msg, State) ->
    ?warning("handle_cast(): Unknown call: ~p", [_Msg]),
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
handle_info({tcp, Sock, Data},
	    #st { ip = undefined } = St) ->
    {ok, {IP, Port}} = inet:peername(Sock),
    NSt = St#st { ip = inet_parse:ntoa(IP), port = Port },
    handle_info({tcp, Sock, Data}, NSt);


handle_info({tcp, Sock, Data},
	    #st { ip = IP,
		  port = Port,
		  packet_mod = PMod,
		  packet_st = PSt,
		  decode_st = DSt,
		  frag_opts = FragOpts} = State) ->
    ?debug("handle_info(~p, PMod=~p, St=~p): From: ~p:~p ",
	   [Data, PMod, PSt, IP, Port]),
    case dlink_data:decode(Data, fun(Elems) ->
					 got_msg(Elems, State)
				 end, DSt, ?MODULE, FragOpts) of
	{ok, DSt1} = Ok ->
	    inet:setopts(Sock, [{active, once}]),
	    {noreply, State#st{decode_st = DSt1}};
	{error, Reason} ->
	    ?error("decode failed, Reason = ~p", [Reason]),
	    {stop, Reason, State}
    end;
    %% case PMod:decode(Data, fun(Elems) ->
    %% 				   handle_elements(Elems, State)
    %% 			   end, PSt) of
    %% 	{ok, PSt1} ->
    %% 	    inet:setopts(Sock, [{active, once}]),
    %% 	    {noreply, State#st{packet_st = PSt1}};
    %% 	{error, Reason} ->
    %% 	    ?error("decode failed, Reason = ~p", [Reason]),
    %% 	    {stop, Reason, State}
    %% end;

handle_info({tcp_closed, Sock},
	    #st { ip = IP,
		  port = Port,
		  mod = Mod,
		  func = Fun,
		  args = Arg } = State) ->
    ?debug("handle_info(tcp_closed): Address: ~p:~p ", [IP, Port]),
    Mod:Fun(self(), IP, Port, closed, Arg),
    gen_tcp:close(Sock),
    connection_manager:delete_connection_by_pid(self()),
    {stop, normal, State};


handle_info({tcp_error, _Sock},
	    #st { ip = IP,
		  port = Port,
		  mod = Mod,
		  func = Fun,
		  args = Arg} = State) ->

    ?debug("handle_info(tcp_error): Address: ~p:~p ", [IP, Port]),
    Mod:Fun(self(), IP, Port, error, Arg),
    connection_manager:delete_connection_by_pid(self()),
    {stop, normal, State};


handle_info(_Info, State) ->
    ?warning("handle_cast(): Unknown info: ~p", [_Info]),
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
    ?debug("terminate(): Reason: ~p ", [_Reason]),
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
%% jsx_decode_stream(Data, St) ->
%%     jsx_decode_stream(Data, St, []).

%% jsx_decode_stream(Data, undefined, Acc) ->
%%     case jsx:decode(Data, [stream, return_tail]) of
%% 	{incomplete, Cont} ->
%% 	    {lists:reverse(Acc), Cont};
%% 	{with_tail, Elems, <<>>} ->
%% 	    {lists:reverse([Elems|Acc]), undefined};
%% 	{with_tail, Elems, Rest} ->
%% 	    jsx_decode_stream(Rest, undefined, [Elems|Acc])
%%     end.

%% decode(Data, PMod, PSt, Mod, Fun, IP, Port, CS) ->
%%     case PMod:decode(Data, PSt) of
%% 	{ok, Elements, PSt1} ->
%% 	    ?debug("data complete: Processed: ~p",
%% 		   [[authorize_keys:abbrev_payload(E) || E <- Elements]]),
%% 	    Mod:Fun(self(), IP, Port, data, Elements, CS),
%% 	    {ok, PSt1};
%% 	{more, Elements, Rest, PSt1} ->
%% 	    ?debug("data complete with Rest: Processed: ~p",
%% 		   [[authorize_keys:abbrev_payload(E) || E <- Elements]]),
%% 	    Mod:Fun(self(), IP, Port, data, Elements, CS),
%% 	    decode(Rest, PMod, PSt1, Mod, Fun, IP, Port, CS);
%% 	{more, PSt1} ->
%% 	    {ok, PSt1};
%% 	{ ->

%% handle_elements(Elements, #st{frag_opts = FragOpts} = St) ->
%%     MaybeF = rvi_frag:maybe_fragment(Elements, ?MODULE, FragOpts),
%%     ?debug("maybe_fragment(~p) -> ~p", [Elements, MaybeF]),
%%     case MaybeF of
%% 	true ->
%% 	    %% It was a fragment, but not a complete message yet
%% 	    St;
%% 	{true, Msg} ->
%% 	    #st{packet_mod = PMod, packet_st = PSt} = St,
%% 	    PMod:decode(Msg, fun(Elems) ->
%% 				     got_msg(Elems, St)
%% 			     end, PSt);
%% 	false ->
%% 	    got_msg(Elements, St)
%%     end.

got_msg(Elements, #st{ip = IP, port = Port,
		      mod = Mod, func = Fun, cs = CS}) ->
    ?debug("data complete: Processed: ~p",
	   [authorize_keys:abbrev(Elements)]),
    Mod:Fun(self(), IP, Port, data, Elements, CS).
