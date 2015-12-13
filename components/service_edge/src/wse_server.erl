%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Web socket server (RFC 6455)
%%% @end
%%% Created :  9 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(wse_server).
-include_lib("lager/include/log.hrl").

-export([start/4, start/5,  stop/1]).
-export([ws_loop/3]).
-export([send/2]).
-export([close/1]).

-compile(export_all).

-define(WS_UUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").
-define(WS_OP_TEXT,   1).
-define(WS_OP_BINARY, 2).
-define(WS_OP_CLOSE,  8).
-define(WS_OP_PING,   9).
-define(WS_OP_PONG,   10).

-define(WSE_DEFAULT_PORT, 8808).

-record(ws_header,
	{
	  host,         %% 'Host'
	  upgrade,      %% 'Upgrade'
	  connection,   %% 'Connection'
	  key,          %% "Sec-WebSocket-Key"
	  protocol,     %% "Sec-WebSocket-Protocol"
	  origin,       %%
	  version,      %% "Sec-WebSocket-Version"
	  cookie,       %% 'Cookie'
	  hs = []
	}).

-record(s,
	{
	  iref = 1,
	  closing=false,        %% false|client|server
	  pingInterval = 10000, %% ping every T ms
	  pongTimeout  = 5000,  %% wait max T ns for pong
	  ping_ref,             %% current ping reference
	  pong_tmr,             %% current pong timeout reference
	  ping_data,            %% current ping data
	  proto,                %% from handshake "bert"?
	  type,                 %% ?WS_OP_TEXT|?WS_OP_BINARY
	  fs   = [],            %% fragments
	  wait = [],            %% #event
	  header,               %% ws_header
	  cb = {undefined, undefined, undefined }
	}).



%% start()
%%  This should be in another module for clarity
%%  but is included here to make the example self-contained


start(Port, M, F, A) when is_integer(Port) ->
    start_([{cb, {M,F,A}}, {port,Port}]).

start(Port,M,F,A, Opts) when is_integer(Port) ->
    start_([{port,Port}, {cb, {M,F,A}}] ++ Opts).

start_(Opts) -> spawn(fun() -> init(Opts) end).

stop(RegName) when is_atom(RegName) ->
    RegName ! stop.


init(Opts) ->
    Port = proplists:get_value(port, Opts, ?WSE_DEFAULT_PORT),

    case proplists:get_value(name, Opts) of
	undefined -> ok;
	Name -> register(Name, self())
    end,
    Addr = proplists:get_value(ifaddr, Opts, any),
    {ok, Listen} = gen_tcp:listen(Port,
				  [{packet,http},{reuseaddr,true},
				   {ifaddr, Addr},
				   {mode, binary}, {active, once}]),
    process_flag(trap_exit, true),
    listen_loop(Listen,Opts).



listen_loop(Listen,Opts) ->
    ?debug("Listen loop ~p\n", [Listen]),
    Parent = self(),
    Pid = spawn_link(fun() -> accept(Parent, Listen, Opts) end),
    ?MODULE:accept_loop(Listen,Opts,Pid).

accept_loop(Listen,Opts,Pid) ->
    ?debug("Accept loop ~p\n", [Listen]),
    receive
	{Pid,ok} ->
	    ?MODULE:listen_loop(Listen,Opts);
	{Pid,Error} ->
	    ?warning("process ~p error: ~p\n", [Pid, Error]),
	    ?MODULE:listen_loop(Listen,Opts);
	{'EXIT',Pid,Reason} ->
	    ?warning("process ~p crashed: ~p\n", [Pid, Reason]),
	    ?MODULE:listen_loop(Listen,Opts);
	{'EXIT',OtherPid,Reason} ->
	    ?debug("other process ~p exited: ~p\n", [OtherPid, Reason]),
	    ?MODULE:accept_loop(Listen, Opts, Pid);
	stop ->
	    gen_tcp:close(Listen),
	    exit(stopped)
    end.

accept(Parent, Listen, Opts) ->
    ?debug("Accept ~p\n", [Listen]),
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    ?debug("Connected to ~p\n", [inet:peername(Socket)]),
	    Parent ! {self(), ok},
	    process_flag(trap_exit, true),
	    put(parent, Parent),
	    ?MODULE:ws_handshake(Socket,Opts);
	Error ->
	    Parent ! {self(), Error}
    end.


send(Pid, Data) ->
    try
	Pid ! { send, Data },
	ok
    catch
	error:Reason ->
	    ?info("wse_server:send(~p) failed : ~s\n", [Pid, Reason]),
	    {error, Reason}
    end.


close(Pid) ->
    try
	Pid ! close,
	ok
    catch
	_:_ -> %% Already closed
	    ok
    end.


%%
ws_encode(Term,?WS_OP_BINARY) ->
    Term;
ws_encode(Term,?WS_OP_TEXT) ->
    Term.

ws_decode(Data, ?WS_OP_BINARY) -> {mesg, Data};
ws_decode(Data, ?WS_OP_TEXT) ->   {mesg, Data};
ws_decode(Data, ?WS_OP_PING) ->   {ping, Data};
ws_decode(Data, ?WS_OP_PONG) ->   {pong, Data};
ws_decode(Data, ?WS_OP_CLOSE) ->  {close,Data}.

ws_handshake(Socket,Opts) ->
    receive
	{http, Socket, _Req={http_request,'GET',Uri,_Version}} ->
	    ?debug("got ws request ~p", [_Req]),
	    ws_handshake(Socket, Uri, Opts);
	{http, _Socket, Req={http_request, _, _, _}} ->
	    ?warning("reject ws request ~p", [Req]),
	    %% send error reply!
	    ws_error({error, bad_request});
	Any ->
	    ?warning("reject ws data ~p", [Any]),
	    ws_error({error, no_data})
    end.

ws_handshake(Socket, _Uri, Opts) ->
    inet:setopts(Socket, [{active, once}]),
    case ws_recv_headers(Socket, #ws_header{}, 1000) of
	Err ={error,_} ->
	    ws_error(Err);
	F when is_list(F#ws_header.key) ->
	    ?debug("got request data: uri=~p, header=~p", [_Uri, F]),
	    %% fixme: check base64! (just crash now)
	    %% ?debug("Random = ~w", [base64:decode(F#ws_header.key)]),
	    Accept1 = [F#ws_header.key, ?WS_UUID],
	    Accept2 = crypto:hash(sha, Accept1),
	    Accept  = base64:encode(Accept2),
	    WsAccept = ["Sec-Websocket-Accept:",Accept,"\r\n"],
	    ?debug("Accept = ~w", [Accept]),
	    WsProto = if is_list(F#ws_header.protocol) ->
			      ["Sec-Websocket-Protocol:",
			       hd(string:tokens(F#ws_header.protocol, ",")),
			       "\n\n"];
			 true -> []
		      end,
	    Handshake =
		[
		 "HTTP/1.1 101 Switching Protocols\r\n",
		 "Upgrade: websocket\r\n",
		 "Connection: Upgrade\r\n",
		 WsAccept,
		 WsProto,
		 "\r\n"],
	    gen_tcp:send(Socket, Handshake),
	    ?debug("ws_server: sent: ~p", [Handshake]),
	    inet:setopts(Socket, [{packet, 0},{active,once}]),
	    PingInterval = proplists:get_value(pingInterval,Opts,10000),
	    PongTimeout = proplists:get_value(pongTimeout,Opts,5000),
	    Type = case proplists:get_value(type,Opts,binary) of
		       binary -> ?WS_OP_BINARY;
		       text -> ?WS_OP_TEXT
		   end,
	    %% Store header in process dictionary for direct access
	    put(header, F#ws_header.hs),
	    S0 = #s {proto=WsProto,
		     type=Type,
		     pingInterval=PingInterval,
		     pongTimeout=PongTimeout,
		     header = F,
		     cb = proplists:get_value(cb, Opts, {undefined, undefined, undefined})
		    },
	    S1 = start_ping_timer(S0),
	    ws_loop(<<>>, Socket, S1);
	true ->
	    ws_error({error, missing_key})
    end.

ws_error(Error) ->
    ?error("~w", [Error]),
    Error.

ws_recv_headers(S, F, Timeout) ->
    receive
	{http, S, http_eoh} ->
	    F;
	{http, S, {http_header, _, K, _, V}} ->
	    inet:setopts(S, [{active, once}]),
	    %% Save all in hs
	    F1 = F#ws_header { hs = [{K,V}|F#ws_header.hs]},
	    case K of
		'Host' ->
		    ws_recv_headers(S, F1#ws_header { host = V}, Timeout);
		'Upgrade' ->
		    ws_recv_headers(S, F1#ws_header { upgrade = V}, Timeout);
		'Connection' ->
		    ws_recv_headers(S, F1#ws_header { connection = V}, Timeout);
		"Sec-Websocket-Key" ->
		    ws_recv_headers(S, F1#ws_header { key = V}, Timeout);
		"Sec-Websocket-Protocol" ->
		    ws_recv_headers(S, F1#ws_header { protocol = V}, Timeout);
		"Sec-Websocket-Version" ->
		    ws_recv_headers(S, F1#ws_header { version = V}, Timeout);
		'Cookie' ->
		    ws_recv_headers(S, F1#ws_header { cookie = V}, Timeout);
		_ ->
		    ws_recv_headers(S, F1, Timeout)
	    end
    after Timeout ->
	    {error, timeout}
    end.



ws_loop(Buf, Socket, S) ->
    receive
	%% WebSocket stuff
	{tcp, Socket, Data} ->
	    ws_data(Buf, Data, Socket, S);

	{tcp_closed, Socket} ->
	    ?debug("tcp_closed ~w", [Socket]),
	    exit(closed);

	{'EXIT',Pid,Reason} ->
	    case get(parent) of
		Pid ->
		    ?debug("exit from parent ~w reason=~p\n", [Pid, Reason]),
		    exit(Reason);
		_ ->
		    ?debug("exit from ~w reason=~p\n", [Pid, Reason]),
		    ws_loop(Buf, Socket, S)
	    end;

	Message ->
	    ?debug("handle_local: ~p - ~p", [Message, S]),
	    case handle_local(Message, Socket, S) of
		{noreply,S1} ->
		    ws_loop(Buf, Socket, S1);
		{stop,normal} ->
		    ok;
		{stop,Reason} ->
		    exit(Reason)
	    end
    end.



ws_data(Buf, Data, Socket, S) ->
    case <<Buf/binary, Data/binary>> of
	%% masked data
	<<Fin:1,_Rsv:3,Op:4,1:1,126:7,L:16,M:4/binary,Frag:L/binary,Buf1/binary>> ->
	    Frag1 = ws_mask(M, Frag),
	    S1 = ws_fragment(Socket, Fin, Op, Frag1, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	<<Fin:1,_Rsv:3,Op:4,1:1,127:7,L:64,M:4/binary,Frag:L/binary,Buf1/binary>> ->
	    Frag1 = ws_mask(M, Frag),
	    S1 = ws_fragment(Socket,Fin, Op, Frag1, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	<<Fin:1,_Rsv:3,Op:4,1:1,L:7,M:4/binary,Frag:L/binary,Buf1/binary>> ->
	    Frag1 = ws_mask(M, Frag),
	    S1 = ws_fragment(Socket,Fin, Op, Frag1, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	%% non masked data
	<<Fin:1,_Rsv:3,Op:4,0:1,126:7,L:16,Frag:L/binary,Buf1/binary>> ->
	    S1 = ws_fragment(Socket,Fin, Op, Frag, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	<<Fin:1,_Rsv:3,Op:4,0:1,127:7,L:64,Frag:L/binary,Buf1/binary>> ->
	    S1 = ws_fragment(Socket,Fin, Op, Frag, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	<<Fin:1,_Rsv:3,Op:4,0:1,L:7,Frag:L/binary,Buf1/binary>> ->
	    S1 = ws_fragment(Socket,Fin, Op, Frag, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	Buf1 -> %% handle to large messages and mal formed
	    inet:setopts(Socket, [{active, once}]),
	    ?MODULE:ws_loop(Buf1, Socket, S)
    end.

ws_mask(<<M:32>>, Frag) ->
    Frag1 = << <<(X bxor M):32>> || <<X:32>> <= Frag >>,
    Sz = byte_size(Frag),
    case Sz band 3 of
	0 -> Frag1;
	SzA ->
	    Sz0 = Sz-SzA,
	    SzB = 4-SzA,
	    <<_:Sz0/unit:8, Xa:SzA/unit:8>> = Frag,
	    <<X:32>> = <<Xa:SzA/unit:8,0:SzB/unit:8>>,
	    <<Yi:SzA/unit:8,_:SzB/unit:8>> = <<(X bxor M):32>>,
	    <<Frag1/binary,Yi:SzA/unit:8>>
    end.

ws_fragment(Socket,1, Op, Frag, S) ->
    Payload = iolist_to_binary(lists:reverse([Frag|S#s.fs])),
    Message = ws_decode(Payload,Op),
    handle_remote(Message, Socket, S#s { fs=[] });

ws_fragment(_Socket, 0, _Op, Frag, S) ->
    %% ?debug("collect fragment: Op=~w, Frag=~p", [_Op,Frag]),
    S#s { fs = [Frag|S#s.fs ]}.


ws_opcode(0) -> continuation;
ws_opcode(?WS_OP_TEXT) -> text;
ws_opcode(?WS_OP_BINARY) -> binary;
ws_opcode(?WS_OP_CLOSE) -> close;
ws_opcode(?WS_OP_PING) -> ping;
ws_opcode(?WS_OP_PONG) -> pong;
ws_opcode(Op) -> Op.

ws_make_server_frame(Payload0,Type) ->
    Fin = 1,
    ws_make_frame(Fin,Type,<<>>, Payload0).

ws_make_client_frame(Payload0,Type) ->
    Fin = 1,
    M = crypto:rand_bytes(4),
    Payload = ws_mask(M, Payload0),
    ws_make_frame(Fin,Type,M,Payload).


ws_make_frame(Fin, Op, Mask, Data) ->
    L = byte_size(Data),
    M = if Mask =:= <<>> -> 0; true -> 1 end,
    %% ?debug("payload size = ~w, mask=~w\n", [L,M]),
    if L < 126 ->
	    <<Fin:1,0:3,Op:4,M:1,L:7,Mask/binary,Data/binary>>;
       L < 65536 ->
	    <<Fin:1,0:3,Op:4,M:1,126:7,L:16,Mask/binary,Data/binary>>;
       true ->
	    <<Fin:1,0:3,Op:4,M:1,127:7,L:64,Mask/binary,Data/binary>>
    end.


handle_local({ send,Data},Socket,S0) ->
    ?debug("wse_server:send(): ~p", [ Data]),
    gen_tcp:send(Socket, ws_make_server_frame(Data, S0#s.type)),
    { noreply,S0 };

handle_local(close,Socket,S0) ->
    ?debug("wse_server:close()"),
    gen_tcp:send(Socket, ws_make_server_frame(<<"unknown">>,?WS_OP_CLOSE)),
    {noreply, S0#s { closing=server }};

handle_local({timeout,Ref,ping},Socket,S0) when S0#s.ping_ref =:= Ref ->
    %% ping the browser!
    PingData = crypto:rand_bytes(4),
    %% ?debug("sending ping ~p\n", [PingData]),
    Frame = ws_make_server_frame(<<PingData/binary>>,?WS_OP_PING),
    gen_tcp:send(Socket, Frame),
    S1 = start_pong_timer(S0#s { ping_data=PingData, ping_ref=undefined }),
    {noreply, S1};

handle_local({timeout,Ref,pong},_Socket,S0) when S0#s.pong_tmr =:= Ref ->
    ?debug("timeout waiting for pong ~p, stopping\n", [S0#s.ping_data]),
    {stop, not_responding};

handle_local(Other,_Socket,S0) ->
    ?warning("handle_local: got ~p~n",[Other]),
    {noreply,S0}.

%%
%% Handle remote operations and replies
%%
handle_remote({ping,Data}, Socket, S0) ->
    %% ?debug("got ping ~p, sending pong ~p", [Data]),
    gen_tcp:send(Socket, ws_make_server_frame(Data,?WS_OP_PONG)),
    S0;

handle_remote({pong,Data}, _Socket, S0) ->
    if Data =:= S0#s.ping_data ->
	    %% ?debug("got pong reply: ~p", [Data]),
	    S1 = stop_pong_timer(S0),
	    start_ping_timer(S1);
       true ->
	    ?debug("got heartbeat pong: ~p", [Data]),
	    S0
    end;

handle_remote({close,Data}, Socket, S0) ->
    if S0#s.closing =:= server ->
	    ?debug("got close ~p, both sides closed", [Data]),
	    gen_tcp:close(Socket),
	    exit(Data);
       S0#s.closing =:= false ->
	    ?debug("got close ~p, client closing", [Data]),
	    gen_tcp:send(Socket, ws_make_server_frame(Data,?WS_OP_CLOSE)),
	    S0#s { closing = client }
    end;

handle_remote({mesg, Mesg}, _Socket, #s { cb = {M,F,A} } = S) ->
    %% Parameters are delivered as JSON. Decode into tuple
    _Pid = spawn_link(M,F,[self(), Mesg, A ]),
    S.
handle_mesg(_Other, _Socket, S0) ->
    ?debug("unknown mesg ~p\n", [_Other]),
    S0.


start_ping_timer(S0) ->
    if is_integer(S0#s.pingInterval),S0#s.pingInterval>0 ->
	    Ref = erlang:start_timer(S0#s.pingInterval, self(), ping),
	    S0#s { ping_ref = Ref, ping_data = undefined };
       true ->
	    S0
    end.

start_pong_timer(S0) ->
    if is_integer(S0#s.pongTimeout),S0#s.pongTimeout>0 ->
	    Ref = erlang:start_timer(S0#s.pongTimeout, self(), pong),
	    S0#s { pong_tmr = Ref };
       true ->
	    S0
    end.

stop_pong_timer(S0) ->
    Tmr = S0#s.pong_tmr,
    if is_reference(Tmr) ->
	    erlang:cancel_timer(Tmr),
	    receive
		{timeout,Tmr,pong} ->
		    ok
	    after 0 ->
		    ok
	    end,
	    S0#s { pong_tmr = undefined };
       true ->
	    S0
    end.
