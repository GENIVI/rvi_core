%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2006 - 2014, Rogvall Invest AB, <tony@rogvall.se>
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
%%%-------------------------------------------------------------------
%%% File    : obex.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : OBEX implemenation
%%%
%%% Created : 19 May 2006 by Tony Rogvall <tony@PBook.local>
%%%-------------------------------------------------------------------
-module(obex).

-behaviour(gen_server).

%% API
-export([open/2, open/3, close/1]).
-export([connect/1, connect/2]).
-export([disconnect/1, disconnect/2]).
-export([put_chunk_start/2, put_chunk/2, put_chunk_end/2]).
-export([put/3]).
-export([get_chunk_start/2, get_chunk/1]).
-export([get/2]).
-export([abort/2]).
-export([setpath/2, setpath/3]).
-export([command/2]).
%% SERVER API
-export([server/2, server_link/2]).
-export([server_init/6]).


%% Xtra - API
-export([connect_dm_client/1]).
-export([connect_syncml_client/1]).
-export([connect_folder_browsing/1]).
-export([set_cwd/2, make_dir/2]).
-export([list_dir/1]).
-export([capability/1, object_profile/2]).
-export([client/4, client_link/4]).
-export([client/3, client_link/3]).
-export([time_iso/1, utc_time_iso/1]).

%% Debug
-export([encode_headers/1, decode_headers/1, decode_packet/2]).
-export([size_header/2, size_headers/1]).
-export([encode_header/2]).
-export([make_packets/4, make_get_packets/2]).

-import(lists, [reverse/1, member/2, foldl/3]).

-include("../include/obex.hrl").
-include("../include/uuid.hrl").
-include("../include/sdp.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ifdef(debug).
-define(dbg(Fmt,As), io:format("~s:~w:" Fmt "\n", [?FILE,?LINE | As])).
-else.
-define(dbg(Fmt,As), ok).
-endif.


-record(obex_packet,
	{
	  final,     %% 0 or 1
	  opcode,    %% 7 bit opcode
	  args,      %% extra args (list)
	  headers,   %% headers
	  callback   %% packet reply callback
	 }).

-record(s,
	{
	  role,       %% client | server
	  transport,  %% transport module
	  ref,        %% transport reference
	  sref,       %% pid - server handle
	  smod,       %% server module
	  sstate,     %% server state
	  channel,    %% channel/port number
	  id,         %% connection ID when connected
	  %% Host side info
	  host_mtu = ?OBEX_DEFAULT_MTU,  %% our recieve buffer size
	  host_flags = 0,                %% out Flags
	  host_version = ?OBEX_VERSION,  %% our OBEX version
	  %% Peer side info
	  peer_mtu = ?OBEX_DEFAULT_MTU,   %% peer recive buffer size
	  peer_flags = 0,                 %% peer flags
	  peer_version = 0,               %% peer OBEX version
	  %% Receive buffer
	  recv_remain = -3,    %% data remain to be received
	  recv_buffer = <<>>,  %% input buffer
	  recv_queue = [],     %% input packet queue
	  %% Send buffer
	  send_queue = [],     %% output packet queue
	  %% 
	  opts=[]     %% obex options
	 }).


-define(size_byte_sequence(Value),
	if is_binary((Value)) -> size((Value));
	   is_list((Value)) -> length((Value))
	end).

-define(size_unicode(Value), 
	if (Value) == "" -> 0;
	   true -> 2*(length((Value))+1)
	end).

%% Note: The length field includes it self and the tag!
-define(enc_byte_sequence(ID, Value),
	if is_binary((Value)) ->
		[<<(ID):8, (size((Value))+3):16>>, Value];
	   is_list((Value)) ->
		[<<(ID):8, (length((Value))+3):16>>, Value]
	end).

%% For null terminate byte-ascii sequences 
-define(enc_null_sequence(ID, Value),
	if is_binary((Value)) ->
		[<<(ID):8, (size((Value))+4):16>>, Value, 0];
	   is_list((Value)) ->
		[<<(ID):8, (length((Value))+4):16>>, Value, 0]
	end).

%%====================================================================
%% API
%%====================================================================
open(Address, Channel) ->
    open(Address, Channel, [{transport,rfcomm}]).

open(Address, Channel, Opts) ->
    client_link(Address, Channel, Opts).

close(Obex) ->
    gen_server:call(Obex, close).

%%
%% OBEX CONNECT
%%
connect(Obex) ->
    connect(Obex, []).

connect(Obex, Headers) ->
    gen_server:call(Obex, {connect,Headers}, 10000).

%%
%% OBEX DISCONNECT
%%
disconnect(Obex) ->
    disconnect(Obex, []).

disconnect(Obex, Headers) ->
    gen_server:call(Obex, {disconnect,Headers}, 10000).

%%
%% OBEX PUT
%%
put_chunk_start(Obex, Headers) ->
    gen_server:call(Obex, {put_chunk_start, Headers}, 10000).
put_chunk(Obex, Data) ->
    gen_server:call(Obex, {put_chunk, Data}, 10000).
put_chunk_end(Obex, Data) -> 
    gen_server:call(Obex, {put_chunk_end, Data}, 10000).

put(Obex, Headers, Body) ->
    case put_chunk_start(Obex, Headers) of
	continue ->
	    put_chunk_end(Obex,Body);
	Error -> Error
    end.

%%
%% OBEX GET
%%
get_chunk_start(Obex, Headers) ->
    gen_server:call(Obex, {get_chunk_start, Headers}, 10000).
get_chunk(Obex) ->
    gen_server:call(Obex, get_chunk, 10000).
    
get(Obex, Headers) ->
    case get_chunk_start(Obex, Headers) of
	{continue,Hs} ->
	    get_continue(Obex,[Hs]);
	{ok,Hs} -> get_reply([Hs]);
	Error -> Error
    end.

get_continue(Obex, Acc) ->
    case get_chunk(Obex) of
	{continue,Hs} -> get_continue(Obex,[Hs|Acc]);
	{ok,Hs} ->	 get_reply([Hs|Acc]);
	Error -> Error
    end.

%% Generate a sorted and useful combined reply
get_reply([]) ->
    {ok,[],<<>>};
get_reply([Hs|HsList]) ->
    get_reply(reverse(Hs),HsList,[],[]).

get_reply([],[],HAcc,BAcc) ->
    {ok,reverse(HAcc),list_to_binary(BAcc)};
get_reply([{bodyEnd,B}|Hs],HsList,HAcc,BAcc) ->
    get_reply(Hs, HsList, HAcc, [B | BAcc]);
get_reply([{body,B}|Hs],HsList,HAcc,BAcc) ->
    get_reply(Hs, HsList, HAcc, [B | BAcc]);
get_reply([{H,V}|Hs],HsList,HAcc,BAcc) ->
    get_reply(Hs, HsList, [{H,V}|HAcc], BAcc);
get_reply([], [Hs|HsList], HAcc, BAcc) ->
    get_reply(reverse(Hs), HsList, HAcc, BAcc).

%%
%% OBEX ABORT
%%
abort(Obex, Headers) ->
    gen_server:call(Obex, {abort,Headers}, 10000).

%%
%% OBEX SETPATH
%%
setpath(Obex,Hs) ->
    setpath(Obex,[],Hs).

setpath(Obex,Opts,Hs) ->
    gen_server:call(Obex, {setpath,Opts,Hs}, 10000).

%%
%% COMMAND
%%
command(Obex, Hs) ->
    gen_server:call(Obex, {command, Hs}, 10000).

%%
%% UTIL COMMANDS
%%

%% Special - simpified connectes
connect_dm_client(Obex) ->
    connect(Obex, [{target,?TARGET_SYNCML_DM}]).

connect_syncml_client(Obex) ->
    connect(Obex, [{target,?TARGET_SYNCML_SYNC}]).

connect_folder_browsing(Obex) ->
    connect(Obex, [{target,?TARGET_FOLDER_BROWSING}]).

%% requires - connect_folder_browsing
set_cwd(Obex, "..") ->
    setpath(Obex,[backup,dont_create],[]);
set_cwd(Obex, Dir) ->
    setpath(Obex,[dont_create],[{name,Dir}]).

%% both connect_folder_browsing and file transfer
make_dir(Obex, Dir) ->
    setpath(Obex,[],[{name,Dir}]).

%% requires - connect_folder_browsing
list_dir(Obex) ->
    case get(Obex, [{type,"x-obex/folder-listing"}]) of
	{ok,_Hs,Bin} ->
	    makeup:string(Bin);
	Error ->
	    Error
    end.

%% requires - connect []
capability(Obex) -> 
    get(Obex, [{type,"x-obex/capability"}]).

%% requires - connect []
object_profile(Obex, MimeType) ->
    get(Obex, [{type,"x-obex/object-profile"},
	       {name,MimeType}]).

%%
%% START Functions
%%
client(Address, Channel, Opts) ->
    gen_server:start(?MODULE, [Address,Channel,Opts], []). 

client(Name, Address, Channel, Opts) ->
    gen_server:start({local,Name},?MODULE,[Address,Channel,Opts],[]).

client_link(Address, Channel, Opts) ->
    gen_server:start_link(?MODULE, [Address,Channel,Opts], []).     

client_link(Name, Address, Channel, Opts) ->
    gen_server:start_link({local,Name},?MODULE,[Address,Channel,Opts],[]).
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Address,Channel,Opts]) ->
    case transport(Opts, Address) of
	undefined -> {stop, {error, unknown_transport}};
	Transport ->
	    Mod = transport_module(Transport),
	    S0 = #s { role = client,
		      transport = Mod,
		      host_version = ?OBEX_VERSION, 
		      host_mtu = getopt(mtu,Opts,?OBEX_DEFAULT_MTU),
		      host_flags = getopt(mtu,Opts,0),
		      opts = Opts
		     },
	    TransOpts = getopt(transport_options, Opts, [binary]),
	    case catch Mod:open(Address,Channel,TransOpts) of
		{ok,Ref} ->
		    {ok, S0#s{ ref=Ref }};
		{'EXIT',Reason} ->
		    {stop, {error, Reason}};
		Error ->
		    {stop, Error}
	    end
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({connect,Hs},From,S) ->
    Args = [S#s.host_version, S#s.host_flags, S#s.host_mtu],
    P = #obex_packet { final   = 1,
		       opcode  = connect,
		       args    = Args,
		       headers = Hs,
		       callback = fun(S0,#obex_packet { opcode={ok,What},
							headers=Hs1,
							args=[Version,Flags1,Mtu]
						       }) ->
					  %% Keep track on connectionID
					  %% this simplifies alot...
					  ID=getopt(connectionID,Hs1),
					  gen_server:reply(From, {ok,What}),
					  {noreply,S0#s { id = ID,
							  peer_mtu = Mtu,
							  peer_flags=Flags1,
							  peer_version = Version }};
				     (S0,#obex_packet { opcode=Error }) ->
					  gen_server:reply(From, Error),
					  {noreply, S0}
				  end
		      },
    send_packet(P, S);

handle_call({disconnect,Hs}, From, S) ->
    P = #obex_packet { final = 1,
		       opcode = disconnect,
		       headers = prepare_headers(Hs,S),
		       callback = fun(S0,#obex_packet { opcode=Reply }) ->
					  gen_server:reply(From, Reply),
					  {noreply,S0#s { id=undefined }}
				  end
		      },
    send_packet(P, S);

handle_call({abort,Hs}, From, S) ->
    P = #obex_packet { final  = 1,
		       opcode = abort,
		       headers = prepare_headers(Hs,S), %% needed?
		       callback = fun(S0,#obex_packet { opcode=Reply }) ->
					  gen_server:reply(From, Reply),
					  {noreply,S0}
				  end
		      },
    send_packet(P, S);

handle_call({get_chunk_start,Hs}, From, S) ->
    Hs1 = prepare_headers(Hs, S),
    handle_get_chunk_start(Hs1, From, S);
 
handle_call(get_chunk, From, S) ->
    handle_get_chunk(From, S);

handle_call({put_chunk_start,Hs}, From, S) ->
    Hs1 = prepare_headers(Hs, S),
    handle_put_chunk_start(Hs1, From, S);

handle_call({put_chunk,Data}, From, S) ->
    handle_put_chunk(Data, From, S);

handle_call({put_chunk_end,Data}, From, S) ->
    handle_put_chunk_end(Data, From, S);

handle_call({setpath,Opts,Hs}, From, S) ->
    %% MUST FIT in one packet
    Constants = 0,  %% reserved MUST be 0 (version=1.2)
    Flags = case member(backup, Opts) of
		true -> 16#01;
		false -> 16#00
	    end bor
	case member(dont_create, Opts) of
	    true -> 16#02;
	    false -> 16#00
	end,
    P = #obex_packet { final = 1,
		       opcode = setpath,
		       args = [Flags, Constants],
		       headers = prepare_headers(Hs, S),
		       callback = fun(S0,#obex_packet { opcode=Reply }) ->
					  gen_server:reply(From, Reply),
					  {noreply,S0}
				  end
		      },
    send_packet(P, S);

handle_call({command,Hs}, From, S) ->
    P = #obex_packet { final = 1,
		       opcode = command,
		       headers = prepare_headers(Hs, S),
		       callback = fun (S0,_) ->
					  gen_server:reply(From, ok),
					  {noreply,S0} 
				  end
		      },
    send_packet(P, S);
    
handle_call(close, From, S) ->
    if S#s.ref == undefined ->
	    {stop, normal, ok, S};
       true ->
	    Callback = fun(S0,_) ->
			       gen_server:reply(From, ok),
			       ?dbg("Transport close: ~w\n", [S#s.ref]),
			       (S#s.transport):close(S#s.ref),
			       {stop, normal, S0#s { ref=undefined }}
		       end,
	    P = #obex_packet { final     = 1,
			       callback  = Callback,
			       opcode = undefined,  %% sync packet
			       headers = [] },
	    send_packet(P, S)
    end;
    
handle_call(_Request, _From, S) ->
    {reply, {error, bad_call}, S}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, S) ->
    {noreply, S}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({rfcomm,Ref,{data,Data}}, S) when S#s.ref == Ref ->
    ?dbg("RFCOMM: Packet received: ~p\n", [Data]),
    recv(Data, S);
handle_info({tcp,Ref,Data}, S) when S#s.ref == Ref ->
    ?dbg("TCP: Packet received: ~p\n", [Data]),
    recv(Data, S);
handle_info({rfcomm,Ref,closed}, S) when S#s.ref == Ref ->
    ?dbg("RFCOMM: closed\n", []),
    recv_closed(S);
handle_info({tcp_closed,Ref}, S) when S#s.ref == Ref ->
    ?dbg("TCP: closed\n", []),
    recv_closed(S);
handle_info(_Info, S) ->
    ?dbg("Unhandled INFO ~p\n", [_Info]),
    {noreply, S}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, S) ->
    if S#s.ref == undefined ->
	    ok;
       true ->
	    ?dbg("Transport close: ~w\n", [S#s.ref]),
	    (S#s.transport):close(S#s.ref)
    end.


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Get transport from options 
transport(Opts) ->
    transport(Opts, undefined).

transport(Opts, Address) ->
    case getopt(transport, Opts) of
	undefined ->
	    if Address == undefined -> rfcomm;
	       true ->
		    case inet_parse:address(Address) of
			{ok, _} -> tcp;
			_ ->
			    case bt_util:getaddr(Address) of
				{ok,_} -> rfcomm;
				_ ->
				    case inet_parse:domain(Address) of
					true -> tcp;
					_ -> undefined
				    end
			    end
		    end
	    end;
	Transport when is_atom(Transport) -> Transport;
	_ -> undefined
    end.

transport_module(rfcomm) -> obex_rfcomm;
transport_module(tcp) -> obex_tcp;
transport_module(T) -> list_to_atom("obex_" ++ atom_to_list(T)).

server_link(Channel, Opts) ->
    case transport(Opts) of
	undefined ->
	    {error, unknown_transport};
	Transport ->
	    Mod = transport_module(Transport),
	    Mod:server_link(Channel, Opts)
    end.    

server(Channel, Opts) ->
    case transport(Opts) of
	undefined ->
	    {error, unknown_transport};
	Transport ->
	    Mod = transport_module(Transport),
	    Mod:server(Channel, Opts)
    end.


%% A new connection from Transport backend
server_init(SrvHandle, Ref, _Address, Channel, Transport, Opts) ->
    ?dbg("obex [~w] Accept from ~w on channel=~w\n", 
	 [Transport,_Address,Channel]),
    Wait = #obex_packet { final = 1, 
			  opcode = wait, 
			  callback = fun handle_connect/2 },
    S = #s { role         = server,
	     sref         = SrvHandle,
	     transport    = Transport,
	     ref          = Ref,
	     channel      = Channel,
	     host_mtu     = getopt(mtu,Opts,?OBEX_DEFAULT_MTU),
	     host_flags   = getopt(flags,Opts,0),
	     host_version = ?OBEX_VERSION,
	     opts         = Opts,
	     send_queue   = [Wait]
	    },
    gen_server:enter_loop(?MODULE, [], S).

%% handle connect request
handle_connect(S0,#obex_packet { opcode=connect,
				 headers=Hs,
				 args=[Version,Flags,Mtu] }) ->
    ?dbg("handle_connect: version=~w, flags=~w, mtu=~w, hs=~w",
	 [Version,Flags,Mtu,Hs]),
    S = S0#s { peer_version=Version, peer_mtu=Mtu,peer_flags=Flags },
    case lists:keysearch(target, 1, Hs) of
	false ->
	    handle_connect_target(S, undefined, obex_generic);
	{value,{_,Target}} ->
	    case (S#s.transport):lookup(S#s.sref, Target) of
		{error, not_found} ->
		    Packet = #obex_packet { final = 1,
					    opcode = {error,not_found},
					    headers = [],
					    callback = fun handle_connect/2 },
		    send_packet(Packet, S);
		{ok,Module} ->
		    handle_connect_target(S, Target, Module)
	    end
    end;
handle_connect(S,P=#obex_packet { opcode=_Opcode }) ->
    ?dbg("handle_connect: opcode ~w", [_Opcode]),
    TS = obex_generic:init(S#s.sref, S#s.opts, S#s.peer_mtu),
    handle_request(S#s { smod=obex_generic, sstate=TS},P).
    
%%
%% Connected to target handled by Mod
%%
handle_connect_target(S, Target, Mod) ->
    TS = Mod:init(S#s.sref, S#s.opts, S#s.peer_mtu),
    case target_connect(Target, S#s { smod=Mod, sstate=TS}) of
	{reply,{ok,Hs}, S1} ->
	    Packet = #obex_packet { final = 1,
				    opcode = connect_response,
				    args=[S#s.host_version,
					  S#s.host_flags,
					  S#s.host_mtu],
				    headers = Hs,
				    callback = fun handle_request/2 },
	    send_packet(Packet, S1);
	{reply,Error,S1} ->
	    Packet = #obex_packet { final = 1,
				    opcode = Error,
				    headers = [],
				    callback = fun handle_request/2 },
	    send_packet(Packet, S1)
    end.

%%
%% Collect ALL get headers before doing the target callback
%%
next_get(HsAcc) ->
    fun(S, #obex_packet { final=0, opcode=get, headers=Hs }) ->
	    ?dbg("GET continue request ~p\n", [Hs]),
	    Packet = #obex_packet { final=0, opcode=continue, headers=[],
				    callback=next_get(HsAcc++Hs) },
	    send_packet(Packet, S);
       (S, P=#obex_packet { final=1, opcode=get, headers=Hs }) ->
	    handle_request(S,P#obex_packet { headers=HsAcc++Hs });
       (S, _P=#obex_packet { final=1, opcode=abort }) ->
	    Packet = #obex_packet { final=0, opcode={ok,success}, headers=[],
				    callback=fun handle_request/2 },
	    send_packet(Packet, S);
       (S, _P) ->
	    ?dbg("Got ~p while collecting GET headers",[_P#obex_packet.opcode]),
	    Packet = #obex_packet { final=0, opcode={error,conflict}, headers=[],
				    callback=fun handle_request/2 },
	    send_packet(Packet, S)
    end.
%%
%% Collect ALL put headers before doing the target callback
%%
next_put(HsAcc) ->
    fun(S, #obex_packet { final=0, opcode=put, headers=Hs }) ->
	    ?dbg("PUT continue request ~p\n", [Hs]),
	    continue_put(S,Hs,HsAcc);
       (S, #obex_packet { final=1, opcode=put, headers=Hs }) ->
	    final_put(S,Hs,HsAcc);
       (S, _P=#obex_packet { final=1, opcode=abort }) ->
	    Packet = #obex_packet { final=0, opcode={ok,success}, headers=[],
				    callback=fun handle_request/2 },
	    send_packet(Packet, S);
       (S, _P) ->
	    ?dbg("Got ~p while collecting PUT headers",[_P#obex_packet.opcode]),
	    Packet = #obex_packet { final=0, opcode={error,conflict}, 
				    headers=[],
				    callback=fun handle_request/2 },
	    send_packet(Packet, S)
    end.


continue_put(S,Hs,HsAcc) ->
    case split_headers(Hs) of
	{true,Hs1,Data} -> %% bodyEnd in non final? trailer?
	    ?dbg("got bodyEnd in continue put packet!", []),
	    {reply,Reply,S1} = target_put(HsAcc++Hs1, Data, S),
	    Packet = #obex_packet { final=1, opcode=Reply,
				    headers=[],
				    callback=fun handle_request/2
				   },
	    send_packet(Packet, S1);
	{false,Hs1,<<>>} -> %% no data no end
	    Packet = #obex_packet { final=1, opcode=continue,
				    headers=[],
				    callback=next_put(HsAcc++Hs1)
				   },
	    send_packet(Packet,S);
	{false,Hs1,Data} -> %% data found not end
	    {reply,Reply,S1} = target_put(HsAcc++Hs1, Data, S),
	    Packet = #obex_packet { final=1, opcode=Reply,
				    headers=[],
				    callback=fun handle_request/2 },
	    send_packet(Packet,S1)
    end.

final_put(S,Hs,HsAcc) ->
    case split_headers(Hs) of
	{true,Hs1,Data} ->
	    case target_put(HsAcc++Hs1, Data, S) of
		{reply,continue,S1} ->
		    {reply,Reply,S2} = target_put([],<<>>,S1),
		    Packet = #obex_packet { final=1, opcode=Reply,
					    headers=[],
					    callback=fun handle_request/2
					   },
		    send_packet(Packet,S2);
		{reply,Reply,S1} ->
		    Packet = #obex_packet { final=1, opcode=Reply,
					    headers=[],
					    callback=fun handle_request/2
					   },
		    send_packet(Packet,S1)
	    end;
	{false,_Hs1,_Data} ->
	    ?dbg("missing bodyEnd in final put packet!", []),
	    Packet = #obex_packet { final=1, opcode={error,bad_request},
				    headers=[],
				    callback=fun handle_request/2},
	    send_packet(Packet, S)
    end.    

%%
%% Send GET reply packets
%%
send_reply_packets([], _End, S) ->
    ?dbg("send_reply_packets: NO packets to send???",[]),
    {noreply, S};
send_reply_packets([P], End, S) ->
    P1 = if End == true ->
		 P#obex_packet { final=1,opcode={ok,success},
				 callback=fun handle_request/2};
	    true ->
		 P#obex_packet { final=1, opcode=continue,
				 callback=fun handle_request/2}
	 end,
    send_packet(P1, S);
send_reply_packets([P|Ps], End, S) ->
    Callback = fun(S0,_Pi=#obex_packet { opcode=get }) -> %% ignoring final!!!
		       send_reply_packets(Ps, End, S0);
		  (S0,Pi) ->
		       handle_request(S0, Pi)
	       end,
    P1 = P#obex_packet { final=1, opcode=continue, callback=Callback },
    send_packet(P1, S).

%%
%% Handle GET
%%
handle_request(S,#obex_packet { final=1, opcode=get, headers=Hs }) ->
    ?dbg("GET final=1 request ~p\n", [Hs]),
    case target_get(Hs, S) of
	{reply,{continue,ReplyHs,Data},S1} ->
	    Ps = make_packets(ReplyHs, Data, false, S1#s.peer_mtu),
	    send_reply_packets(Ps, false, S1);
	{reply,{ok,ReplyHs,Data},S1} ->
	    Ps = make_packets(ReplyHs, Data, true, S1#s.peer_mtu),
	    send_reply_packets(Ps, true, S1);
	{reply,Error,S1} ->
	    Packet = #obex_packet { final=1, 
				    opcode=Error,
				    headers=[],
				    callback=fun handle_request/2},
	    send_packet(Packet, S1)
    end;
handle_request(S,#obex_packet { final=0, opcode=get, headers=Hs }) ->
    ?dbg("GET continue request ~p\n", [Hs]),
    Packet = #obex_packet { final=0, opcode=continue, headers=[],
			    callback=next_get(Hs) },
    send_packet(Packet, S);
%%
%% Handle PUT (collect headers until we find body or bodyEnd)
%%
handle_request(S,#obex_packet { final=0, opcode=put, headers=Hs}) ->
    ?dbg("PUT continue request ~p\n", [Hs]),
    continue_put(S,Hs,[]);
handle_request(S,#obex_packet { final=1, opcode=put, headers=Hs}) ->
    ?dbg("PUT final request ~p\n", [Hs]),
    final_put(S,Hs,[]);

%%
%% Handle SETPATH
%%
handle_request(S,#obex_packet { final=1, opcode=setpath, args=Args, headers=Hs }) ->
    ?dbg("SETPATH hs=~p\n", [Hs]),
    {reply,Reply,S1} = target_setpath(Hs,Args,S),
    Packet = #obex_packet { final=1, opcode=Reply, headers=[],
			    callback=fun handle_request/2 },
    send_packet(Packet, S1);
%%
%% ABORT
%%
handle_request(S,#obex_packet { final=1, opcode=abort, headers=Hs }) ->
    ?dbg("ABORT hs=~p\n", [Hs]),
    {reply,Reply,S1} = target_abort(Hs, S),
    Packet = #obex_packet { final=1, opcode=Reply, headers=[],
			    callback=fun handle_request/2 },
    send_packet(Packet, S1);
%%
%% DISCONNECT
%%
handle_request(S,#obex_packet { final=1, opcode=disconnect, headers=Hs }) ->
    ?dbg("DISCONNECT hs=~p\n", [Hs]),
    {reply,Reply,S1} = target_disconnect(Hs, S),
    Packet = #obex_packet { final=1, opcode=Reply, headers=[],
			    callback=fun handle_request/2 },
    send_packet(Packet, S1);
%%
%% COMMAND
%%
handle_request(S,#obex_packet { final=1, opcode=command, headers=Hs }) ->
    ?dbg("COMMAND hs=~p\n", [Hs]),
    {reply,Reply,S1} = target_command(Hs, S), 
    Packet = #obex_packet { final=1, opcode=Reply, headers=[],
			    callback=fun handle_request/2 },
    send_packet(Packet, S1);
%%
%% OTHER packets (final=0 strange opcodes etc)
%%
handle_request(S,_P) ->
    ?dbg("OTHER packet=~p", [_P]),
    Packet = #obex_packet { final=1, opcode={error,bad_request}, headers=[],
			    callback=fun handle_request/2 },
    send_packet(Packet, S).
    

%% Wrap calls to target
target_connect(Target, S) ->
    {reply,Reply,TS} = (S#s.smod):handle_connect(Target,S#s.sstate),
    {reply,Reply,S#s { sstate = TS}}.

target_disconnect(Hs, S) ->
    {reply,Reply,TS} = (S#s.smod):handle_disconnect(Hs,S#s.sstate),
    {reply,Reply,S#s { sstate = TS}}.

target_get(Hs, S) ->
    {reply,Reply,TS} = (S#s.smod):handle_get(Hs,S#s.sstate),
    {reply,Reply,S#s { sstate = TS}}.

target_put(Hs, Data, S) ->
    {reply,Reply,TS} = (S#s.smod):handle_put(Hs,Data,S#s.sstate),
    {reply,Reply,S#s { sstate = TS}}.

target_abort(Hs, S) ->
    {reply,Reply,TS} = (S#s.smod):handle_abort(Hs,S#s.sstate),
    {reply,Reply,S#s { sstate = TS}}.

target_setpath(Hs,Args,S) ->
    {reply,Reply,TS} = (S#s.smod):handle_setpath(Hs,Args,S#s.sstate),
    {reply,Reply,S#s { sstate = TS}}.

target_command(Hs, S) ->
    {reply,Reply,TS} = (S#s.smod):handle_command(Hs,S#s.sstate),
    {reply,Reply,S#s { sstate = TS}}.


%% update header with connection id 
prepare_headers(Hs, S) ->
    if S#s.id == undefined ->
	    Hs;
       true ->
	    [{connectionID,S#s.id}|Hs]
    end.

handle_get_chunk_start(Hs, From, S) ->
    Packets = make_get_packets(Hs, S#s.peer_mtu),
    send_get_packets(Packets, From, S).

handle_get_chunk(From, S) ->
    send_get_packet_1(#obex_packet { opcode=get, headers=[] }, From, S).

handle_put_chunk_start(Hs, From, S) ->
    Packets = make_packets(Hs, <<>>, false, S#s.peer_mtu),
    send_put_packets(Packets, 0, From, S).

handle_put_chunk(Data, From, S) ->
    Packets = make_packets([], Data, false, S#s.peer_mtu),
    send_put_packets(Packets, 0, From, S).

handle_put_chunk_end(Data, From, S) ->
    Packets = make_packets([], Data, true, S#s.peer_mtu),
    send_put_packets(Packets, 1, From, S).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% send_get_packets:
%%   Send the GET header packets util all packets have been sent
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_get_packets([], From, S) ->
    %% send a empty get packet
    send_get_packet_1(#obex_packet { opcode=get, headers=[] }, From, S);
send_get_packets([P], From, S) ->
    send_get_packet_1(P, From, S);
send_get_packets([P|Ps], From, S) ->
    Callback = fun(S0,_Pi=#obex_packet { final=0, opcode=_Reply}) ->
		       ?dbg("send_get_packets: input=~p\n", [_Pi]),
		       send_get_packets(Ps, From, S0);
		  (S0, _Pi=#obex_packet { final=1, opcode=Reply}) ->
		       ?dbg("send_get_packets: input=~p\n", [_Pi]),
		       gen_server:reply(From, Reply),
		       {noreply,S0}
	       end,
    Pout = P#obex_packet { final=0, callback=Callback},
    send_packet(Pout, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Send the GET final=1 packet
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_get_packet_1(P, From, S) ->
    Callback = 
	fun (S0,_P=#obex_packet { final=1,opcode=continue,headers=Hs}) ->
		?dbg("send_get_packet_1: input=~p\n", [_P]),
		gen_server:reply(From, {continue,Hs}),
		{noreply,S0};
	    (S0, _P=#obex_packet { final=1,opcode=Reply,headers=Hs}) ->
		?dbg("send_get_packet_1: input=~p\n", [_P]),
		case Reply of
		    {ok,success} ->
			gen_server:reply(From, {ok,Hs});
		    Error ->
			gen_server:reply(From, Error)
		end,
		{noreply,S0};
	    (S0,_P=#obex_packet { final=0,opcode=_Reply,headers=Hs}) ->
		?dbg("send_get_packet_1: input=~p\n", [_P]),
		%% request more request headers !!!
		gen_server:reply(From, {more, Hs}),
		{noreply,S0}
	end,
    Packet = P#obex_packet { final= 1, callback = Callback },
    send_packet(Packet, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% send_put_packets:
%%   Send the PUT packets util all packets have been sent
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
send_put_packets([], _Finally, _From, S) ->
    {noreply, S};
send_put_packets([P], Finally, From, S) ->
    Callback = fun(S0,_Pi=#obex_packet { final=0, opcode=Reply}) ->
		       ?dbg("handle_put: input=~p\n", [_Pi]),
		       gen_server:reply(From, Reply),
		       {noreply,S0};
		  (S0, _Pi=#obex_packet { final=1, opcode=Reply}) ->
		       ?dbg("handle_put: input=~p\n", [_Pi]),
		       gen_server:reply(From, Reply),
		       {noreply,S0}
	       end,
    Pout = P#obex_packet { opcode=put, final=Finally, callback=Callback},
    send_packet(Pout, S);
send_put_packets([P|Ps], Finally, From, S) ->
    Callback = fun(S0,_Pi=#obex_packet { final=0, opcode=_Reply}) ->
		       ?dbg("handle_put: input=~p\n", [_Pi]),
		       send_put_packets(Ps, Finally, From, S0);
		  (S0, _Pi=#obex_packet { final=1, opcode=Reply}) ->
		       ?dbg("handle_put: input=~p\n", [_Pi]),
		       gen_server:reply(From, Reply),
		       {noreply,S0}
	       end,
    Pout = P#obex_packet { opcode=put, final=0, callback=Callback},
    send_packet(Pout, S).
    
recv_closed(S) when S#s.role == server ->
    ?dbg("server connection closed (normal)\n", []),
    {stop, normal, S};
recv_closed(S) when S#s.role == client ->
    ?dbg("client connection closed\n", []),
    {noreply, S#s { ref=undefined, id=undefined }}.

recv(Data, S) ->
    Buf = recv_concat(Data, S#s.recv_buffer),
    BufLen = size(Buf),
    Remain = S#s.recv_remain,
    if Remain < 0 ->
	    if BufLen + Remain < 0 ->
		    {noreply, S#s { recv_remain=BufLen+Remain, 
				    recv_buffer = Buf }};
	       true ->
		    <<_OP:8,PacketLen:16,_/binary>> = Buf,
		    if BufLen < PacketLen ->
			    {noreply, S#s { recv_remain=PacketLen-BufLen,
					    recv_buffer=Buf }};
		       BufLen >= PacketLen ->
			    <<Packet:PacketLen/binary,Buf1/binary>> = Buf,
			    recv_packet(Packet, Buf1, S#s { recv_remain=0,
							    recv_buffer=(<<>>) 
							   })
		    end
	    end;
       Remain > 0 ->
	    if BufLen < Remain ->
		    {noreply, S#s { recv_remain=Remain - BufLen,
				    recv_buffer=Buf }};
	       true ->
		    <<_OP:8,PacketLen:16,_/binary>> = Buf,
		    <<Packet:PacketLen/binary, Buf1/binary>> = Buf,
		    recv_packet(Packet, Buf1, S#s { recv_remain=0,
						    recv_buffer=(<<>>) })
	    end
    end.

recv_packet(Data, Buf, S) ->
    Cmd = case S#s.send_queue of
	      [P|_] -> P#obex_packet.opcode;
	      [] -> undefined
	  end,
    Reply = decode_packet(Data, Cmd),
    ?dbg("recv_packet: ~p\n", [Reply]),
    S1 = push_packets(Buf, S),  %% push rest of packet data
    case S1#s.send_queue of
	[#obex_packet { callback=Callback }|_] ->
	    %% Note that the callback may send packets, but they
	    %% are queued after current packet.
	    case Callback(S1, Reply) of
		{noreply, S2} ->
		    Q = tl(S2#s.send_queue),
		    send_packet(S2#s { send_queue = Q });
		{stop,Reason,Reply,S2} ->
		    {stop,Reason,Reply,S2};
		{stop,Reason,S2} ->
		    {stop,Reason,S2}
	    end;
	[] ->
	    ?dbg("Reply not expected\n",[]),
	    {noreply, S1}
    end.

decode_opcode(Code) ->
    case Code band 16#7f of
	?OBEX_CMD_CONNECT -> connect;
	?OBEX_CMD_DISCONNECT -> disconnect;
	?OBEX_CMD_PUT -> put;
	?OBEX_CMD_GET -> get;
	?OBEX_CMD_COMMAND -> command;
	?OBEX_CMD_SETPATH -> setpath;
	?OBEX_CMD_ABORT -> abort;

	%% 1x
	?OBEX_RSP_CONTINUE   -> continue;
	?OBEX_RSP_SWITCH_PRO -> switch_protocols;

	%% 2x
	?OBEX_RSP_SUCCESS   -> {ok,success};
	?OBEX_RSP_CREATED  -> {ok,created};
	?OBEX_RSP_ACCEPTED -> {ok,accepted};
	?OBEX_RSP_NO_CONTENT -> {ok,no_content};
	?OBEX_RSP_RESET_CONTENT -> {ok, reset_content};
	?OBEX_RSP_PARTIAL_CONTENT -> {ok, partial_content};

	%% 3x
	?OBEX_RSP_MULTIPLE_CHOICES -> {error,multiple_choices};
	?OBEX_RSP_MOVED_PERMANENTLY -> {error,moved_permanently};
	?OBEX_RSP_MOVED_TEMPORARILY -> {error,moved_temporarily};
	?OBEX_RSP_SEE_OTHER         -> {error,see_other};
	?OBEX_RSP_NOT_MODIFIED      -> {error,not_modified};
	?OBEX_RSP_USE_PROXY         -> {error,use_proxy};

	%% 4x
	?OBEX_RSP_BAD_REQUEST -> {error,bad_request};
	?OBEX_RSP_UNAUTHORIZED -> {error,unauthorized};
	?OBEX_RSP_PAYMENT_REQUIRED -> {error,payment_required};
        ?OBEX_RSP_FORBIDDEN   -> {error, forbidden};
        ?OBEX_RSP_NOT_FOUND   -> {error, not_found};
        ?OBEX_RSP_METHOD_NOT_ALLOWED -> {error,not_allowed};
        ?OBEX_RSP_CONFLICT -> {error, conflict};
        ?OBEX_RSP_GONE -> {error, gone};
	?OBEX_RSP_LENGTH_REQUIRED -> {error, length_required};
	?OBEX_RSP_PRECONDITION_FAILED -> {error, precondition_failed};
	?OBEX_RSP_ENTITY_TOO_LARGE -> {error, entity_too_large};
	?OBEX_RSP_URL_TOO_LARGE -> {error, url_too_large};
	?OBEX_RSP_UNSUPPORTED_MEDIA -> {error, unsupported_media};

	%%
	?OBEX_RSP_INTERNAL_SERVER_ERROR -> {error, internal_server_error};
	?OBEX_RSP_NOT_IMPLEMENTED -> {error, not_implemented};
	?OBEX_RSP_BAD_GATEWAY -> {error, bad_gateway};
	?OBEX_RSP_SERVICE_UNAVAILABLE -> {error, service_unavailable};
	?OBEX_RSP_GATEWAY_TIMEOUT -> {error, gateway_timeout};
	?OBEX_RSP_HTTP_VERSION_NOT_SUPPORTED -> {error, http_version_not_supported};

	%% 
	?OBEX_RSP_DATABASE_FULL -> {error, database_full};
	?OBEX_RSP_DATABASE_LOCKED -> {error, database_locked};
	%%
	Code1 ->
	    case (Code1 bsr 4) of
		2 -> {ok,Code1};
		_ -> {error,Code1}
	    end
    end.


encode_opcode(Code) ->
    case Code of
	%% 0x
	connect -> ?OBEX_CMD_CONNECT;
	disconnect -> ?OBEX_CMD_DISCONNECT;
	put -> ?OBEX_CMD_PUT;
	get -> ?OBEX_CMD_GET;
	command -> ?OBEX_CMD_COMMAND;
	setpath -> ?OBEX_CMD_SETPATH;
	abort -> ?OBEX_CMD_ABORT;
	%% 1x
	continue -> ?OBEX_RSP_CONTINUE;
	switch_protocols -> ?OBEX_RSP_SWITCH_PRO;
	%% 2x
	{ok,success} -> ?OBEX_RSP_SUCCESS;
	{ok,created} -> ?OBEX_RSP_CREATED;
	{ok,accetped} -> ?OBEX_RSP_ACCEPTED;
	{ok,no_content} -> ?OBEX_RSP_NO_CONTENT;
	%% 3x
	{error,multiple_choices} -> ?OBEX_RSP_MULTIPLE_CHOICES;
	{error,moved_permanently} -> ?OBEX_RSP_MOVED_PERMANENTLY;
	{error,moved_temporarily} -> ?OBEX_RSP_MOVED_TEMPORARILY;
	{error,see_other} -> ?OBEX_RSP_SEE_OTHER;
	{error,not_modified} -> ?OBEX_RSP_NOT_MODIFIED;
	{error,use_proxy} -> ?OBEX_RSP_USE_PROXY;

	%% 4x
	{error,bad_request} -> ?OBEX_RSP_BAD_REQUEST;
	{error,unauthorized} -> ?OBEX_RSP_UNAUTHORIZED;
	{error,payment_required} -> ?OBEX_RSP_PAYMENT_REQUIRED;
        {error,forbidden} -> ?OBEX_RSP_FORBIDDEN;
        {error,not_found} -> ?OBEX_RSP_NOT_FOUND;
        {error,not_allowed} -> ?OBEX_RSP_METHOD_NOT_ALLOWED;
        {error,conflict} -> ?OBEX_RSP_CONFLICT;
        {error, gone} -> ?OBEX_RSP_GONE;
	{error, length_required} -> ?OBEX_RSP_LENGTH_REQUIRED;
	{error, precondition_failed} -> ?OBEX_RSP_PRECONDITION_FAILED;
	{error, entity_too_large} -> ?OBEX_RSP_ENTITY_TOO_LARGE;
	{error, url_too_large} -> ?OBEX_RSP_URL_TOO_LARGE;
	{error, unsupported_media} -> ?OBEX_RSP_UNSUPPORTED_MEDIA;
	%% 5x
	{error,internal_server_error} -> ?OBEX_RSP_INTERNAL_SERVER_ERROR;
	{error,not_implemented} -> ?OBEX_RSP_NOT_IMPLEMENTED;
	%% 6x
	{error,database_full} -> ?OBEX_RSP_DATABASE_FULL;
	{error,database_locked} -> ?OBEX_RSP_DATABASE_LOCKED;
	%%
	{ok,Code} when Code >= 0, Code =< 15 -> Code;
	{error,Code} -> Code
    end.

%% push back extra packet data (ABORT?)
%% (should never happend but why should we not try to support pipelining ...)
push_packets(Buf, S) when size(Buf) < 3 ->
    S#s { recv_remain = -3 + size(Buf), recv_buffer = Buf }; 
push_packets(Buf = <<_Op:8, PacketLen:16,_>>, S) ->
    DataLen = size(Buf),
    if DataLen >= PacketLen ->
	    <<Packet:PacketLen/binary, Buf1/binary>> = Buf,
	    Qs = S#s.recv_queue ++ [Packet],
	    push_packets(Buf1, S#s { recv_queue = Qs });
       true ->
	    S#s { recv_remain = PacketLen-DataLen,
		  recv_buffer = Buf }
    end.

%%
%%  Generate a sequence of packets with 
%%  Headers body and bodyEnd
%%  End == false  - Do not generated bodyEnd headers
%%  Hs  == []     - Only generate body or bodyEnd headers
%%  Data == <<>>  - No data possible a empty dataEnd is generated
%%
make_packets(Hs, Data, End, Mtu) ->
    make_headers(Hs,Mtu-3,Mtu-3,End,Data,[],[]).

make_headers([{Key,Value}|Hs],Remain,Mtu,End,Data,HsAcc,PsAcc) ->
    Sz = size_header(Key,Value),
    if Sz =< Remain ->
	    make_headers(Hs,Remain-Sz,Mtu,End,Data, 
			 [{Key,Value}|HsAcc],PsAcc);
       Sz > Mtu ->
	    io:format("WARNING: SIZE(~p,~p)=~w,MTU=~w\n",[Key,Value,Sz,Mtu]),
	    if HsAcc == [] ->
		    P = #obex_packet { headers=[{Key,Value}]},
		    make_headers(Hs,Mtu,Mtu,End,Data,[],[P|PsAcc]);
	       true ->
		    P1 = #obex_packet { headers=reverse(HsAcc) },
		    P2 = #obex_packet { headers=[{Key,Value}]},
		    make_headers(Hs,Mtu,Mtu,End,Data,[],[P2,P1|PsAcc])
	    end;
       true ->
	    P = #obex_packet { headers=reverse(HsAcc) },
	    make_headers(Hs,Mtu-Sz,Mtu,End,Data,[{Key,Value}],[P|PsAcc])
    end;
make_headers([],Remain,Mtu,End,Data,HsAcc,PsAcc) ->
    make_data(Data,Remain,Mtu,End,HsAcc,PsAcc).

make_data(Data,Remain,Mtu,End,HsAcc,PsAcc) ->
    Sz = size(Data)+3, %% DataSize + Tag & length
    if
	Sz =< Remain ->
	    if End == true ->
		    HsAcc1 = [{bodyEnd,Data}|HsAcc],
		    P = #obex_packet { headers=reverse(HsAcc1) }, 
		    reverse([P|PsAcc]);
	       Sz == 3, HsAcc == [] -> %% No data and No End
		    reverse(PsAcc);
	       Sz == 3 ->
		    P = #obex_packet { headers=reverse(HsAcc) },
		    reverse([P|PsAcc]);
	       true ->  %% Data but not Last Chunk
		    HsAcc1 = [{body,Data}|HsAcc],
		    P = #obex_packet { headers=reverse(HsAcc1) },
		    reverse([P|PsAcc])
	    end;
	Remain < 4 ->
	    P = #obex_packet { headers=reverse(HsAcc) },
	    make_data(Data,Mtu,Mtu,End,[],[P|PsAcc]);
	Sz > Remain ->
	    Len = Remain - 3,
	    <<Data1:Len/binary, Data2/binary>> = Data,
	    HsAcc1 = [{body,Data1} | HsAcc],
	    P = #obex_packet { headers=reverse(HsAcc1) },
	    make_data(Data2,Mtu,Mtu,End,[],[P|PsAcc])
    end.

%%
%% Create a sequence of GET packets covering
%% the sending of the initial headers
%%
make_get_packets(Hs, Mtu) ->
    make_get_headers(Hs,Mtu-3,Mtu-3,[],[]).

make_get_headers([{Key,Value}|Hs],Remain,Mtu,HsAcc,PsAcc) ->
    Sz = size_header(Key,Value),
    if Sz =< Remain ->
	    make_get_headers(Hs,Remain-Sz,Mtu,
			     [{Key,Value}|HsAcc],PsAcc);
       Sz > Mtu ->
	    io:format("WARNING: SIZE(~p,~p)=~w,MTU=~w\n", [Key,Value,Sz,Mtu]),
	    if HsAcc == [] ->
		    P = #obex_packet { opcode=get, headers=[{Key,Value}]},
		    make_get_headers(Hs,Mtu,Mtu,[],[P|PsAcc]);
	       true ->
		    P1 = #obex_packet { opcode=get, headers=reverse(HsAcc) },
		    P2 = #obex_packet { opcode=get, headers=[{Key,Value}]},
		    make_get_headers(Hs,Mtu,Mtu,[],[P2,P1|PsAcc])
	    end;
       true ->
	    P = #obex_packet { opcode=get, headers=reverse(HsAcc) },
	    make_get_headers(Hs,Mtu-Sz,Mtu,[{Key,Value}],[P|PsAcc])
    end;
make_get_headers([],_Remain,_Mtu,[],PsAcc) ->
    reverse(PsAcc);    
make_get_headers([],_Remain,_Mtu,HsAcc,PsAcc) ->
    P = #obex_packet { opcode=get,headers=reverse(HsAcc)},
    reverse([P|PsAcc]).

%%
%% split headers info {Regular, Body}
%%
split_headers(Hs) ->
    split_headers(Hs, [], [], false).

split_headers([{body,Data}|Hs], Regular, Body, End) ->
    split_headers(Hs, Regular, [Data|Body], End);
split_headers([{bodyEnd,Data}|Hs], Regular, Body, _End) ->
    split_headers(Hs, Regular, [Data|Body], true);
split_headers([H | Hs], Regular, Body, End) ->
    split_headers(Hs, [H|Regular], Body, End);
split_headers([], Regular, Body, End) ->
    {End, reverse(Regular), list_to_binary(reverse(Body))}.
    
%%
%% Decode packet
%%	    

%% 0x - commands
decode_packet(Data = <<Final:1,?OBEX_CMD_CONNECT:7,_:16,
		      Version:8,Flags:8,MaxPacketLength:16,_/binary>>, _Cmd) ->
    #obex_packet { final   = Final,
		   opcode  = connect,
		   args    = [Version,Flags,MaxPacketLength],
		   headers = decode_headers(Data, 7) };
decode_packet(Data = <<Final:1,?OBEX_CMD_SETPATH:7,_:16,
		      Flags:8,Constants:8,_/binary>>, _Cmd) ->
    #obex_packet { final   = Final,
		   opcode  = setpath,
		   args    = [Flags,Constants],
		   headers = decode_headers(Data, 5) };
%% 2x - ok (connect reply)
decode_packet(Data = <<Final:1,Code:7,_:16,
		      Version,Flags,MaxPacketLength:16,_/binary>>, connect) ->
    #obex_packet { final=Final,
		   opcode  = decode_opcode(Code),
		   args    = [Version,Flags,MaxPacketLength],
		   headers = decode_headers(Data, 7)};
decode_packet(Data = <<Final:1,Code:7,_:16,_/binary>>,_Cmd) ->
    #obex_packet { final   = Final,
		   opcode  = decode_opcode(Code),
		   headers = decode_headers(Data, 3)  }.


recv_concat(Data, []) -> Data;
recv_concat(Data, Buffer) -> <<Buffer/binary, Data/binary>>.

%% send or buffer a packet
send_packet(_P, S) when S#s.ref == undefined ->
    ?dbg("send_packet: transport closed\n", []),
    {reply, {error,closed}, S};
send_packet(P, S) when is_record(P, obex_packet) ->
    if S#s.send_queue == [] ->
	    send_packet(S#s { send_queue = [P]});
       true ->
	    ?dbg("send_packet: queued ~p\n", [P]),
	    Q = S#s.send_queue ++ [P],
            {noreply, S#s { send_queue = Q }}
    end.

%% send the first packet on the queue
send_packet(S) ->
    case S#s.send_queue of
	[] -> 
	    {noreply, S};
	[P=#obex_packet { opcode = undefined, callback=Callback } | Q] ->
	    %% sync / close / packet
	    Callback(S#s { send_queue=Q} , P);

	[P=#obex_packet { final=Final, opcode=command, callback=Callback,
			headers=Hs }| Q] ->
	    ?dbg("send_packet: ~p\n", [P]),
	    Data = encode_packet(Final, ?OBEX_CMD_COMMAND,<<>>, Hs, 
				 S#s.peer_mtu),
	    (S#s.transport):send(S#s.ref, Data),
	    Callback(S#s { send_queue=Q }, P);
	    
	[_P=#obex_packet { final=Final, opcode=connect,
			   args=[Version,Flags,MaxPacketLength],
			   headers=Hs }|_Q] ->
	    ?dbg("send_packet: ~p\n", [_P]),
	    Data = encode_packet(Final, ?OBEX_CMD_CONNECT,
				 <<Version:8,Flags:8,MaxPacketLength:16>>,
				 Hs, 0),
	    (S#s.transport):send(S#s.ref, Data),
	    {noreply,S};
	
	%% Synthetic opcode!!!
	[_P=#obex_packet { final=Final, opcode=connect_response,
			   args=[Version,Flags,MaxPacketLength],
			   headers=Hs }|_Q] ->
	    ?dbg("send_packet: ~p\n", [_P]),
	    Data = encode_packet(Final, ?OBEX_RSP_SUCCESS,
				 <<Version:8,Flags:8,MaxPacketLength:16>>,
				 Hs, 0),
	    (S#s.transport):send(S#s.ref, Data),
	    {noreply,S};

	[_P=#obex_packet { final=Final, opcode=setpath, 
			  args=[Flags,Constants],
			  headers=Hs } | _Q] ->
	    ?dbg("send_packet: ~p\n", [_P]),
	    Data = encode_packet(Final, ?OBEX_CMD_SETPATH,
				 <<Flags:8, Constants:8>>,
				 Hs, S#s.peer_mtu),
	    (S#s.transport):send(S#s.ref, Data),
	    {noreply,S};



	[_P=#obex_packet { final=Final, opcode=Op, 
			   args=undefined,
			   headers=Hs } | _Q] ->
	    ?dbg("send_packet: ~p\n", [_P]),
	    OpCode = encode_opcode(Op),
	    Data = encode_packet(Final, OpCode, <<>>,
				 Hs, S#s.peer_mtu),
	    (S#s.transport):send(S#s.ref, Data),
	    {noreply,S}
    end.
				 


getopt(Key, Opts) ->
    getopt(Key, Opts, undefined).

getopt(Key, Opts, Default) ->
    case lists:keysearch(Key, 1, Opts) of
	{value, {_,Value}} ->
	    Value;
	false ->
	    Default
    end.

decode_headers(Bin) ->
    decode_headers(Bin, 0).

decode_headers(Bin, Offset) ->
    ?dbg("decode_headers: offset=~p\n", [Offset]),
    decode_headers(Bin, Offset, []).

decode_headers(Bin, Offset, Hs) when Offset >= size(Bin) ->
    reverse(Hs);
decode_headers(Bin, Offset, Hs) ->
    ?dbg("decode_headers: offset=~p\n", [Offset]),
    case Bin of
	<<_:Offset/binary,?OBEX_V0:2, ID:6,HeaderLen:16,_/binary>> ->
	    Offset1 = Offset+3,
	    ValueLen = HeaderLen - 3,  %% not counting tag and length
	    <<_:Offset1/binary, Value:ValueLen/binary, _/binary>> = Bin,
	    Offset2 = Offset1+ValueLen,
	    Val = decode_unicode(Value, 0, []),
	    case ((?OBEX_V0 bsl 6) bor ID) of
		?OBEX_HDR_NAME ->
		    ?dbg("header: name=~p\n", [Val]),
		    decode_headers(Bin,Offset2,
				   [{name,Val}|Hs]);
		?OBEX_HDR_DESCRIPTION ->
		    ?dbg("header: desciption=~p\n", [Val]),
		    decode_headers(Bin,Offset2,
				   [{description,Val}|Hs]);
		ID1 ->
		    ?dbg("header: ~w=~p\n", [ID1,Val]),
		    decode_headers(Bin,Offset2,
				   [{ID1,Val}|Hs])
	    end;

	<<_:Offset/binary,?OBEX_Vn:2,ID:6,HeaderLen:16,_/binary>> ->
	    Offset1 = Offset+3,
	    ValueLen = HeaderLen - 3,
	    <<_:Offset1/binary,Value:ValueLen/binary,_/binary>> = Bin,
	    Offset2 = Offset1+ValueLen,
	    case ((?OBEX_Vn bsl 6) bor ID) of
		?OBEX_HDR_TYPE ->
		    Val = decode_ascii(Value, 0, []),
		    ?dbg("header: type=~p\n", [Val]),
		    decode_headers(Bin,Offset2,[{type,Val}|Hs]);
		?OBEX_HDR_TIME ->
		    Val = binary_to_list(Value),
		    ?dbg("header: time=~p\n", [Val]),
		    decode_headers(Bin,Offset2,[{time,Val}|Hs]);
		?OBEX_HDR_TARGET ->
		    Val = binary_to_list(Value),
		    ?dbg("header: target=~p\n", [Val]),
		    decode_headers(Bin,Offset2,[{target,Val}|Hs]);
		?OBEX_HDR_HTTP ->
		    Val = binary_to_list(Value),
		    ?dbg("header: http=~p\n", [Val]),		    
		    decode_headers(Bin,Offset2,[{http,Val}|Hs]);
		?OBEX_HDR_BODY ->
		    ?dbg("header: body=~p\n", [Value]),
		    decode_headers(Bin,Offset2,[{body,Value}|Hs]);
		?OBEX_HDR_BODY_END ->
		    ?dbg("header: bodyEnd=~p\n", [Value]),
		    decode_headers(Bin,Offset2,[{bodyEnd,Value}|Hs]);
		?OBEX_HDR_WHO ->
		    Val = binary_to_list(Value),
		    ?dbg("header: who=~p\n", [Val]),
		    decode_headers(Bin,Offset2,[{who,Val}|Hs]);

		?OBEX_HDR_APPARAM ->
		    ?dbg("header: appparam=~p\n", [Value]),
		    decode_headers(Bin,Offset2,
				   [{appParameters,Value}|Hs]);

		?OBEX_HDR_AUTHCHAL ->
		    ?dbg("header: authChal=~p\n", [Value]),
		    decode_headers(Bin,Offset2,
				   [{authorizationChallange,Value}|Hs]);

		?OBEX_HDR_AUTHRESP ->
		    ?dbg("header: authResp=~p\n", [Value]),
		    decode_headers(Bin,Offset2,
				   [{authorizationResponse,Value}|Hs]);

		?OBEX_HDR_OBJCLASS ->
		    Val = binary_to_list(Value),
		    ?dbg("header: objClass=~p\n", [Val]),
		    decode_headers(Bin,Offset2,[{objectClass,Val}|Hs]);

		ID1 ->
		    ?dbg("header: ~p=~p\n", [ID1,Value]),
		    decode_headers(Bin,Offset2, [{ID1,Value} | Hs])
	    end;

	<<_:Offset/binary,?OBEX_I4:2,ID:6,Value:32,_/binary>> ->
	    Offset1 = Offset+1+4,
	    case ((?OBEX_I4 bsl 6) bor ID)  of
		?OBEX_HDR_COUNT ->
		    ?dbg("header: count=~p\n", [Value]),
		    decode_headers(Bin,Offset1,
				   [{count,Value}|Hs]);
		?OBEX_HDR_CONNECTION_ID ->
		    ?dbg("header: connectionID=~p\n", [Value]),
		    decode_headers(Bin,Offset1,
				   [{connectionID,Value}|Hs]);
		?OBEX_HDR_LENGTH ->
		    ?dbg("header: length=~p\n", [Value]),
		    decode_headers(Bin,Offset1,
				   [{length,Value}|Hs]);
		?OBEX_HDR_TIME2 ->
		    ?dbg("header: time2=~p\n", [Value]),
		    decode_headers(Bin,Offset1,
				   [{time2,Value}|Hs]);
		ID1 ->
		    ?dbg("header: ~p=~p\n", [ID1,Value]),
		    decode_headers(Bin,Offset1,
				   [{ID1,Value} | Hs])
	    end;

	<<_:Offset/binary,?OBEX_I1:2,ID:6,Value:8,_/binary>> ->
	    Offset1 = Offset+1+1,
	    case ((?OBEX_I1 bsl 6) bor ID)  of
		ID1 ->
		    ?dbg("header: ~p=~p\n", [ID1,Value]),
		    decode_headers(Bin,Offset1,
				   [{ID1,Value} | Hs])
	    end
    end.


decode_unicode(Bin, Offset, Acc) when Offset >= size(Bin)  ->
    ?dbg("Warning: unicode string not null terminated\n", []),
    reverse(Acc);
decode_unicode(Bin, Offset, Acc) ->
    case Bin of
	<<_:Offset/binary, 0, 0>> -> reverse(Acc);
	<<_:Offset/binary, Char:16, _/binary>> ->
	    decode_unicode(Bin, Offset+2, [Char|Acc])
    end.

decode_ascii(Bin, Offset, Acc) when Offset >= size(Bin) ->
    reverse(Acc);
decode_ascii(Bin, Offset, Acc) ->
    case Bin of
	<<_:Offset/binary, 0>> ->
	    reverse(Acc);
	<<_:Offset/binary, Char:8, _/binary>> ->
	    decode_ascii(Bin, Offset+1, [Char|Acc])
    end.    

encode_packet(Final, Opcode, BinArgs, Headers, Mtu) ->
    BinHs = encode_headers(Headers),
    PacketLen = 3+size(BinArgs)+size(BinHs),
    if Mtu == 0 -> 
	    ok;
       PacketLen > Mtu ->
	    ?dbg("Waring: packet bigger than peer mtu (~w)\n", [Mtu]);
       true -> ok
    end,
    [<<Final:1, Opcode:7, PacketLen:16>>, BinArgs, BinHs].
    

encode_headers(Hs) ->
    list_to_binary(enc_headers(Hs)).

enc_headers([{Key,Value}|Hs]) ->
    [encode_header(Key,Value) | enc_headers(Hs)];
enc_headers([]) ->
    [].
    
encode_header(Key,Value) ->
    case Key of
	%% null terminated unicode unicode
	name ->
	    encode_unicode(?OBEX_HDR_NAME, Value);
	description ->
	    encode_unicode(?OBEX_HDR_DESCRIPTION,Value);
	ID when is_integer(ID), (ID band ?OBEX_HDR_MASK) == ?OBEX_V0 ->
	    encode_unicode(ID,Value);
	%% byte sequence
	type ->
	    ?enc_null_sequence(?OBEX_HDR_TYPE,Value);
	time ->
	    ?enc_byte_sequence(?OBEX_HDR_TIME,Value);
	target ->
	    ?enc_byte_sequence(?OBEX_HDR_TARGET,Value);
	http ->
	    ?enc_byte_sequence(?OBEX_HDR_HTTP,Value);
	body ->
	    ?enc_byte_sequence(?OBEX_HDR_BODY,Value);
	bodyEnd ->
	    ?enc_byte_sequence(?OBEX_HDR_BODY_END,Value);
	who ->
	    ?enc_byte_sequence(?OBEX_HDR_WHO,Value);
	appParameters ->
	    ?enc_byte_sequence(?OBEX_HDR_APPARAM,Value);
	authorizationChallange ->
	    ?enc_byte_sequence(?OBEX_HDR_AUTHCHAL,Value);
	authorizationResponse ->
	    ?enc_byte_sequence(?OBEX_HDR_AUTHRESP,Value);
	objectClass ->
	    ?enc_byte_sequence(?OBEX_HDR_OBJCLASS,Value);
	ID when is_integer(ID), (ID band ?OBEX_HDR_MASK) == ?OBEX_Vn ->
	    ?enc_byte_sequence(ID, Value);
	%% 4 byte values
	count -> <<?OBEX_HDR_COUNT,Value:32>>;
	length -> <<?OBEX_HDR_LENGTH,Value:32>>;
	time2 ->  <<?OBEX_HDR_TIME2, Value:32>>;
	connectionID -> (<<?OBEX_HDR_CONNECTION_ID,Value:32>>);
	ID when is_integer(ID), (ID band ?OBEX_HDR_MASK) == ?OBEX_I4 ->
	    (<<ID, Value:32>>);
	ID when is_integer(ID), (ID band ?OBEX_HDR_MASK) == ?OBEX_I1 ->
	    (<<ID, Value:8>>)
    end.

%% time header should be in  UTC time
utc_time_iso(Time) when is_list(Time) -> 
    Time;
utc_time_iso(Time={Ms,S,Us}) when is_integer(Ms),is_integer(S),is_integer(Us) ->
    utc_time_iso(calendar:now_to_universal_time(Time));
utc_time_iso({{YYYY,Mon,Day},{H,M,S}}) ->
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0wT~2..0w~2..0w~2..0wZ",
				[YYYY,Mon,Day,H,M,S])).

%% but here is a local version
time_iso(Time) when is_list(Time) -> 
    Time;
time_iso(Time={Ms,S,Us}) 
  when is_integer(Ms),is_integer(S),is_integer(Us) ->
    time_iso(calendar:now_to_local_time(Time));
time_iso({{YYYY,Mon,Day},{H,M,S}}) ->
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0wT~2..0w~2..0w~2..0w",
				[YYYY,Mon,Day,H,M,S])).
%%
%% Calculate size for a header list
%%
size_headers(Hs) when is_list(Hs) ->
    size_headers(Hs, 0).

size_headers([{Key,Value}|Hs], Sz) ->
    size_headers(Hs, size_header(Key,Value)+Sz);
size_headers([], Sz) ->
    Sz.

%%
%% Calculate the size of header
%% Including  <tag> <len:16> <data>
%%
size_header(Key,Value) ->
    case Key of
	%% null terminates
	name ->  3+?size_unicode(Value);
	description -> 3+?size_unicode(Value);
	ID when is_integer(ID), (ID band ?OBEX_HDR_MASK) == ?OBEX_V0 ->
	    3+?size_unicode(Value);
	%% byte sequence
	type ->    4+?size_byte_sequence(Value);  %% (+1 for null termination)
	time ->    3+?size_byte_sequence(Value);
	target ->  3+?size_byte_sequence(Value);
	http ->    3+?size_byte_sequence(Value);
	body ->    3+?size_byte_sequence(Value);
	bodyEnd -> 3+?size_byte_sequence(Value);
	who ->     3+?size_byte_sequence(Value);
	appParameters -> 3+?size_byte_sequence(Value);
	authorizationChallange -> 3+?size_byte_sequence(Value);
	authorizationResponse ->  3+?size_byte_sequence(Value);
	objectClass ->  3+?size_byte_sequence(Value);
	ID when is_integer(ID), (ID band ?OBEX_HDR_MASK) == ?OBEX_Vn ->
	    3+?size_byte_sequence(Value);
	%% 4 byte values
	count -> 5;
	length -> 5;
	time2 -> 5;
	connectionID -> 5;
	ID when is_integer(ID), (ID band ?OBEX_HDR_MASK) == ?OBEX_I4 -> 5;
	ID when is_integer(ID), (ID band ?OBEX_HDR_MASK) == ?OBEX_I1 -> 2
    end.    

%% Note: length field include it self and tag + unicode null termination 0,0
encode_unicode(ID, "") ->  %% speical (test)
    [ID, <<3:16>>];
encode_unicode(ID, Value) ->
    Len = (length(Value)+1)*2 + 3,
    [ID, <<Len:16>> | encode_chars(Value)].

encode_chars([H|T]) ->
    [<<H:16>> | encode_chars(T)];
encode_chars([]) ->
    [<<0:16>>].
