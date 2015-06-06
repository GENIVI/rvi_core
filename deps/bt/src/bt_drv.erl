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
%%% File    : bt_drv.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : Bluetooth driver control
%%%
%%% Created : 28 Jan 2006 by Tony Rogvall <tony@iMac.local>
%%%-------------------------------------------------------------------
-module(bt_drv).

-behaviour(gen_server).

-include("../include/bt.hrl").

%% API
-export([start_link/0, start/0, stop/0]).
-export([ping/0]).
-export([register/2, unregister/1]).
-export([devices/0]).
-export([recent_devices/0]).
-export([paired_devices/0]).
-export([favorite_devices/0]).
-export([device_info/2]).
-export([local_info/1]).
-export([service_info/1, service_info/2]).
-export([service_query/1, service_query/2]).
-export([service_add/1, service_add_persist/1]).
-export([service_del/1]).
-export([service_rfcomm/1]).
-export([connect/1, connect/2, connect/3, disconnect/1]).
-export([remote_name/1, remote_name/2]).

-export([inquiry_start/1,
	 inquiry_stop/1,
	 inquiry_flush/1]).

-export([rfcomm_open/2,
	 rfcomm_close/1,
	 rfcomm_send/2,
	 rfcomm_listen/1,
	 rfcomm_accept/1,
	 rfcomm_accept/2,
	 rfcomm_accept/3,
	 rfcomm_mtu/1,
	 rfcomm_channel/1,
	 rfcomm_address/1]).

-export([l2cap_open/2,
	 l2cap_close/1,
	 l2cap_send/2,
	 l2cap_listen/1,
	 l2cap_accept/1,
	 l2cap_mtu/1,
	 l2cap_psm/1,
	 l2cap_address/1]).

-export([debug/1]).

%% utils
-export([decode_class/1,
	 encode/1, decode/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% -define(debug, true).

-ifdef(debug).
-define(dbg(Fmt,As), io:format("~s:~w:" Fmt "\n", [?FILE,?LINE | As])).
-else.
-define(dbg(Fmt,As), ok).
-endif.

-define(SERVER, bt_drv).

%% -record(session,
%% 	{
%% 	  id,
%% 	  owner,
%% 	  tag,
%% 	  data
%% 	 }).

%% event subscriptions
-record(subscription,
	{
	  id,          %% subscription id (32 bit)
	  subscriber,  %% event subsriber 
	  monitor,     %% monitor on subscriber
	  ref,         %% event reference
	  tag,         %% event tag
	  data         %% subscription data
	 }).

-record(wait,
	{
	  from,    %% gen_server From
	  id,      %% command id
	  data     %% reply style
	 }).

-record(state, 
	{
	  bt_port,
	  cmd_id = 1,         %% 1..16#ffffffff   command id
	  evt_id = 1,         %% 1..16#ffffffff   event id
	  wait = [],          %% #wait
	  subscription = [],  %% #subscription
	  sessions = [],      %% #session
	  reg                 %% table of registrations
	 }).

-define(CMD_PING,             1).
-define(CMD_RECENT_DEVICES,   2).
-define(CMD_PAIRED_DEVICES,   3).
-define(CMD_FAVORITE_DEVICES, 4).
-define(CMD_INQUIRY_START,    5).
-define(CMD_INQUIRY_STOP,     6).
-define(CMD_REMOTE_NAME,      7).
-define(CMD_CONNECT,          8).
-define(CMD_DISCONNECT,       9).

-define(CMD_DEVICE_INFO,      10).
-define(CMD_SERVICE_INFO,     11).
-define(CMD_SERVICE_QUERY,    12).
-define(CMD_SERVICE_ADD,      13).
-define(CMD_SERVICE_DEL,      14).
-define(CMD_SERVICE_RFCOMM,   15).
-define(CMD_LOCAL_INFO,       16).
-define(CMD_DEBUG,            17).

%% RCCOMM Channels
-define(CMD_RFCOMM_OPEN,      20).
-define(CMD_RFCOMM_CLOSE,     21).
-define(CMD_RFCOMM_LISTEN,    22).
-define(CMD_RFCOMM_SEND,      23).
-define(CMD_RFCOMM_ACCEPT,    24).
-define(CMD_RFCOMM_MTU,       25).
-define(CMD_RFCOMM_ADDRESS,   26).
-define(CMD_RFCOMM_CHANNEL,   27).


%% L2CAP 
-define(CMD_L2CAP_OPEN,       30).
-define(CMD_L2CAP_CLOSE,      31).
-define(CMD_L2CAP_LISTEN,     32).
-define(CMD_L2CAP_SEND,       33).
-define(CMD_L2CAP_ACCEPT,     34).
-define(CMD_L2CAP_MTU,        35).
-define(CMD_L2CAP_ADDRESS,    36).
-define(CMD_L2CAP_PSM,        37).


-define(REPLY_OK,    1).
-define(REPLY_ERROR, 2).
-define(REPLY_EVENT, 3).

-define(BOOLEAN,        0).
-define(UINT8,          1).
-define(UINT16,         2).
-define(UINT32,         3).
-define(UINT64,         4).
-define(STRING1,        5).
-define(LIST,           6).
-define(LIST_END,       7).
-define(TUPLE,          8).
-define(TUPLE_END,      9).
-define(ATOM,           10).
-define(BINARY,         11).
-define(INT8,           12).
-define(INT16,          13).
-define(INT32,          14).
-define(INT64,          15).
-define(FLOAT32,        16).
-define(FLOAT64,        17).
-define(STRING4,        18).

-define(ADDR,           100).
-define(DATE,           101).


%% device info codes 
-define(NFO_DEVICE_NAME,             1).
-define(NFO_DEVICE_CLASS,            2).
-define(NFO_DEVICE_CLOCK,            3).
-define(NFO_DEVICE_INQUIRY,          4).
-define(NFO_DEVICE_ACCESS,           5).
-define(NFO_DEVICE_UPDATE,           6).
-define(NFO_DEVICE_IS_FAVORITE,      7).
-define(NFO_DEVICE_IS_PAIRED,        8).
-define(NFO_DEVICE_IS_CONNECTED,     9).

%% local info
-define(NFO_LOCAL_NAME,              1).
-define(NFO_LOCAL_CLASS,             2).
-define(NFO_LOCAL_ADDRESS,           3).
-define(NFO_LOCAL_DISCOVERABLE,      4).
-define(NFO_LOCAL_POWER_STATE,       5).

%% Service Classes
-define(CLASS_LIMITED_DISCOVERABLE, 2#00000000001).
-define(CLASS_POSITIONING,          2#00000001000).
-define(CLASS_NETWORKING,           2#00000010000).
-define(CLASS_RENDERING,            2#00000100000).
-define(CLASS_CAPTURING,            2#00001000000).
-define(CLASS_OBJECT_TRANSFER,      2#00010000000).
-define(CLASS_AUDIO,                2#00100000000).
-define(CLASS_TELEPHONY,            2#01000000000).
-define(CLASS_INFORMATION,          2#10000000000).

%% Miscellaneous
-define(CLASS_MAJOR_MISCELLANEOUS, 16#00). 
%% Desktop, Notebook, PDA, Organizers, etc...
-define(CLASS_MAJOR_COMPUTER, 16#01).
%% Cellular, Cordless, Payphone, Modem, etc...
-define(CLASS_MAJOR_PHONE, 16#02).
%% LAN Access Point
-define(CLASS_MAJOR_LAN_ACCESSPOINT, 16#03).
%% Headset, Speaker, Stereo, etc...
-define(CLASS_MAJOR_AUDIO, 16#04).
%% Mouse, Joystick, Keyboards, etc...
-define(CLASS_MAJOR_PERIPHERAL, 16#05).
%% Printing, scanner, camera, display, etc...
-define(CLASS_MAJOR_IMAGING, 16#06).

%% FIXME: Add minor per major ..

-define(NEXT_ID(N),
	if (N)==16#ffffffff -> 1;
	   true -> (N)+1
	end).

-define(DLOG_DEBUG,     7).
-define(DLOG_INFO,      6).
-define(DLOG_NOTICE,    5).
-define(DLOG_WARNING,   4).
-define(DLOG_ERROR,     3).
-define(DLOG_CRITICAL,  2).
-define(DLOG_ALERT,     1).
-define(DLOG_EMERGENCY, 0).
-define(DLOG_NONE,     -1).
	
%%====================================================================
%% API
%%====================================================================

ping() ->
    gen_server:call(?SERVER, ping).

debug(Level) when is_atom(Level) ->
    gen_server:call(?SERVER, {debug, level(Level)}).

register(Type, MFA={M,F,As}) when is_atom(M),is_atom(F),is_list(As) ->
    ets:insert(btreg, {Type,MFA}).

unregister(Type) ->
    ets:delete(btreg, Type).

devices() ->
    {ok,D1} = recent_devices(),
    {ok,D2} = paired_devices(),
    {ok,D3} = favorite_devices(),
    {ok,ordsets:to_list(ordsets:union([ordsets:from_list(D1),
				       ordsets:from_list(D2),
				       ordsets:from_list(D3)]))}.

recent_devices() ->
    gen_server:call(?SERVER, recent_devices).

paired_devices() ->
    gen_server:call(?SERVER, paired_devices).

favorite_devices() ->
    gen_server:call(?SERVER, favorite_devices).

device_info(Address, Info) ->
    {ok,Addr} = bt_util:getaddr(Address),
    case gen_server:call(?SERVER, {device_info, Addr, Info}) of
	{ok,InfoReply} ->
	    {ok, map2(fun(A,B) -> {A,B} end, Info, InfoReply)};
	Error ->
	    Error
    end.

local_info(Info) ->
    case gen_server:call(?SERVER, {local_info, Info}) of
	{ok,InfoReply} ->
	    {ok, map2(fun(A,B) -> {A,B} end, Info, InfoReply)};
	Error ->
	    Error
    end.
    

service_info(Address) ->
    service_info(Address,<<>>).

service_info(Address, UUID) ->
    case bt_util:getaddr(Address) of
	{ok,Addr} ->
	    gen_server:call(?SERVER, {service_info, Addr, UUID});
	Error -> Error
    end.

service_query(Address) ->
    service_query(Address, << >>).

service_query(Address, UUID) ->
    case bt_util:getaddr(Address) of
	{ok,Addr} ->
	    gen_server:call(?SERVER, {service_query, Addr, UUID}, 20000);
	Error -> Error
    end.

%% add a "bound" service  
service_add(ServiceRecord) when is_list(ServiceRecord) ->
    Service = lists:map(fun({ID,Value}) -> bt_sdp:encode(ID,Value) end,
			ServiceRecord),
    gen_server:call(?SERVER, {service_add,self(),Service}, 10000).

%% add persistent service (i.e no owner)
service_add_persist(ServiceRecord) when is_list(ServiceRecord) ->
    Service = lists:map(fun({ID,Value}) -> bt_sdp:encode(ID,Value) end,
			ServiceRecord),
    gen_server:call(?SERVER, {service_add,undefined,Service}, 10000).

service_del(Handle) when is_integer(Handle) ->
    gen_server:call(?SERVER, {service_del, Handle}, 10000).

service_rfcomm(Handle) when is_integer(Handle) ->
    gen_server:call(?SERVER, {service_rfcomm, Handle}, 10000). 
    

connect(Address) ->
    {ok,Addr} = bt_util:getaddr(Address),
    gen_server:call(?SERVER, {connect, Addr}, 20000).

connect(Address,Timeout) ->
    {ok,Addr} = bt_util:getaddr(Address),
    gen_server:call(?SERVER, {connect, Addr, Timeout, false}, 20000).

connect(Address,Timeout,Auth) ->
    {ok,Addr} = bt_util:getaddr(Address),
    gen_server:call(?SERVER, {connect, Addr, Timeout, Auth}, 20000).

disconnect(Address) ->
    {ok,Addr} = bt_util:getaddr(Address),
    gen_server:call(?SERVER, {disconnect, Addr}, 10000).

remote_name(Address) ->
    remote_name(Address, 0).

remote_name(Address, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    gen_server:call(?SERVER, {remote_name, Address, Timeout}, 20000).

%%
%% start bluetooth scan return {ok, Ref} or {error, Reason}
%% events sent to caller are:
%%   {bt, Ref, started}
%%   {bt, Ref, {device, Address}}
%%   {bt, Ref, stopped}
%% 
inquiry_start(Secs) ->
    gen_server:call(?SERVER, {inquiry_start,self(),Secs}).

%%
%% Stop bluetooth scanning (early)  possibly
%%
inquiry_stop(Ref) ->
    Res = gen_server:call(?SERVER, {inquiry_stop,Ref}),
    inquiry_flush(Ref),
    Res.

%% remove (only) stopped messages!
inquiry_flush(Ref) ->
    receive
	{bt,Ref,stopped} ->
	    ok
    after 0 ->
	    ok
    end.

rfcomm_open(Address, Channel) ->
    {ok,Addr} = bt_util:getaddr(Address),
    gen_server:call(?SERVER, {rfcomm_open, self(), Addr, Channel}, 20000).

rfcomm_close(Ref) ->
    gen_server:call(?SERVER, {rfcomm_close, Ref}).

rfcomm_send(Ref, Data) ->    
    gen_server:call(?SERVER, {rfcomm_send, Ref, Data}).

rfcomm_listen(Channel) ->
    gen_server:call(?SERVER, {rfcomm_listen,self(),Channel}).

rfcomm_accept(ListenRef) ->
    rfcomm_accept(ListenRef, infinity).

rfcomm_accept(ListenRef, Timeout) ->
    rfcomm_accept(ListenRef, Timeout, self()).

rfcomm_accept(ListenRef, Timeout, CtlPid) ->
    gen_server:call(?SERVER, {rfcomm_accept, CtlPid, ListenRef, Timeout}).

rfcomm_mtu(Ref) ->
    gen_server:call(?SERVER, {rfcomm_mtu, Ref}).    

rfcomm_channel(Ref) ->
    gen_server:call(?SERVER, {rfcomm_channel, Ref}).

rfcomm_address(Ref) ->
    gen_server:call(?SERVER, {rfcomm_address, Ref}).

%% L2CAP
l2cap_open(Address, Psm) ->
    {ok,Addr} = bt_util:getaddr(Address),
    gen_server:call(?SERVER, {l2cap_open, self(), Addr, Psm}, 20000).

l2cap_close(Ref) ->
    gen_server:call(?SERVER, {l2cap_close, Ref}).

l2cap_send(Ref, Data) ->    
    gen_server:call(?SERVER, {l2cap_send, Ref, Data}).

l2cap_listen(Psm) ->
    gen_server:call(?SERVER, {l2cap_listen,self(),Psm}).

l2cap_accept(ListenRef) ->
    l2cap_accept(ListenRef, infinity).

l2cap_accept(ListenRef, Timeout) ->
    gen_server:call(?SERVER, {l2cap_accept,self(),ListenRef,Timeout}).

l2cap_mtu(Ref) ->
    gen_server:call(?SERVER, {l2cap_mtu, Ref}).    

l2cap_psm(Ref) ->
    gen_server:call(?SERVER, {l2cap_psm, Ref}).

l2cap_address(Ref) ->
    gen_server:call(?SERVER, {l2cap_address, Ref}).


%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

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
init([]) ->
    PD = case code:priv_dir(bt) of
	     { error, bad_name } -> "./priv/";
	     Res -> Res
	 end,
	     
    Driver = filename:join([PD,"bt"]),
    Port = open_port({spawn, Driver}, [{packet,4},binary,eof]),
    Reg = ets:new(btreg, [public, set, named_table]),
    {ok, #state{ bt_port = Port, reg = Reg }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(ping, From, State) ->
    CmdId = State#state.cmd_id,
    State1 = bt_command(From, State, ?CMD_PING,  CmdId, []),
    {noreply, State1};
handle_call({debug,Level}, From, State) ->
    CmdId = State#state.cmd_id,
    State1 = bt_command(From, State, ?CMD_DEBUG,  CmdId, <<Level:32/signed>>),
    {noreply, State1};
handle_call(recent_devices, From, State) ->
    CmdId = State#state.cmd_id,
    State1 = bt_command(From, State, ?CMD_RECENT_DEVICES,  CmdId, []),
    {noreply, State1};
handle_call(paired_devices, From, State) ->
    CmdId = State#state.cmd_id,
    State1 = bt_command(From, State, ?CMD_PAIRED_DEVICES,  CmdId, []),
    {noreply, State1};
handle_call(favorite_devices, From, State) ->
    CmdId = State#state.cmd_id,
    State1 = bt_command(From, State, ?CMD_FAVORITE_DEVICES,  CmdId, []),
    {noreply, State1};
handle_call({device_info,Address,Info}, From, State) ->
    case is_address(Address) of
	true ->
	    AddrArg = tuple_to_list(Address),
	    case catch encode_info(Info) of
		{'EXIT', _} ->
		    {reply, {error, einval}, State};
		InfoArg ->
		    CmdId = State#state.cmd_id,
		    State1 = bt_command(From, State, ?CMD_DEVICE_INFO,  CmdId, 
					[AddrArg, InfoArg]),
		    {noreply, State1}
	    end;
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({local_info,Info}, From, State) ->
    case catch encode_linfo(Info) of
	{'EXIT', _} ->
	    {reply, {error, einval}, State};
	InfoArg ->
	    CmdId = State#state.cmd_id,
	    State1 = bt_command(From, State, ?CMD_LOCAL_INFO,  CmdId, 
				[InfoArg]),
	    {noreply, State1}
    end;
handle_call({service_info,Address,UUID}, From, State) ->
    case is_address(Address) andalso is_binary(UUID) of
	true ->
	    AddrArg = tuple_to_list(Address),
	    CmdId = State#state.cmd_id,
	    State1 = bt_command(From, State, ?CMD_SERVICE_INFO,  CmdId, 
				[AddrArg, UUID]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({service_query,Address,UUID}, From, State) ->
    case is_address(Address) andalso is_binary(UUID) of
	true ->
	    AddrArg = tuple_to_list(Address),
	    CmdId = State#state.cmd_id,
	    State1 = bt_command(From, State, ?CMD_SERVICE_QUERY,  CmdId, 
				[AddrArg, UUID]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;

handle_call({service_add,Owner,Service}, From, State) ->
    case lists:all(fun(B) -> is_binary(B) end, Service) of
	true ->
	    CmdId = State#state.cmd_id,
	    EvtId = State#state.evt_id,
	    Args = [<< EvtId:32 >>,encode(Service)],
	    EvtRef = make_ref(),
	    Mon = if is_pid(Owner) ->erlang:monitor(process, Owner);
		     true -> undefined
		  end,
	    SList = [#subscription { ref=EvtRef,
				     id=EvtId,
				     subscriber=Owner,
				     monitor=Mon,
				     tag  = sdp,
				     data = sdp
				    } | State#state.subscription],
	    State1 = bt_command(From, 
				State#state { evt_id = ?NEXT_ID(EvtId),
					      subscription = SList
					     },
				?CMD_SERVICE_ADD, CmdId, Args,
				{reply, EvtRef}),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({service_del, Handle}, From, State) ->
    if is_integer(Handle) ->
	    case lists:keysearch(Handle, #subscription.data,State#state.subscription) of
		{value, S} when S#subscription.tag == sdp ->
		    SList = State#state.subscription -- [S],
		    unmon(S#subscription.monitor),
		    CmdId = State#state.cmd_id,
		    State1 = bt_command(From,
					State#state { subscription = SList },
					?CMD_SERVICE_DEL, CmdId,
					<<(S#subscription.id):32>>),
		    {noreply, State1};
		false ->
		    {reply, {error, einval}, State}
	    end;
       true ->
	    {reply, {error, einval}, State}
    end;
handle_call({service_rfcomm, Handle}, From, State) ->
    if is_integer(Handle) ->
	    case lists:keysearch(Handle, #subscription.data,State#state.subscription) of
		{value, S} when S#subscription.tag == sdp ->
		    CmdId = State#state.cmd_id,
		    State1 = bt_command(From,State,?CMD_SERVICE_RFCOMM,CmdId,[<<(S#subscription.id):32>>]),
		    {noreply, State1};
		_ ->
		    {reply, {error, einval}, State}
	    end;
       true ->
	    {reply, {error, einval}, State}
    end;
handle_call({inquiry_start,Pid,Timeout},From,State) ->
    case is_pid(Pid) andalso is_timeout(Timeout) of 
	true ->
	    CmdId = State#state.cmd_id,
	    EvtId = State#state.evt_id,
	    EvtRef = make_ref(),
	    Mon = erlang:monitor(process, Pid),
	    SList = [#subscription { ref=EvtRef, 
				     id=EvtId, 
				     subscriber=Pid,
				     monitor=Mon,
				     tag = bt,
				     data = inquiry
				    } | State#state.subscription],
	    State1 = bt_command(From, 
				State#state { evt_id = ?NEXT_ID(EvtId),
					      subscription = SList
					     },
				?CMD_INQUIRY_START, CmdId,
				<<EvtId:32, Timeout:32>>,
				{reply, EvtRef}),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({inquiry_stop,Ref}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of
	false ->
	    {reply, {error, einval}, State};
	{value,S} ->
	    unmon(S#subscription.monitor),
	    SList = State#state.subscription -- [S],
	    State1 = bt_command(From, 
				State#state { subscription = SList },
				?CMD_INQUIRY_STOP, 
				State#state.cmd_id,
				<<(S#subscription.id):32>>),
	    {noreply, State1}
    end;
handle_call({remote_name,Address,Timeout},From,State) ->
    case is_address(Address) andalso
	is_timeout(Timeout) of 
	true ->
	    AddrArg = tuple_to_list(Address),
	    CmdId = State#state.cmd_id,
	    State1 = bt_command(From, State, ?CMD_REMOTE_NAME,  CmdId, 
				[AddrArg, <<Timeout:32>>]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;    
handle_call({connect, Address},From,State) ->
    case is_address(Address) of
	true ->
	    AddrArg = tuple_to_list(Address),
	    CmdId = State#state.cmd_id,
	    State1 = bt_command(From, State, ?CMD_CONNECT, CmdId, [AddrArg]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({connect, Address, Timeout, Auth},From,State) ->
    case is_address(Address) andalso
	is_timeout(Timeout) andalso 
	(Auth == true orelse Auth == false) of
	true ->
	    AddrArg = tuple_to_list(Address),
	    AuthArg = if Auth == true -> 1; Auth == false -> 0 end,
	    CmdId = State#state.cmd_id,
	    State1 = bt_command(From, State, ?CMD_CONNECT, CmdId, 
				[AddrArg, <<Timeout:32, AuthArg:8>>]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({disconnect, Address},From,State) ->
    case is_address(Address) of
	true ->
	    AddrArg = tuple_to_list(Address),
	    CmdId = State#state.cmd_id,
	    State1 = bt_command(From, State, ?CMD_DISCONNECT, CmdId, [AddrArg]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
%% RFCOMM
handle_call({rfcomm_open, Owner, Address, Channel}, From, State) ->
    case is_pid(Owner) andalso 
	is_address(Address) andalso
	is_channel(Channel) of
	true ->
	    CmdId = State#state.cmd_id,
	    EvtId = State#state.evt_id,
	    AddrArg = tuple_to_list(Address),
	    Args = [<< EvtId:32 >>,AddrArg,<<Channel:8>>],
	    EvtRef = make_ref(),
	    Mon = erlang:monitor(process, Owner),
	    SList = [#subscription { ref=EvtRef, 
				     id=EvtId, 
				     subscriber=Owner,
				     monitor=Mon,
				     tag = rfcomm,
				     data = rfcomm
				    } | State#state.subscription],
	    State1 = bt_command(From, 
				State#state { evt_id = ?NEXT_ID(EvtId),
					      subscription = SList
					     },
				?CMD_RFCOMM_OPEN, CmdId, Args,
				{reply, EvtRef}),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;

handle_call({rfcomm_listen, Owner, Channel}, From, State) ->
    case is_pid(Owner) andalso 
	(is_channel(Channel) orelse (Channel==0)) of
	true ->
	    CmdId = State#state.cmd_id,
	    EvtId = State#state.evt_id,
	    Args = [<< EvtId:32 >>,<<Channel:8>>],
	    EvtRef = make_ref(),
	    Mon = erlang:monitor(process, Owner),
	    SList = [#subscription { ref=EvtRef, 
				     id=EvtId, 
				     subscriber=Owner,
				     monitor=Mon,
				     tag = rfcomm_listen,
				     data = rfcomm_listen
				    } | State#state.subscription],
	    State1 = bt_command(From, 
				State#state { evt_id = ?NEXT_ID(EvtId),
					      subscription = SList
					     },
				?CMD_RFCOMM_LISTEN, CmdId, Args,
				{reply, EvtRef}),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;

handle_call({rfcomm_accept,Owner,Ref,_Timeout}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of
	{value,S} when S#subscription.tag == rfcomm_listen ->
	    CmdId = State#state.cmd_id,
	    EvtId = State#state.evt_id,
	    Args = [<<EvtId:32, (S#subscription.id):32 >>],
	    EvtRef = make_ref(),
	    Mon = erlang:monitor(process, Owner),
	    SList = [#subscription { ref=EvtRef, 
				     id=EvtId, 
				     subscriber=Owner,
				     monitor=Mon,
				     tag = rfcomm,
				     data = rfcomm
				    } | State#state.subscription],
	    State1 = bt_command(From, 
				State#state { evt_id = ?NEXT_ID(EvtId),
					      subscription = SList
					     },
				?CMD_RFCOMM_ACCEPT, CmdId, Args,
				{reply, EvtRef}),
	    {noreply, State1};
	_ ->
	    {reply, {error, einval}, State}
    end;

handle_call({rfcomm_close, Ref}, From, State) ->	    
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of    
	{value,S} when S#subscription.tag == rfcomm; S#subscription.tag == rfcomm_listen ->
	    unmon(S#subscription.monitor),
	    SList = State#state.subscription -- [S],
	    State1 = bt_command(From, 
				State#state { subscription = SList },
				?CMD_RFCOMM_CLOSE,
				State#state.cmd_id,
				<<(S#subscription.id):32>>),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({rfcomm_send, Ref, Data}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of    
	{value,S} when S#subscription.tag == rfcomm ->
	    State1 = bt_command(From, 
				State,
				?CMD_RFCOMM_SEND,
				State#state.cmd_id,
				[ <<(S#subscription.id):32 >> , Data]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({rfcomm_channel, Ref}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of    
	{value,S} when S#subscription.tag == rfcomm ->
	    State1 = bt_command(From, 
				State,
				?CMD_RFCOMM_CHANNEL,
				State#state.cmd_id,
				[ <<(S#subscription.id):32 >>]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({rfcomm_mtu, Ref}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of    
	{value,S} when S#subscription.tag == rfcomm ->
	    State1 = bt_command(From, 
				State,
				?CMD_RFCOMM_MTU,
				State#state.cmd_id,
				[ <<(S#subscription.id):32 >>]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({rfcomm_address, Ref}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of    
	{value,S} when S#subscription.tag == rfcomm ->
	    State1 = bt_command(From, 
				State,
				?CMD_RFCOMM_ADDRESS,
				State#state.cmd_id,
				[ <<(S#subscription.id):32 >>]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
%% L2CAP
handle_call({l2cap_open, Owner, Address, Psm}, From, State) ->
    case is_pid(Owner) andalso 
	is_address(Address) andalso
	is_psm(Psm) of
	true ->
	    CmdId = State#state.cmd_id,
	    EvtId = State#state.evt_id,
	    AddrArg = tuple_to_list(Address),
	    Args = [<< EvtId:32 >>,AddrArg,<<Psm:16>>],
	    EvtRef = make_ref(),
	    Mon = erlang:monitor(process, Owner),
	    SList = [#subscription { ref=EvtRef,
				     id=EvtId,
				     subscriber=Owner,
				     monitor=Mon,
				     tag = l2cap,
				     data = l2cap
				    } | State#state.subscription],
	    State1 = bt_command(From, 
				State#state { evt_id = ?NEXT_ID(EvtId),
					      subscription = SList
					     },
				?CMD_L2CAP_OPEN, CmdId, Args,
				{reply, EvtRef}),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;

handle_call({l2cap_listen, Owner, Psm}, From, State) ->
    case is_pid(Owner) andalso 
	(is_psm(Psm) orelse (Psm=:=0)) of
	true ->
	    CmdId = State#state.cmd_id,
	    EvtId = State#state.evt_id,
	    Args = [<< EvtId:32 >>,<<Psm:16>>],
	    EvtRef = make_ref(),
	    Mon = erlang:monitor(process, Owner),
	    SList = [#subscription { ref=EvtRef,
				     id=EvtId,
				     subscriber=Owner,
				     monitor=Mon,
				     tag = l2cap_listen,
				     data = l2cap_listen
				    } | State#state.subscription],
	    State1 = bt_command(From,
				State#state { evt_id = ?NEXT_ID(EvtId),
					      subscription = SList
					     },
				?CMD_L2CAP_LISTEN, CmdId, Args,
				{reply, EvtRef}),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;

handle_call({l2cap_accept,Owner,Ref,_Timeout}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of
	{value,S} when S#subscription.tag == l2cap_listen ->
	    CmdId = State#state.cmd_id,
	    EvtId = State#state.evt_id,
	    Args = [<<EvtId:32, (S#subscription.id):32 >>],
	    EvtRef = make_ref(),
	    Mon = erlang:monitor(process, Owner),
	    SList = [#subscription { ref=EvtRef,
				     id=EvtId,
				     subscriber=Owner,
				     monitor=Mon,
				     tag = l2cap,
				     data = l2cap
				    } | State#state.subscription],
	    State1 = bt_command(From, 
				State#state { evt_id = ?NEXT_ID(EvtId),
					      subscription = SList
					     },
				?CMD_L2CAP_ACCEPT, CmdId, Args,
				{reply, EvtRef}),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;

handle_call({l2cap_close, Ref}, From, State) ->	    
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of    
	{value,S} when S#subscription.tag == l2cap; S#subscription.tag == l2cap_listen ->
	    unmon(S#subscription.monitor),
	    SList = State#state.subscription -- [S],
	    State1 = bt_command(From, 
				State#state { subscription = SList },
				?CMD_L2CAP_CLOSE,
				State#state.cmd_id,
				<<(S#subscription.id):32>>),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({l2cap_send, Ref, Data}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of    
	{value,S} when S#subscription.tag == l2cap ->
	    State1 = bt_command(From,
				State,
				?CMD_L2CAP_SEND,
				State#state.cmd_id,
				[ <<(S#subscription.id):32 >> , Data]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({l2cap_psm, Ref}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of    
	{value,S} when S#subscription.tag == l2cap ->
	    State1 = bt_command(From, 
				State,
				?CMD_L2CAP_PSM,
				State#state.cmd_id,
				[ <<(S#subscription.id):32 >>]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({l2cap_mtu, Ref}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of    
	{value,S} when S#subscription.tag == l2cap ->
	    State1 = bt_command(From,
				State,
				?CMD_L2CAP_MTU,
				State#state.cmd_id,
				[ <<(S#subscription.id):32 >>]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
handle_call({l2cap_address, Ref}, From, State) ->
    case lists:keysearch(Ref,#subscription.ref,State#state.subscription) of    
	{value,S} when S#subscription.tag == l2cap ->
	    State1 = bt_command(From, 
				State,
				?CMD_L2CAP_ADDRESS,
				State#state.cmd_id,
				[ <<(S#subscription.id):32 >>]),
	    {noreply, State1};
	false ->
	    {reply, {error, einval}, State}
    end;
%% 
handle_call(stop, _From, State) ->
    erlang:port_close(State#state.bt_port),
    {stop, normal, ok, State};    
handle_call(_Request, _From, State) ->
    {reply, {error, bad_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Port,{data,Data}},State) when Port == State#state.bt_port ->
    ?dbg("Port: data=~p", [Data]),
    case Data of
	<<?REPLY_OK, CmdId:32, ReplyData/binary>> ->
	    ?dbg("Got: OK cmdid=~w, data=~p", [CmdId,ReplyData]),
	    State1 = case decode(ReplyData) of
			 false ->
			     bt_reply(CmdId, ok, State);
			 {value,Decoded} ->
			     bt_reply(CmdId, {ok,Decoded}, State)
		     end,
	    {noreply, State1};
	<<?REPLY_ERROR, CmdId:32, ReplyData/binary>> ->
	    ?dbg("Got: ERROR cmdid=~w, data=~p", [CmdId,ReplyData]),
	    State1 = case decode(ReplyData) of
			 false ->
			     bt_reply(CmdId, error, State);
			 {value,Decoded} ->
			     bt_reply(CmdId, {error,Decoded}, State)
		     end,			 
	    {noreply, State1};
	<<?REPLY_EVENT, EvtId:32, EventData/binary>> ->
	    ?dbg("Got: EVENT evtid=~w, data=~p", [EvtId,EventData]),
	    case lists:keysearch(EvtId,#subscription.id, 
				 State#state.subscription) of
		false ->
		    ?dbg("no receipient for evtid=~p data=~p", 
			 [EvtId, decode(EventData)]),
		    {noreply, State};
		{value,S} ->
		    case decode(EventData) of
			false ->
			    ?dbg("bad event data ~p", [EventData]),
			    {noreply, State};
			{value, Decoded} ->
			    S#subscription.subscriber !
				{S#subscription.tag,S#subscription.ref,Decoded},
			    if Decoded == closed ->
				    unmon(S#subscription.monitor),
				    SList = State#state.subscription -- [S],
				    State1 = State#state { subscription = SList },
				    {noreply, State1};
			       true ->
				    {noreply, State}
			    end
		    end
	    end;
	_Other ->
	    ?dbg("Unexpected data=~999p", [_Other]),
    	    {noreply, State}
    end;
handle_info({Port,eof}, State) when Port == State#state.bt_port ->
    ?dbg("bt_drv closed",[]),
    erlang:port_close(Port),
    {stop, closed, State};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    case lists:keysearch(Ref, #subscription.monitor,State#state.subscription) of
	false ->
	    ?dbg("handle_info: Down process ~p not subsribing\n", [_Pid]),
	    {noreply, State};
	{value, S} ->
	    SList = State#state.subscription -- [S],
	    State1 = 
		if S#subscription.data == inquiry ->
			bt_command(undefined,
				   State#state { subscription = SList },
				   ?CMD_INQUIRY_STOP, 0,
				   <<(S#subscription.id):32>>);
		   S#subscription.data == rfcomm ->
			bt_command(undefined,
				   State#state { subscription = SList },
				   ?CMD_RFCOMM_CLOSE, 0,
				   <<(S#subscription.id):32>>);
		   S#subscription.data == rfcomm_listen ->
			bt_command(undefined,
				   State#state { subscription = SList },
				   ?CMD_RFCOMM_CLOSE, 0,
				   <<(S#subscription.id):32>>);
		   S#subscription.data == l2cap ->
			bt_command(undefined,
				   State#state { subscription = SList },
				   ?CMD_L2CAP_CLOSE, 0,
				   <<(S#subscription.id):32>>);
		   S#subscription.data == l2cap_listen ->
			bt_command(undefined,
				   State#state { subscription = SList },
				   ?CMD_L2CAP_CLOSE, 0,
				   <<(S#subscription.id):32>>);
		   true ->
			State#state { subscription = SList }
		end,
	    {noreply, State1}
    end;
handle_info(Info, State) ->
    io:format("handle_info: got ~p\n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
bt_command(From, State, OpCode, CmdId, Data) ->
    bt_command(From, State, OpCode, CmdId, Data, reply).
    
bt_command(From, State, OpCode, CmdId, Data, ReplyData) ->
    ?dbg("Command: ~w ~w ~w", [OpCode, CmdId, Data]),
    erlang:port_command(State#state.bt_port, [OpCode, <<CmdId:32>>, Data]),
    if CmdId == 0 -> %% no reply expected
	    State;
       true ->
	    Wait = [#wait { from=From, id=CmdId, data=ReplyData} | 
		    State#state.wait],
	    State#state { cmd_id = ?NEXT_ID(CmdId), wait = Wait }
    end.

bt_reply(0, _Reply, State) -> %% global reply
    ?dbg("bt_drv: error:  ~p", [_Reply]),
    %% maybe send this error to all, and terminate ?
    State;
bt_reply(CmdId, Reply, State) ->
    case lists:keysearch(CmdId, #wait.id, State#state.wait) of
	{value,W = #wait { data = reply }} ->
	    gen_server:reply(W#wait.from, Reply),
	    Wait = State#state.wait -- [W],
	    State#state { wait = Wait };

	{value,W = #wait { data={reply,EvtRef} }} ->
	    case Reply of
		ok ->
		    gen_server:reply(W#wait.from, {ok,EvtRef}),
		    Wait = State#state.wait -- [W],
		    State#state { wait = Wait };
		Other ->
		    ?dbg("other reply: ~p", [Other]),
		    Wait = State#state.wait -- [W],
		    case lists:keysearch(EvtRef, #subscription.ref,
					 State#state.subscription) of
			false ->
			    gen_server:reply(W#wait.from, Other),
			    State#state { wait = Wait };
			{value,S} when S#subscription.data == sdp ->
			    %% special case
			    case Other of 
				{ok,Handle} ->
				    S1 = S#subscription { data = Handle },  %% switch to handle
				    gen_server:reply(W#wait.from, Other),
				    SList = lists:keyreplace(EvtRef, #subscription.ref, 
							     State#state.subscription, S1),
				    State#state { wait = Wait, subscription = SList };
				_  ->
				    gen_server:reply(W#wait.from, Other),
				    unmon(S#subscription.monitor),
				    SList = State#state.subscription -- [S],
				    State#state { wait = Wait, subscription = SList }
			    end;
			{value,S} ->
			    gen_server:reply(W#wait.from, Other),
			    unmon(S#subscription.monitor),
			    SList = State#state.subscription -- [S],
			    State#state { wait = Wait, subscription = SList }
		    end
	    end;
	false ->
	    io:format("Did not find waiter for cmdid ~w\n", [CmdId]),
	    State
    end.

%% unmon - ignore if reference is undefined
unmon(undefined) -> ok;
unmon(Ref) -> erlang:demonitor(Ref, [flush]).

encode_info([What | Info]) ->
    case What of
	name  ->  [?NFO_DEVICE_NAME | encode_info(Info)];
	class ->  [?NFO_DEVICE_CLASS | encode_info(Info)];
	clock   -> [?NFO_DEVICE_CLOCK | encode_info(Info)];
	inquiry -> [?NFO_DEVICE_INQUIRY | encode_info(Info)];
	access  -> [?NFO_DEVICE_ACCESS | encode_info(Info)];
	update  -> [?NFO_DEVICE_UPDATE | encode_info(Info)];
	is_favorite  -> [?NFO_DEVICE_IS_FAVORITE | encode_info(Info)];
	is_paired -> [?NFO_DEVICE_IS_PAIRED | encode_info(Info)];
	is_connected -> [?NFO_DEVICE_IS_CONNECTED | encode_info(Info)]
    end;
encode_info([]) -> [].

encode_linfo([What | Info]) ->
    case What of
	name    ->  [?NFO_LOCAL_NAME | encode_linfo(Info)];
	class   ->  [?NFO_LOCAL_CLASS | encode_linfo(Info)];
	address ->  [?NFO_LOCAL_ADDRESS | encode_linfo(Info)];
	discoverable ->  [?NFO_LOCAL_DISCOVERABLE | encode_linfo(Info)];
	power_state ->  [?NFO_LOCAL_POWER_STATE | encode_linfo(Info)]
    end;
encode_linfo([]) -> [].


decode_class(Class) ->    
    <<Service:11, Major:5, Minor:6,_:2>> = <<Class:24>>,
    {Service,Major,Minor}.

%% encode into Data format
encode(Term) ->
    list_to_binary([enc(Term)]).

enc(true) ->   
    <<?BOOLEAN, 1>>;
enc(false) ->  
    <<?BOOLEAN, 0>>;
enc(X) when is_atom(X) ->
    Val = atom_to_list(X),
    [<<?ATOM, (length(Val)):8>>, Val];
enc(X) when is_integer(X) ->
    if X =< 16#ffffffff -> <<?UINT32, X:32>> ;
       X >= -16#8000000 -> <<?INT32, X:32>> ;
       X > 0 -> <<?UINT64, X:64>>;
       true -> (<<?INT64, X:64>>)
    end;
enc(X) when is_float(X) -> 
    <<?FLOAT64, X:64>>;
enc(X) when is_list(X) ->
    case is_string(X) of
	true ->
	    Len = length(X),
	    if Len =< 255 ->
		    [<<?STRING1, Len:8>>, X];
	       true ->
		    [<<?STRING4, Len:32>>, X]
	    end;
	false ->
	    [?LIST, lists:map(fun(E) -> enc(E) end, X), ?LIST_END]
    end;
enc(X) when is_tuple(X) ->
    [?TUPLE, lists:map(fun(E) -> enc(E) end, tuple_to_list(X)), ?TUPLE_END];
enc(X) when is_binary(X) ->
    [<<?BINARY,(size(X)):32>>, X].
     
is_string([X|Xs]) when X >= 0, X =< 255 -> is_string(Xs);
is_string([]) -> true;
is_string(_) -> false.


%% decode reply data
decode(<<>>) -> false;
decode(Data) -> {value, decode(Data, [])}.
    
decode(<<>>, [Hd]) -> Hd;
decode(Data, Stack) ->
    case Data of
	<<?LIST, Rest/binary>> -> 
	    ?dbg("LIST",[]),
	    decode(Rest, [list|Stack]);
	<<?TUPLE, Rest/binary>> ->
	    ?dbg("TUPLE",[]),
	    decode(Rest, [tuple|Stack]);
	<<?BOOLEAN, B, Rest/binary>> -> 
	    ?dbg("BOOLEAN:~w",[B]),
	    decode(Rest, [B =/= 0 | Stack]);
	<<?UINT8, I:8, Rest/binary>> -> 
	    ?dbg("UINT8:~w",[I]),
	    decode(Rest, [I|Stack]);
	<<?UINT16, I:16, Rest/binary>> ->
	    ?dbg("UINT16:~w",[I]),
	    decode(Rest, [I|Stack]);
	<<?UINT32, I:32, Rest/binary>> ->
	    ?dbg("UINT32:~w",[I]),
	    decode(Rest, [I|Stack]);
	<<?UINT64, I:64, Rest/binary>> ->
	    ?dbg("UINT64:~w",[I]),
	    decode(Rest, [I|Stack]);
	<<?INT8, I:8/signed, Rest/binary>> ->
	    ?dbg("INT8:~w",[I]),
	    decode(Rest, [I|Stack]);
	<<?INT16, I:16/signed, Rest/binary>> ->
	    ?dbg("INT16:~w",[I]),
	    decode(Rest, [I|Stack]);
	<<?INT32, I:32/signed, Rest/binary>> -> 
	    ?dbg("INT32:~w",[I]),
	    decode(Rest, [I|Stack]);
	<<?INT64, I:64/signed, Rest/binary>> ->
	    ?dbg("INT64:~w",[I]),
	    decode(Rest, [I|Stack]);
	<<?FLOAT32, F:32/float, Rest/binary>> ->
	    ?dbg("FLOAT32:~w",[F]),
	    decode(Rest, [F|Stack]);
	<<?FLOAT64, F:64/float, Rest/binary>> -> 
	    ?dbg("FLOAT64:~w",[F]),
	    decode(Rest, [F|Stack]);
	<<?STRING1, Len:8, String:Len/binary, Rest/binary>> -> 
	    ?dbg("STRING1: len=~w, ~w",[Len,String]),
	    decode(Rest, [binary_to_list(String)  | Stack]);
	<<?STRING4, Len:32, String:Len/binary, Rest/binary>> ->
	    ?dbg("STRING4: len=~w, ~w",[Len,String]),
	    decode(Rest, [binary_to_list(String)  | Stack]);
	<<?BINARY, Len:32, Bin:Len/binary, Rest/binary>> -> 
	    ?dbg("BINARY: len=~w, ~w",[Len,Bin]),
	    decode(Rest, [Bin | Stack]);
	<<?ATOM, Len:8, Atom:Len/binary, Rest/binary>> -> 
	    ?dbg("ATOM: len=~w, ~w",[Len,Atom]),
	    decode(Rest, [list_to_atom(binary_to_list(Atom)) | Stack]);
	<<?LIST_END, Rest/binary>> ->
	    ?dbg("LIST_END",[]),
	    {L,[_|Stack1]} = lists:splitwith(fun(X) -> X =/= list end, Stack),
	    decode(Rest, [lists:reverse(L) | Stack1]);
	<<?TUPLE_END, Rest/binary>> ->
	    ?dbg("TUPLE_END",[]),
	    {L,[_|Stack1]}=lists:splitwith(fun(X) -> X =/= tuple end, Stack),
	    decode(Rest, [list_to_tuple(lists:reverse(L)) | Stack1]);
	%% EXTENSIONS
	<<?DATE, Date:32, Rest/binary>> ->
	    ?dbg("DATE: ~w",[Date]),
	    DateTime = if Date == 0 -> {{0,0,0},{0,0,0}};
			  true ->
			       Now = {Date div 1000000, Date rem 1000000, 0},
			       calendar:now_to_datetime(Now)
		       end,
	    decode(Rest,[DateTime|Stack]);
	<<?ADDR, Addr:6/binary, Rest/binary>> -> 
	    ?dbg("ADDR: ~w",[Addr]),
	    decode(Rest, [list_to_tuple(binary_to_list(Addr)) | Stack])
    end.

map2(Fun, [A|As], [B|Bs]) ->
    [ Fun(A, B) | map2(Fun, As, Bs)];
map2(_Fun, [], []) ->
    [].

is_address(Addr) ->
    if ?is_bt_address(Addr) -> true;
       true -> false
    end.
	    
is_channel(C) when is_integer(C), C >= 1, C =< 30 ->
    true;
is_channel(_) -> false.

is_psm(C) when is_integer(C), C >= 0, C =< 16#ffff ->
    true;
is_psm(_) -> false.


is_timeout(T) when is_integer(T), T>=0 ->
    true;
is_timeout(_) -> false.

%% convert symbolic to numeric level
level(true)  -> ?DLOG_DEBUG;
level(false) -> ?DLOG_NONE;
level(debug) -> ?DLOG_DEBUG;
level(info)  -> ?DLOG_INFO;
level(notice) -> ?DLOG_NOTICE;
level(warning) -> ?DLOG_WARNING;
level(error) -> ?DLOG_ERROR;
level(critical) -> ?DLOG_CRITICAL;
level(alert) -> ?DLOG_ALERT;
level(emergency) -> ?DLOG_EMERGENCY;
level(none) -> ?DLOG_NONE.
