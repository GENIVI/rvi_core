%%% @author Tony Rogvall <tony@up13>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%     hci_drv
%%% @end
%%% Created :  5 Apr 2015 by Tony Rogvall <tony@up13>

-module(hci_drv).

-export([open/0]).
-export([bind/2]).
-export([close/1]).
-export([send/2]).
-export([activate/1]).
-export([deactivate/1]).
-export([activate/2]).
-export([debug/2]).
-export([dev_up/2]).
-export([dev_down/2]).
-export([dev_reset/2]).  %% down,up?
-export([dev_restat/2]). %% reset statistics
-export([get_dev_list/1]).
-export([get_dev_info/2]).
-export([get_conn_list/2]).
-export([get_conn_info/3]).
-export([get_auth_info/2]).
-export([set_raw/2]).
-export([set_auth/3]).
-export([set_encrypt/3]).
-export([set_ptype/3]).
-export([set_link_policy/3]).
-export([set_link_mode/3]).
-export([set_scan/3]).
-export([set_acl_mtu/4]).
-export([set_sco_mtu/4]).
-export([block/2]).
-export([unblock/2]).
-export([inquiry/5]).

-export([set_filter/2]).
-export([get_filter/1]).
-export([set_filter_ptype/2]).
-export([clr_filter_ptype/2]).
-export([set_filter_event/2]).
-export([clr_filter_event/2]).
-export([set_filter_opcode/2]).
-export([make_filter/3]).

-include("../include/hci_drv.hrl").
-include("hci_api.hrl").

%% deugging
-compile(export_all).

-define(CMD_ACTIVE,         1).
-define(CMD_DEBUG,          2).
-define(CMD_BIND,           3).
-define(CMD_GETFILTER,      4).
-define(CMD_SETFILTER,      5).

-define(CMD_HCIDEVUP,	   201).
-define(CMD_HCIDEVDOWN,	   202).
-define(CMD_HCIDEVRESET,	   203).
-define(CMD_HCIDEVRESTAT,   204).
-define(CMD_HCIGETDEVLIST,  210).
-define(CMD_HCIGETDEVINFO,  211).
-define(CMD_HCIGETCONNLIST, 212).
-define(CMD_HCIGETCONNINFO, 213).
-define(CMD_HCIGETAUTHINFO, 215).
-define(CMD_HCISETRAW,	   220).
-define(CMD_HCISETSCAN,	   221).
-define(CMD_HCISETAUTH,	   222).
-define(CMD_HCISETENCRYPT,  223).
-define(CMD_HCISETPTYPE,   224).
-define(CMD_HCISETLINKPOL,  225).
-define(CMD_HCISETLINKMODE, 226).
-define(CMD_HCISETACLMTU,   227).
-define(CMD_HCISETSCOMTU,   228).
-define(CMD_HCIBLOCKADDR,   230).
-define(CMD_HCIUNBLOCKADDR, 231).
-define(CMD_HCIINQUIRY,	   240).


-define(DLOG_DEBUG,     7).
-define(DLOG_INFO,      6).
-define(DLOG_NOTICE,    5).
-define(DLOG_WARNING,   4).
-define(DLOG_ERROR,     3).
-define(DLOG_CRITICAL,  2).
-define(DLOG_ALERT,     1).
-define(DLOG_EMERGENCY, 0).
-define(DLOG_NONE,     -1).


-spec open() -> hci_socket_t().
open() ->
    Driver = "hci_drv",
    Path = code:priv_dir(bt),
    %% io:format("load_driver '~s' from: '~s'\n", [Driver, Path]),
    case erl_ddll:load_driver(Path, Driver) of
	ok ->
	    erlang:open_port({spawn_driver, Driver}, [binary]);
	{error,Error} ->
	    io:format("Error: ~s\n", [erl_ddll:format_error_int(Error)]),
	    erlang:error(Error)
    end.

%% Close the HCI socket
-spec close(Hci::hci_socket_t()) -> boolean().
close(Hci) when is_port(Hci) ->
    erlang:port_close(Hci).

%% Bind HCI socket to device
-spec bind(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

bind(Hci, DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_BIND, <<DevID:32/signed>>).

%% Bring the device UP
-spec dev_up(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

dev_up(Hci,DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_HCIDEVUP, <<DevID:32/signed>>).

%% Bring the device DOWN
-spec dev_down(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

dev_down(Hci,DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_HCIDEVDOWN, <<DevID:32/signed>>).

%% Reset the device, maybe do down/up? as seen in library code elsewhere
-spec dev_reset(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

dev_reset(Hci,DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_HCIDEVRESET, <<DevID:32/signed>>).

%% Reset device statistics
-spec dev_restat(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

dev_restat(Hci,DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_HCIDEVRESTAT, <<DevID:32/signed>>).

%% Get list of [{device(),options()}] 
-spec get_dev_list(Hci::hci_socket_t()) ->
			  {ok,[{DevID::hci_devid_t(), Opt::integer()}]} |
			  {error,posix()}.
get_dev_list(Hci) when is_port(Hci) ->
    case port_call(Hci, ?CMD_HCIGETDEVLIST, <<>>) of
	{ok, DevList} ->
	    {ok,
	     [{DevID,DevOpt} || 
		 <<DevID:16, DevOpt:32>>
		     <= DevList  ]};
	Error ->
	    Error
    end.

%% get device info
-spec get_dev_info(Hci::hci_socket_t(), DevID::hci_devid_t()) ->
			  {ok,[#hci_dev_info{}]} | {error,posix()}.

get_dev_info(Hci, DevID) ->
    case port_call(Hci, ?CMD_HCIGETDEVINFO, <<DevID:32/signed>>) of
	{ok, Info} ->
	    {ok, decode_hci_dev_info(Info)};
	Error ->
	    Error
    end.

%% get device info
-spec get_conn_list(Hci::hci_socket_t(), DevID::hci_devid_t()) ->
			   {ok,[#hci_conn_info{}]} | {error,posix()}.
get_conn_list(Hci, DevID) ->
    case port_call(Hci, ?CMD_HCIGETCONNLIST, <<DevID:32/signed>>) of
	{ok, Data} ->
	    {ok, decode_hci_conn_list(Data)};
	Error ->
	    Error
    end.

-spec get_conn_info(Hci::hci_socket_t(), Addr::bdaddr_t(), Type::uint8_t()) ->
			   {ok,#hci_conn_info{}} | {error,posix()}.

get_conn_info(Hci, Addr, Type) when is_port(Hci) ->
    case bt:getaddr(Addr) of
	{ok,{A,B,C,D,E,F}} ->
	    case port_call(Hci, ?CMD_HCIGETCONNINFO, <<F,E,D,C,B,A,Type>>) of
		{ok, Info} ->
		    {ok, decode_hci_conn_info(Info)};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

-spec get_auth_info(Hci::hci_socket_t(), Addr::bdaddr_t()) ->
			   {ok, Type::uint8_t()} | {error,posix()}.

get_auth_info(Hci, Addr) when is_port(Hci) ->
    case bt:getaddr(Addr) of
	{ok,{A,B,C,D,E,F}} ->
	    port_call(Hci, ?CMD_HCIGETAUTHINFO, <<F,E,D,C,B,A>>);
	Error ->
	    Error
    end.

%% Set raw processing?
-spec set_raw(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

set_raw(Hci,DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_HCISETRAW, <<DevID:32/signed>>).

%% Enable/Disable authentication
-spec set_auth(Hci::hci_socket_t(), DevID::hci_devid_t(), DoAuth::boolean()) ->
		      ok | {error,posix()}.
set_auth(Hci,DevID,DoAuth) when is_port(Hci), is_integer(DevID),
				is_boolean(DoAuth) ->
    port_call(Hci, ?CMD_HCISETAUTH, <<DevID:32/signed,DoAuth:8>>).

%% Enable/Disable authentication
-spec set_encrypt(Hci::hci_socket_t(), DevID::hci_devid_t(), DoAuth::boolean()) ->
			 ok | {error,posix()}.
set_encrypt(Hci,DevID,DoEncrypt) when is_port(Hci), is_integer(DevID),
				      is_boolean(DoEncrypt) ->
    port_call(Hci, ?CMD_HCISETENCRYPT, <<DevID:32/signed,DoEncrypt:8>>).

%% Set device packet type
-spec set_ptype(Hci::hci_socket_t(),DevID::hci_devid_t(),PType::[string()]) ->
		       ok | {error,posix()}.
set_ptype(Hci,DevID,PType0) when is_port(Hci), is_integer(DevID),
				 is_list(PType0) ->
    PType = set_bits(PType0, hci_util:kv_pkt_ptype()),
    port_call(Hci, ?CMD_HCISETPTYPE, <<DevID:32/signed,PType:32>>).

-spec set_link_policy(Hci::hci_socket_t(),DevID::hci_devid_t(),Pol::string()) ->
			     ok | {error,posix()}.
set_link_policy(Hci,DevID,Pol0) when is_port(Hci), is_integer(DevID) ->
    LinkPolicy = hci_util:find_enum_value(Pol0, hci_util:kv_link_policy()),
    port_call(Hci, ?CMD_HCISETLINKPOL, <<DevID:32/signed,LinkPolicy:32>>).

-spec set_link_mode(Hci::hci_socket_t(),DevID::hci_devid_t(),Mode::string()) ->
			     ok | {error,posix()}.
set_link_mode(Hci,DevID,Mode0) when is_port(Hci), is_integer(DevID) ->
    LinkMode = hci_util:find_enum_value(Mode0, hci_util:kv_link_mode()),
    port_call(Hci, ?CMD_HCISETLINKMODE, <<DevID:32/signed,LinkMode:32>>).

-spec set_scan(Hci::hci_socket_t(),DevID::hci_devid_t(),Scan::string()) ->
		      ok | {error,posix()}.
set_scan(Hci,DevID,Scan0) when is_port(Hci) ->
    Scan = hci_util:find_enum_value(Scan0, hci_util:kv_scan()),
    port_call(Hci, ?CMD_HCISETSCAN, <<DevID:32/signed,Scan:32>>).

-spec set_acl_mtu(Hci::hci_socket_t(),DevID::hci_devid_t(),
		  Mtu::integer(),Mpkt::integer()) ->
			 ok | {error,posix()}.
set_acl_mtu(Hci,DevID,Mtu,Mpkt) when is_port(Hci) ->
    port_call(Hci, ?CMD_HCISETACLMTU, <<DevID:32/signed,Mtu:16,Mpkt:16>>).

-spec set_sco_mtu(Hci::hci_socket_t(),DevID::hci_devid_t(),
		  Mtu::integer(),Mpkt::integer()) ->
			 ok | {error,posix()}.
set_sco_mtu(Hci,DevID,Mtu,Mpkt) when is_port(Hci) ->
    port_call(Hci, ?CMD_HCISETSCOMTU, <<DevID:32/signed,Mtu:16,Mpkt:16>>).

%% The Hci socket must be bound before this operation
-spec block(Hci::hci_socket_t(), Addr::bdaddr_t()) ->
		   ok | {error,posix()}.
block(Hci, Addr) when is_port(Hci) ->
    case bt:getaddr(Addr) of
	{ok,{A,B,C,D,E,F}} ->
	    port_call(Hci, ?CMD_HCIBLOCKADDR, <<F,E,D,C,B,A>>);
	Error ->
	    Error
    end.

%% The Hci socket must be bound before this operation
-spec unblock(Hci::hci_socket_t(), Addr:: all | bdaddr_t()) ->
		     ok | {error,posix()}.
unblock(Hci, all) when is_port(Hci) ->
    unblock(Hci, {0,0,0,0,0,0});
unblock(Hci,Addr) when is_port(Hci) ->
    case bt:getaddr(Addr) of
	{ok, {A,B,C,D,E,F}} ->
	    port_call(Hci, ?CMD_HCIUNBLOCKADDR, <<F,E,D,C,B,A>>);
	Error ->
	    Error
    end.

%% Timeout is in millisecs but is converted into 1.28s units
%% 1 len = 1.28s = 1280 ms
inquiry(Hci, Timeout, NumRsp, Lap0, Flags) ->
    Len = (Timeout div 1280) + (if Timeout rem 1280 =:= 0 -> 0; true -> 1 end),
    Lap = if Lap0 =:= 0; Lap0 =:= <<>> -> <<16#33,16#8b,16#9e>>;
	      byte_size(Lap0) =:= 3 -> Lap0
	  end,
    Args = <<Len, NumRsp, Lap/binary, Flags:32>>,
    case port_call(Hci, ?CMD_HCIINQUIRY, Args) of
	{ok, Bin} ->
	    {ok, [hci_api:decode_inquiry_info(Info) ||
		     <<Info:?INQUIRY_INFO_SIZE/binary>> <= Bin]};
	Error ->
	    Error
    end.
    

-spec deactivate(Hci::hci_socket_t()) -> ok | {error,posix()}.

deactivate(Hci) when is_port(Hci) ->
    activate(Hci, 0).    

-spec activate(Hci::hci_socket_t()) -> ok | {error,posix()}.
activate(Hci) when is_port(Hci) ->
    activate(Hci, -1).

-spec activate(Hci::hci_socket_t(),N::integer()) -> ok | {error,posix()}.
activate(Hci, N) when is_port(Hci),
		      is_integer(N), N >= -1, N < 16#7fffffff ->
    port_call(Hci, ?CMD_ACTIVE, <<N:32>>).

-spec debug(Hci::hci_socket_t(), Level::level_t()) -> ok.
debug(Hci,Level) when is_port(Hci), is_atom(Level) ->
    L = level(Level),
    port_call(Hci, ?CMD_DEBUG, <<L:32>>).

set_filter_ptype(T, F = #hci_filter { type_mask = M}) ->
    Bit = if T =:= ?HCI_VENDOR_PKT -> 1;
	      true ->  1 bsl (T band ?HCI_FLT_TYPE_BITS)
	   end,
    F#hci_filter { type_mask = M bor Bit }.

clr_filter_ptype(T, F = #hci_filter { type_mask = M}) ->
    Bit = if T =:= ?HCI_VENDOR_PKT -> 1;
	      true ->  1 bsl (T band ?HCI_FLT_TYPE_BITS)
	   end,
    F#hci_filter { type_mask = M band (bnot Bit) }.

set_filter_event(E, F = #hci_filter { event_mask = M}) ->
    Bit = 1 bsl (E band ?HCI_FLT_EVENT_BITS),
    F#hci_filter { event_mask = M bor Bit }.

clr_filter_event(E, F = #hci_filter { event_mask = M}) ->
    Bit = 1 bsl (E band ?HCI_FLT_EVENT_BITS),
    F#hci_filter { event_mask = M band (bnot Bit) }.

set_filter_opcode(Opcode,  F = #hci_filter { }) ->
    F#hci_filter { opcode = Opcode }.

make_filter(Opcode, Ts, Es) when is_integer(Opcode),
				 is_list(Ts),
				 is_list(Es) ->
    
    Type_mask = make_bits(Ts, 0) band 16#ffffffff,
    Event_mask = make_bits(Es, 0) band 16#ffffffffffffffff,
    #hci_filter { type_mask = Type_mask,
		  event_mask = Event_mask,
		  opcode = Opcode }.

make_bits([255|Ns], Bits) ->  %% ?HCI_VENDOR_PKT! -> 1
    make_bits(Ns, Bits bor 1);
make_bits([-1|_], _Bits) -> 16#ffffffffffffffff;
make_bits([Nr|Ns], Bits) when is_integer(Nr), Nr >= 0 ->
    make_bits(Ns, Bits bor (1 bsl Nr));
make_bits([], Bits) ->
    Bits.


-spec set_filter(Hci::hci_socket_t(), Filter::#hci_filter{}) ->
			ok | {error,posix()}.
set_filter(Hci,Filter) when is_port(Hci), is_record(Filter, hci_filter) ->
    port_call(Hci, ?CMD_SETFILTER, encode_hci_filter(Filter)).

-spec get_filter(Hci::hci_socket_t()) ->
			{ok, Filter::#hci_filter{}} | {error,posix()}.

get_filter(Hci) when is_port(Hci) ->
    case port_call(Hci, ?CMD_GETFILTER, <<>>) of
	{ok, Data} -> {ok, decode_hci_filter(Data)};
	Error -> Error
    end.

-spec send(Hci::hci_socket_t(), Command::iolist()) ->
		  boolean().
send(Hci, Command) ->
    erlang:port_command(Hci, Command).

port_call(Hci, Cmd, Data) ->
    case erlang:port_control(Hci, Cmd, Data) of
	<<0>> ->
	    ok;
	<<255,E/binary>> -> 
	    {error, erlang:binary_to_atom(E, latin1)};
	<<254,E/binary>> -> 
	    {error, binary_to_list(E)};
	<<1,Y:8>> -> {ok,Y};
	<<1,Y:16/native-unsigned>> -> {ok, Y};
	<<1,Y:32/native-unsigned>> -> {ok, Y};
	<<1,Y:64/native-unsigned>> -> {ok, Y};
	<<2,X:32/native-unsigned,Y:32/native-unsigned>> -> {ok,{X,Y}};
	<<3,X/binary>> -> {ok,X};
	<<4,X/binary>> -> {ok,binary_to_list(X)}
    end.
	
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

%% note: bluetooth addresses are reversed in hci
decode_hci_dev_info(
  <<Dev_id:16, Name0:8/binary, F,E,D,C,B,A,
    Flags:32,  Type:8, Features:8/binary,
    Pkt_type:32, Link_policy:32, Link_mode:32, 
    Acl_mtu:16, Acl_pkts:16, 
    Sco_mtu:16, Sco_pkts:16,
    Stat/binary>>) ->
    Name = hci_util:c_string(Name0),
    BdAddr = {A,B,C,D,E,F},
    S = decode_hci_dev_stats(Stat),
    #hci_dev_info {
       dev_id = Dev_id,
       name = Name,
       bdaddr = BdAddr,
       flags = Flags,
       type = Type,
       features = Features,
       pkt_type = Pkt_type,
       link_policy = Link_policy,
       link_mode = Link_mode,
       acl_mtu = Acl_mtu,
       acl_pkts = Acl_pkts,
       sco_mtu = Sco_mtu,
       sco_pkts = Sco_pkts,
       stat = S
      }.

%% 10*4 = 40 bytes
decode_hci_dev_stats(
  << Err_rx:32, Err_tx:32,
     Cmd_tx:32, Evt_rx:32,
     Acl_tx:32, Acl_rx:32,
     Sco_tx:32, Sco_rx:32,
     Byte_rx:32,  Byte_tx:32>>) ->
    #hci_dev_stats {
       err_rx = Err_rx,
       err_tx = Err_tx,
       cmd_tx = Cmd_tx,
       evt_rx = Evt_rx,
       acl_tx = Acl_tx,
       acl_rx = Acl_rx,
       sco_tx = Sco_tx,
       sco_rx = Sco_rx,
       byte_rx = Byte_rx,
       byte_tx = Byte_tx }.

%% bdaddr is reverse in hci
decode_hci_conn_info(<<Handle:16,
		       F,E,D,C,B,A,
		       Type:8, Out:8,
		       State:16, Link_mode:32>>) ->
    #hci_conn_info { handle=Handle, bdaddr={A,B,C,D,E,F},
		     type=Type, out=Out,
		     state=State, link_mode = Link_mode }.

decode_hci_conn_list(<<Item:16/binary, List/binary>>) ->
    [decode_hci_conn_info(Item) | decode_hci_conn_list(List)];
decode_hci_conn_list(<<>>) ->
    [].

encode_hci_filter(#hci_filter { type_mask = Type_mask,
				event_mask = Event_mask,
				opcode = Opcode }) ->
    Event0 = Event_mask band 16#ffffffff,
    Event1 = (Event_mask bsr 32) band 16#ffffffff,
    <<Type_mask:32, Event0:32, Event1:32, Opcode:16>>.

decode_hci_filter(<<Type_mask:32, Event0:32, Event1:32, Opcode:16>>) ->
    Event_mask = Event0 + (Event1 bsl 32),
    #hci_filter { type_mask = Type_mask,
		  event_mask = Event_mask,
		  opcode = Opcode }.

%% given a list of atoms/strings/integers
%% build a bitmask from the values

set_bits(Names, Flags) ->
    set_bits(Names, Flags, 0).

set_bits([Name|Names], Flags, Acc) when is_atom(Name) ->
    case lists:keyfind(atom_to_list(Name), 1, Flags) of
	false ->
	    exit(badarg);
	{_, Bit} ->
	    set_bits(Names, Flags, Bit bor Acc)
    end;
set_bits([Name|Names], Flags, Acc) when is_list(Name) ->
    case lists:keyfind(Name, 1, Flags) of
	false ->
	    exit(badarg);
	{_, Bit} ->
	    set_bits(Names, Flags, Bit bor Acc)
    end;
set_bits([Bits|Names], Flags, Acc) when is_integer(Bits) ->
    set_bits(Names, Flags, Bits bor Acc);
set_bits([], _Flags, Acc) ->
    Acc.
