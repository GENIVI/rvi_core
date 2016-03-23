%%% @author tony <tony@rogvall.se>
%%% @copyright (C) 2013, tony
%%% @doc
%%%    Encode / Decode of netlink messages
%%% @end
%%% Created : 21 May 2013 by tony <tony@rogvall.se>

-module(netlink_codec).

-export([encode_flags/2]).
-export([decode_flags/2]).
-export([encode_tlv/1, encode_tlv_list/1]).
-export([decode_tlv/1, decode_tlv_list/1]).

-export([decode/2]).
-export([encode/2]).

-compile(export_all).

-include("netlink.hrl").
-include("netl_codec.hrl").

-define(ALIGNTO,      4).
-define(ALIGNMASK,    (?ALIGNTO-1)).
-define(NLA_ALIGN(N), (((N)+?ALIGNMASK) band (bnot ?ALIGNMASK))).
-define(NLA_PAD(N),   ((?ALIGNTO-((N) band ?ALIGNMASK)) band 
			   ?ALIGNMASK)).
%% may need adjustment (NLA_ALIGN(sizeof(struct nlattr)))
-define(NLA_HDRLEN, 4). 
%% -define(NLA_PAD(N), (ALIGN(N) - N)).

%% prove that:
%%  ((N+3) band (bnot 3)) - N  ==  (4 - (N band 3)) band 3
%%

encode_flags(Flags, Fun) ->
    encode_flags(Flags, 0, Fun).

encode_flags([], Mask, _Fun) -> Mask;
encode_flags([Flag|Flags], Mask, Fun) ->
    BitNum = Fun(Flag),
    encode_flags(Flags, Mask+(1 bsl BitNum), Fun).


decode_flags(Value, Fun) ->
    decode_flags(0, Value, Fun).

decode_flags(_I, 0, _Fun)   -> [];
decode_flags(I, Value, Fun) ->
    Bit = (1 bsl I),
    if Value band Bit =:= Bit ->
	    [Fun(I) | decode_flags(I+1, Value band (bnot Bit), Fun)];
       true ->
	    decode_flags(I+1,Value,Fun)
    end.

decode_attr([{Ri,_Endian,Rv}|Ds], Fun) ->
    [ Fun(Ri,Rv) | decode_attr(Ds, Fun)];
decode_attr([], _Fun) ->
    [].

encode_tlv({Type0,Endian,Data}) ->
    Payload = if is_list(Data) ->
		      encode_tlv_list(Data);
		 is_binary(Data) -> Data
	      end,
    Type = if is_list(Data) -> 
		   Type0 + 16#8000;
	      Endian =:= big ->
		   Type0 + 16#4000;
	      true ->
		   Type0
	   end,
    Len = ?NLA_HDRLEN+byte_size(Payload),
    Pad = ?NLA_PAD(Len),
    <<Len:16/native-unsigned, Type:16/native-unsigned, 
      Payload/binary, 0:Pad/unit:8>>.

encode_tlv_list(TLVs) ->
    list_to_binary([encode_tlv(TLV) || TLV <- TLVs]).

next_tlv(<<Len0:16/native-unsigned, _:16, Data/binary>>) ->
    Len = Len0 - ?NLA_HDRLEN,
    Pad = ?NLA_PAD(Len),
    <<_:Len/binary, _:Pad/unit:8, Data1/binary>> = Data,
    Data1.

decode_tlv(<<Len0:16/native-unsigned, Type0:16/native-unsigned,
	 Rest/binary>>) ->
    Len = Len0 - ?NLA_HDRLEN,
    <<Payload:Len/binary, _/binary>> = Rest,
    Type = Type0 band 16#3fff,
    if Type0 band 16#8000 =:= 16#8000 ->
	    {Type, native, decode_tlv_list(Payload)};
       Type0 band 16#4000 =:= 16#4000 ->
	    {Type, big, Payload};
       true ->
	    {Type, native, Payload}
    end.

decode_tlv_list(<<>>) ->
    [];
decode_tlv_list(Data) ->
    TLV = decode_tlv(Data),
    Data1 = next_tlv(Data),
    [TLV | decode_tlv_list(Data1)].


decode(Part=
	   << Len:32/native-integer,
	      Type:16/native-integer,
	      Flags:16/native-integer,
	      Seq:32/native-integer,
	      Pid:32/native-integer,
	      Data/binary >>, Acc) ->
    if Len >= byte_size(Part) ->
	    PayloadLen = Len - 16,
	    << Payload:PayloadLen/bytes, NextPart/binary >> = Data,
	    Msg = decode_(Type, Flags, Seq, Pid, Payload),
	    decode(NextPart, [Msg | Acc]);
	true ->
	    decode(<<>>, [{ error, format} | Acc])
    end;
decode(_PadPart, Acc) ->
    lists:reverse(Acc).

decode_(Type,Flags,Seq,Pid,Payload) ->
    MsgType = netl_codec:dec_nlmsg_type(Type),
    FlagList = case msg_type(MsgType) of
		   new ->
		       decode_flags(Flags, fun netl_codec:dec_nlm_new_flags/1);
		   get ->
		       decode_flags(Flags, fun netl_codec:dec_nlm_get_flags/1);
		   _ ->
		       decode_flags(Flags, fun netl_codec:dec_nlm_flags/1)
	       end,
    MsgPayload = apply(netl_codec, 
		       list_to_atom("dec_"++atom_to_list(MsgType)),
		       [{native,Payload}]),
    H = #nlmsghdr { type=MsgType, flags=FlagList, seq=Seq, pid=Pid }, 
    #nlmsg { hdr = H, data=MsgPayload }.

encode(#nlmsghdr { type=MsgType, flags=FlagList, seq=Seq, pid=Pid },
       MsgPayload ) ->
    Type = netl_codec:enc_nlmsg_type(MsgType),
    Flags = case msg_type(MsgType) of
		new ->
		    encode_flags(FlagList, fun netl_codec:enc_nlm_new_flags/1);
		get ->
		    encode_flags(FlagList, fun netl_codec:enc_nlm_get_flags/1);
		_ ->
		    encode_flags(FlagList, fun netl_codec:enc_nlm_flags/1)
	    end,
    Payload = apply(netl_codec, 
		    list_to_atom("enc_"++atom_to_list(MsgType)),
		    [{native,MsgPayload}]),
    N = byte_size(Payload),
    Pad = ?NLA_PAD(N),
    Len = 16 + Pad + N,
    << Len:32/native-integer,
       Type:16/native-integer,
       Flags:16/native-integer,
       Seq:32/native-integer,
       Pid:32/native-integer,
       Payload/binary,
       0:Pad/unit:8 >>.

msg_type(Type) ->
    case Type of
	noop -> misc;
	error -> misc;
	done -> misc;
	overrun -> misc;
	newlink -> new;
	dellink -> del;
	getlink -> get;
	setlink -> set;
	newaddr -> new;
	deladdr -> del;
	getaddr -> get;
	newroute -> new;
	delroute -> del;
	getroute -> get;
	newneigh -> new;
	delneigh -> del;
	getneigh -> get;
	newrule -> new;
	delrule -> del;
	getrule -> get;
	newdisc -> new;
	delqdisc -> del;
	getqdisc -> get;
	newtclass -> new;
	deltclass -> del;
	gettclass -> get;
	newtfilter -> new;
	deltfilter -> del;
	gettfilter -> get;
	newaction -> new;
	delaction -> del;
	getaction -> get;
	newprefix -> new;
	getmulticast -> get;
	getanycast -> get;
	newneightbl -> new;
	getneightbl -> get;
	setneightbl -> set;
	newnduseropt -> new;
	newaddrlabel -> new;
	deladdrlabel -> del;
	getaddrlabel -> get;
	getdcb -> get;
	setdcb -> set
    end.
