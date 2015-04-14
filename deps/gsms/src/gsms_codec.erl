%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Send/Recive SMS with GSM modem
%%% @end
%%% Created : 15 Oct 2012 by Tony Rogvall <tony@rogvall.se>

-module(gsms_codec).

-compile(export_all).

-export([decode_in/1, decode_in_hex/1]).
-export([decode_out/1, decode_out_hex/1]).
-export([hex_to_binary/1]).
-export([is_valid_scts/1]).
-export([decode_dcs/1]).

-import(lists, [reverse/1]).

-include("log.hrl").
-include("../include/gsms.hrl").

-define(Q, $").

-define(decode_bool(X), ((X) =/= 0)).
-define(encode_bool(X), if (X) -> 1; true -> 0 end).
-define(is_byte(X), (((X) band (bnot 16#ff)) =:= 0)).
-define(is_short(X), (((X) band (bnot 16#ffff)) =:= 0)).

decode_in_hex(RawData) ->
    decode_in(hex_to_binary(RawData)).

decode_out_hex(RawData) ->
    decode_out(hex_to_binary(RawData)).

-spec decode_in(Message::binary()) ->
		       {ok, #gsms_deliver_pdu{}} | 
		       {error,Reason::atom()}.

decode_in(<<L1,SmscAddr:L1/binary,
	    TP_RP:1,TP_UDHI:1,TP_SRI:1,R1:1,R2:1,TP_MMS:1,?MTI_SMS_DELIVER:2,
	    AddrLen,Data0/binary>>) ->
    ALen = (AddrLen + 1) bsr 1,  %% number of bytes
    <<AType,Addr:ALen/binary,TP_PID,TP_DCS,
      TP_SCTS:7/binary,TP_UDL,TP_UD/binary>> = Data0,
    AddrType = decode_addr_type(AType),
    Dcs = decode_dcs(TP_DCS),
    {UDH,UD} = decode_ud(Dcs,TP_UDHI,TP_UDL,TP_UD),
    {ok, #gsms_deliver_pdu {
	    smsc = decode_smsc_addr(SmscAddr),
	    rp   = ?decode_bool(TP_RP),
	    udhi = ?decode_bool(TP_UDHI),
	    sri  = ?decode_bool(TP_SRI),
	    res1 = R1,
	    res2 = R2,
	    mms  = ?decode_bool(TP_MMS),
	    addr = decode_addr(AddrType,Addr),
	    pid = TP_PID,
	    dcs = Dcs,
	    scts = decode_scts(TP_SCTS),
	    udl  = TP_UDL,
	    udh  = UDH,
	    ud   = UD
	   }};
decode_in(Binary) when is_binary(Binary) ->
    {error, einval}.

			    
-spec decode_out(Message::binary()) ->
			{ok, #gsms_submit_pdu{}} | 
			{error,Reason::atom()}.

decode_out(<<L1,SmscAddr:L1/binary,
	     TP_RP:1,TP_UDHI:1,TP_SRR:1,TP_VPF:2,TP_RD:1,?MTI_SMS_SUBMIT:2,
	     TP_MREF,
	     AddrLen,   %% in semi octets
	     Data0/binary>>) ->
    ALen = (AddrLen + 1) bsr 1,  %% number of bytes
    <<AType,Addr:ALen/binary,TP_PID,TP_DCS,Data1/binary>> = Data0,
    AddrType = decode_addr_type(AType),
    VPF = decode_vpf(TP_VPF),
    VPLen = case VPF of
		none -> 0;
		relative -> 1;
		enhanced -> 7;
		absolute -> 7
	    end,
    <<VP:VPLen/binary, TP_UDL:8, TP_UD/binary>> = Data1,
    Dcs = decode_dcs(TP_DCS),
    {UDH,UD} = decode_ud(Dcs,TP_UDHI,TP_UDL,TP_UD),
    {ok, #gsms_submit_pdu { 
	    smsc = decode_smsc_addr(SmscAddr),
	    rp   = ?decode_bool(TP_RP),
	    udhi = ?decode_bool(TP_UDHI),
	    srr  = ?decode_bool(TP_SRR),
	    vpf  = VPF,
	    rd   = ?decode_bool(TP_RD),
	    mref = TP_MREF,
	    addr = decode_addr(AddrType,Addr),
	    pid = TP_PID,
	    dcs = Dcs,
	    vp  = decode_vp(VPF,VP),
	    udl = TP_UDL,
	    udh = UDH,
	    ud = UD
	   }};
decode_out(Binary) when is_binary(Binary) ->
    {error, einval}.



%% return a list of pdu's
-spec make_sms_submit(Opts::[gsms_pdu_option()],
		      Message::[integer()]) -> 
			     {ok,[#gsms_submit_pdu{}]} |
			     {error, term()}.
			       
make_sms_submit(Opts,Message) ->
    case set_pdu_opts(Opts,#gsms_submit_pdu{}) of
	{ok,Pdu} ->
	    Ref = proplists:get_value(ref, Opts, 1),
	    {Dcs,UDList} = encode_ud(Pdu#gsms_submit_pdu.dcs, Message,
				     Pdu#gsms_submit_pdu.udh, Ref),
	    {ok,lists:map(
		  fun({Udl,Ud,Udhi0}) ->
			  Udhi = Udhi0 or Pdu#gsms_submit_pdu.udhi,
			  Pdu#gsms_submit_pdu { dcs=Dcs, udl=Udl, 
						ud=Ud, udhi=Udhi }
		  end, UDList)};
	Error ->
	    Error
    end.


-spec set_pdu_opts(Opts::[gsms_pdu_option()], Pdu::#gsms_submit_pdu{}) ->
			  {ok,Pdu::#gsms_submit_pdu{}} |
			  {error, badarg}.

			  
set_pdu_opts([{Key,Value}|Kvs], R=#gsms_submit_pdu{}) ->
    case Key of
	smsc when is_record(Value,gsms_addr) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { smsc=Value });	    
	smsc when is_list(Value),hd(Value)=:=$+ ->
	    Addr = #gsms_addr { type=international, addr=Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { smsc=Addr });
	smsc when is_list(Value) ->
	    Addr = #gsms_addr { type=unknown, addr=Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { smsc=Addr});
	rp when is_boolean(Value) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { rp=Value });
	udhi when is_boolean(Value) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { udhi=Value });
	udh when is_list(Value), Value =/= [] ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { udhi=true, udh=Value });
	udh when Value =:= [] ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { udhi=false, udh=Value });
	srr when is_boolean(Value) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { srr=Value });
	mref when ?is_byte(Value) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { mref=Value });
	vpf when Value =:= none;
		 Value =:= relative; 
		 Value =:= enhanced; 
		 Value =:= absolute ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { vpf=Value });
	vp ->
	    case Value of
		none ->
		    set_pdu_opts(Kvs, R#gsms_submit_pdu 
				 { vpf = none, vp=undefined });
		{relative, Seconds} when is_integer(Seconds), Seconds>=0 ->
		    set_pdu_opts(Kvs, R#gsms_submit_pdu 
				 { vpf = relative, vp=Seconds });
		{absolute,DateTimeTz} ->
		    case is_valid_scts(DateTimeTz) of
			true ->
			    set_pdu_opts(Kvs, R#gsms_submit_pdu 
					 { vpf = absolute,
					   vp=DateTimeTz });
			false ->
			    {error, badarg}
		    end;
		{enhanced,_} ->
		    {error, not_supported}  %% yet
	    end;
	addr when is_record(Value,gsms_addr) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { addr=Value });
	addr when is_list(Value),hd(Value)=:=$+ ->
	    Addr = #gsms_addr { type=international, addr=Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { addr=Addr});
	addr when is_list(Value) ->
	    Addr = #gsms_addr { type=unknown, addr=Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { addr=Addr});
	pid when ?is_byte(Value) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { pid=Value });
	dcs when is_record(Value,gsms_dcs) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { dcs=Value });
	dcs when is_integer(Value) ->
	    Dcs = decode_dcs(Value),
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { dcs=Dcs });
	type -> %% fixme test
	    Dcs = R#gsms_submit_pdu.dcs,
	    Dcs1 = Dcs#gsms_dcs { type = Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { dcs=Dcs1 });
	class -> %% fixme test
	    Dcs = R#gsms_submit_pdu.dcs,
	    Dcs1 = Dcs#gsms_dcs { class = Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { dcs=Dcs1 });
	alphabet -> %% fixme test
	    Dcs = R#gsms_submit_pdu.dcs,
	    Dcs1 = Dcs#gsms_dcs { alphabet = Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { dcs=Dcs1 });
	compression -> %% fixme test
	    Dcs = R#gsms_submit_pdu.dcs,
	    Dcs1 = Dcs#gsms_dcs { compression = Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { dcs=Dcs1 });
	store -> %% fixme test
	    Dcs = R#gsms_submit_pdu.dcs,
	    Dcs1 = Dcs#gsms_dcs { store = Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { dcs=Dcs1 });
	wait_type -> %% fixme test
	    Dcs = R#gsms_submit_pdu.dcs,
	    Dcs1 = Dcs#gsms_dcs { wait_type = Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { dcs=Dcs1 });
	%% recognized options, but not for pdu
	notify ->  %% fixme test?
	    set_pdu_opts(Kvs, R);	    
	ref ->  %% fixme test?
	    set_pdu_opts(Kvs, R);
	_ ->
	    lager:debug("set_pdu_opts: unknown pdu option ~p", 
			[{Key,Value}]),
	    {error, badarg}
    end;
set_pdu_opts([], R) ->
    {ok,R}.

is_valid_scts({{Date,{H,M,S}},Tz}) ->
    try calendar:valid_date(Date) of
	true ->
	    if is_integer(H), H >= 0, H =< 23,
	       is_integer(M), M >= 0, M =< 59,
	       is_integer(S), S >= 0, S =< 59,
	       is_float(Tz) ->
		    true;
	       true ->
		    false
	    end;
	false -> false
    catch
	error:_ -> false
    end;
is_valid_scts(_) ->
    false.

    


encode_sms(R=#gsms_submit_pdu{}) ->
    SmscAddr = encode_smsc_addr(R#gsms_submit_pdu.smsc),
    TP_RP   = ?encode_bool(R#gsms_submit_pdu.rp),
    TP_UDHI = ?encode_bool(R#gsms_submit_pdu.udhi),
    TP_SRR  = ?encode_bool(R#gsms_submit_pdu.srr),
    TP_VPF  = encode_vpf(R#gsms_submit_pdu.vpf),
    TP_RD   = ?encode_bool(R#gsms_submit_pdu.rd),
    TP_MREF = R#gsms_submit_pdu.mref,
    {AType,Addr,AddrLen} = encode_addr(R#gsms_submit_pdu.addr),
    TP_PID = R#gsms_submit_pdu.pid,
    TP_DCS = encode_dcs(R#gsms_submit_pdu.dcs),
    TP_VP  = encode_vp(R#gsms_submit_pdu.vpf,R#gsms_submit_pdu.vp),
    TP_UDL = R#gsms_submit_pdu.udl,
    TP_UD  = R#gsms_submit_pdu.ud,
    ?debug("SmscAddr=~p,Addr=~p,TP_VP=~p,TP_UD=~p\n", 
	   [SmscAddr,Addr,TP_VP,TP_UD]),
    L1 = byte_size(SmscAddr),
    <<L1,SmscAddr:L1/binary,
      TP_RP:1,TP_UDHI:1,TP_SRR:1,TP_VPF:2,TP_RD:1,?MTI_SMS_SUBMIT:2,
      TP_MREF,
      AddrLen,  %% in semi octets
      AType,
      Addr/binary,
      TP_PID,
      TP_DCS,
      TP_VP/binary,
      TP_UDL, 
      TP_UD/binary>>.


binary_to_hex(Bin) ->
    [ element(I+1,{$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,
		   $A,$B,$C,$D,$E,$F}) || <<I:4>> <= Bin ].

hex_to_binary(RawData) ->
    << << H:4 >> || H <- hex_norm_nibbles(RawData) >>.        

hex_norm_nibbles([$\s|Cs]) -> hex_norm_nibbles(Cs);
hex_norm_nibbles([$\t|Cs]) -> hex_norm_nibbles(Cs);
hex_norm_nibbles([$\r|Cs]) -> hex_norm_nibbles(Cs);
hex_norm_nibbles([$\n|Cs]) -> hex_norm_nibbles(Cs);
hex_norm_nibbles([C|Cs]) ->
    if C >= $0, C =< $9 -> [C-$0 | hex_norm_nibbles(Cs)];
       C >= $A, C =< $F -> [(C-$A)+10 | hex_norm_nibbles(Cs)];
       C >= $a, C =< $f -> [(C-$A)+10 | hex_norm_nibbles(Cs)]
    end;
hex_norm_nibbles([]) ->
    [].

%%
%% addresses
%%

decode_smsc_addr_type(<<AType,_/binary>>) ->
    decode_addr_type(AType);
decode_smsc_addr_type(<<>>) ->
    undefined.

decode_smsc_addr(<<AType,Ds/binary>>) ->
    T = decode_addr_type(AType),
    A = decode_addr_semi(T, Ds),
    #gsms_addr { type=T, addr=A };
decode_smsc_addr(<<>>) ->
    undefined.

decode_vpf(2#00) -> none;
decode_vpf(2#10) -> relative;
decode_vpf(2#01) -> enhanced;
decode_vpf(2#11) -> absolute.

encode_vpf(none)     -> 2#00;
encode_vpf(relative) -> 2#10;
encode_vpf(enhanced) -> 2#01;
encode_vpf(absolute) -> 2#11.

%% validity period in seconds
decode_vp(none,<<>>) -> 0;
decode_vp(relative,<<VP>>) ->
    if VP =< 143 -> (VP+1)*5*60;               %% 1..12  hours  (+ 5min)
       VP =< 167 -> (12*60 + (VP-143)*30)*60;  %% 12..24 hours (+ 30min)
       VP =< 196 -> (VP-166)*24*60*60;         %% 2 .. 30 days
       true      -> ((VP-192)*7*24)*60*60      %% 5 .. 63 weeks
    end;
decode_vp(absolute,V) ->
    decode_scts(V).

encode_vp(none, _) -> <<>>;
encode_vp(relative, V) -> 
    Min_0  = V div 60,        %% number of minutes
    Hour_1 = Min_0 div 60,    %% number of hours
    _Min_1  = Min_0 rem 60,   %% mintes with in the hour
    Day_2  = Hour_1 div 24,   %% days
    _Hour_2 = Hour_1 rem 24,  %% hour with in day
    Week_3 = Day_2 div 7,     %% weeks
    _Day_3  = Day_2 rem 7,    %% day within week
    if Week_3 >= 5, Week_3 =< 64 -> 
	    <<((Week_3-5)+197)>>;
       Day_2  >= 2, Day_2 =< 30 ->
	    <<((Day_2-2)+168)>>;
       Min_0  >= 12*60, Min_0 =< 24*60 ->
	    <<(((Min_0 - 12*60) div 30) + 143)>>;
       Min_0 >= 5 ->
	    <<((Min_0 div 5)-1)>>;
       true ->
	    <<0>>
    end;
encode_vp(absolute,V) -> 
    encode_scts(V).
       
encode_smsc_addr(undefined) ->
    <<>>;
encode_smsc_addr(#gsms_addr {type=T,addr=A}) ->
    AType = encode_addr_type(T),
    Ds = encode_addr_semi(T, A),
    <<AType,Ds/binary>>.

%% Handle the ones in use only! (FIXME)
decode_addr_type(2#10000001) -> unknown;
decode_addr_type(2#10010001) -> international;
decode_addr_type(2#10101000) -> national;
decode_addr_type(T) -> T.

encode_addr_type(unknown)       -> 16#81; %% 129
encode_addr_type(international) -> 16#91; %% 145
encode_addr_type(national)      -> 16#A8;
encode_addr_type(T) -> T.

decode_addr(T, <<>>) -> 
    #gsms_addr { type=T, addr=[] };
decode_addr(T, Data) ->
    A = decode_addr_semi(T, Data),
    #gsms_addr { type=T, addr=A }.

encode_addr(#gsms_addr { type=T, addr=A }) ->
    Addr = encode_addr_semi(T, A),
    AddrLen = length(A),   %% number semi digits
    {encode_addr_type(T),Addr,AddrLen}.

decode_addr_semi(international, SemiOctets) ->
    [$+|decode_semi_octets(SemiOctets)];
decode_addr_semi(_, SemiOctets) ->
    decode_semi_octets(SemiOctets).

encode_addr_semi(international, [$+|Ds]) ->
    encode_semi_octets(Ds);
encode_addr_semi(_, Ds) ->
    encode_semi_octets(Ds).

     
decode_bcd(<<H:4,L:4, T/binary>>) ->
    [H*10+L | decode_bcd(T)];
decode_bcd(<<>>) ->
    [].

decode_swapped_bcd(Bin) ->
    [(H*10+L) || <<L:4,H:4>> <= Bin].

encode_swapped_bcd(Ds) ->
    << <<(D rem 10):4, (D div 10):4>> || D <- Ds >>.

decode_semi_octets(<<>>) -> [];
decode_semi_octets(<<16#F:4,D:4>>) -> [D+$0];
decode_semi_octets(<<L:4,H:4,Ds/binary>>) -> [H+$0,L+$0|decode_semi_octets(Ds)].

encode_semi_octets([])  -> <<>>;
encode_semi_octets([D]) -> <<16#F:4,(D-$0):4>>;
encode_semi_octets([H,L|Ds]) ->
    <<(L-$0):4,(H-$0):4, (encode_semi_octets(Ds))/binary>>.
    
decode_scts(Bin) ->
    [YY,Mon,Day,Hour,Min,Sec,Tz] = decode_swapped_bcd(Bin),
    {Year0,_,_} = date(),   %% Year0 is current year
    YY0 = Year0 rem 100,    %% YY0 curent year two digits
    Century = Year0 - YY0,  %% Current Century,
    Year = if YY > YY0+1 ->
		   YY + (Century - 100);
	      true ->
		   YY + Century
	   end,
    Date = {Year,Mon,Day},
    Time = {Hour,Min,Sec},
    Tz0  = (15*(Tz band 16#7f))/60,
    TzH = if (Tz band 16#80) =/= 0 -> -Tz0; true -> Tz0 end,
    {{Date,Time},TzH}.

encode_scts({{{Year0,Mon,Day},{Hour,Min,Sec}},TzH}) ->
    Year = Year0 - 2000,
    Tz = if TzH < 0 -> 
		 16#80 bor (trunc((TzH * 60)/15) band 16#7f);
	    true ->
		 trunc((TzH * 60)/15) band 16#7f
	 end,
    encode_swapped_bcd([Year,Mon,Day,Hour,Min,Sec,Tz]).


decode_dcs(V) ->
    if (V bsr 6) =:= 2#00 -> %% 00xxxxxx
	    #gsms_dcs { type=message,
			compression = if V band 2#00100000 =:= 0 -> 
					      uncompressed;
					 true -> compressed
				      end,
			alphabet = case V band 2#1100 of
				       2#0000 -> default;
				       2#0100 -> octet;
				       2#1000 -> ucs2;
				       2#1100 -> reserved
				   end,
			class = case V band 2#11 of
				    2#00 -> alert;
				    2#01 -> me;
				    2#10 -> sim;
				    2#11 -> te
				end };
       (V bsr 4) =:= 2#1100 -> %% message waiting, discard
	    dcs_message_waiting(V,discard,default);
       (V bsr 4) =:= 2#1101 -> %% message waiting, store
	    dcs_message_waiting(V,store,default);
       (V bsr 4) =:= 2#1110 -> %% message waiting, store
	    dcs_message_waiting(V,store,ucs2);
       (V bsr 4) =:= 2#1111 ->
	    #gsms_dcs { type=data,compression=uncompressed,
			alphabet=if V band 2#0100 =:= 0 -> octet;
				    true -> default
				 end,
			class = case V band 2#11 of
				    2#00 -> alert;
				    2#01 -> me;
				    2#10 -> sim;
				    2#11 -> te
				end
			};
       true -> 
	    lager:error("unknown dcs value ~p", [V]),
	    #gsms_dcs {}
    end.
	    
dcs_message_waiting(V,Store,Alphabet) ->
    #gsms_dcs {type=message_waiting,
	       compression=uncompressed,
	       alphabet=Alphabet,
	       store=Store,
	       active=if V band 2#0100 =:= 0 ->
			      inactive;
			 true -> active
		      end,
	       wait_type = case V band 2#11 of
			       2#00 -> voicemail;
			       2#01 -> fax;
			       2#10 -> email;
			       2#11 -> other
			   end
	      }.


encode_dcs(#gsms_dcs{type=message,compression=Comp,
		     alphabet=Alpha,class=Class}) ->
    2#00000000 +
	if Comp =:= compressed -> 2#00100000;
	   true -> 2#00000000
	end +
	case Alpha of 
	    default  -> 2#0000;
	    octet    -> 2#0100;
	    ucs2     -> 2#1000;
	    reserved -> 2#1100
	end + 
	case Class of
	    alert -> 2#00;
	    me    -> 2#01;
	    sim   -> 2#10;
	    te    -> 2#11
	end;
encode_dcs(#gsms_dcs {type=message_waiting,compression=uncompressed,
		      alphabet=Alphabet,store=Store,active=Active,
		      wait_type=WaitType}) ->
    if Alphabet =:= ucs2, Store =:= store ->
	    2#11110000;
       Alphabet =:= default, Store =:= store ->
	    2#11100000;
       Alphabet =:= default, Store =:= discard ->
	    2#11000000
    end +
	if Active =:= active -> 2#0100;
	   true -> 2#0000
	end +
	case WaitType of
	    voicemail -> 2#00;
	    fax -> 2#01;
	    email -> 2#10;
	    other -> 2#11
	end;
encode_dcs(#gsms_dcs{type=data,compression=uncompressed,
		     alphabet=Alphabet,class=Class}) ->
    2#11110000 +
	if Alphabet =:= octet -> 2#100;
	   true -> 2#000
	end +
	case Class of
	    alert -> 2#00;
	    me    -> 2#01;
	    sim   -> 2#10;
	    te    -> 2#11
	end.

		   
decode_ud(#gsms_dcs{compression=uncompressed,alphabet=default},
	  0, UDL, Data) ->
    L7 = UDL,                  %% number of septets
    <<Gsm8:L7/binary,_/binary>> = decode_gsm7(Data),
    {[],gsms_0338:decode(Gsm8)};
decode_ud(#gsms_dcs{compression=uncompressed,alphabet=default},
	  1, UDL, Data = <<UDHL,_/binary>>) ->
    %% UDHL as number of septets and including it self
    UDHL7  = ((UDHL+1)*8 + 6) div 7,
    L7 = UDL-UDHL7,  %% number of septets in message
    <<_:UDHL7/binary,Gsm8:L7/binary,_/binary>> = decode_gsm7(Data),
    <<_,UDHBin:UDHL/binary,_/binary>> = Data,
    UDH = decode_udh(UDHBin),
    {UDH,gsms_0338:decode(Gsm8)};
decode_ud(#gsms_dcs{compression=uncompressed,alphabet=ucs2},
	  0,UDL,Data1) ->
    case Data1 of
	<<Data2:UDL/binary, _/binary>> ->
	    Ucs = [ U || <<U:16/big>> <= Data2 ],
	    {[],Ucs}
    end;
decode_ud(#gsms_dcs{compression=uncompressed,alphabet=ucs2},
	  1, UDL, Data1 = <<UDHL,UDHBin:UDHL/binary,_/binary>>) ->
    Pad = (UDHL+1) band 1,
    UDL8 = UDL - (UDHL+1+Pad),
    %% io:format("UDL=~w, Pad=~w, UDL8=~w\n", [UDL,Pad,UDL8]),
    case Data1 of
	<<_,_:UDHL/binary,_:Pad/unit:8,Data2:UDL8/binary>> ->
	    UDH = decode_udh(UDHBin),
	    {UDH, [ U || <<U:16/big>> <= Data2 ]}
    end;
decode_ud(#gsms_dcs{compression=uncompressed,alphabet=octet},
	  0,UDL, Data1) ->
    case Data1 of
	<<Data2:UDL/binary>> ->
	    {[], binary_to_list(Data2)}
    end;
decode_ud(#gsms_dcs{compression=uncompressed,alphabet=octet},
	  1, UDL, <<UDHL,UDHBin:UDHL/binary,Data1/binary>>) ->
    UDL8 = UDL - (UDHL+1),
    case Data1 of
	<<_,_:UDHL/binary,Data2:UDL8/binary>> ->
	    UDH = decode_udh(UDHBin),
	    {UDH, binary_to_list(Data2)}
    end.



encode_ud(Dcs0=#gsms_dcs{alphabet=Alpha0},
	  Message, UDH, Ref) ->
    %% fixme: check that there are no concat header ?
    %% calculate total number of letters in message and the encoding
    {Alpha,_TotLen} = message_len(Message,Alpha0),
    Dcs = Dcs0#gsms_dcs{alphabet=Alpha},
    case encode_ud_(Dcs, Message, UDH) of
	{UDL,UD,[],UDHI} ->
	    {Dcs, [{UDL,iolist_to_binary(UD),UDHI}]};
	{_UDL,_UD,_Message1,_} ->
	    {Dcs,encode_ud_loop(Dcs, Message, UDH, Ref, 1, [])}
    end.

encode_ud_loop(Dcs, Message, UDH, Ref, I, Acc) ->
    case encode_ud_(Dcs, Message, [{concat,Ref,0,I}|UDH]) of
	{UDL,UD,[],UDHI} ->
	    ud_patch([{UDL,UD,UDHI}|Acc], I, []);
	{UDL,UD,Message1,UDHI} ->
	    UDH1 = [], %% kill header for segments
	    encode_ud_loop(Dcs,Message1,UDH1,Ref,I+1,[{UDL,UD,UDHI} | Acc])
    end.

%% patch concat and reverse list
ud_patch([{UDL,UD,UDHI}|Tail], N, Acc) ->
    case UD of
	[UDHL,[<<?IE_CONCAT8,3,Ref:8,_N,I>>|UDH],Data] ->
	    UD1 = iolist_to_binary([UDHL,[<<?IE_CONCAT8,3,Ref:8,N,I>>|UDH],
				    Data]),
	    ud_patch(Tail, N, [{UDL,UD1,UDHI}|Acc]);
	[UDHL,[<<?IE_CONCAT16,4,Ref:16,_N,I>>|UDH],Data] ->
	    UD1 = iolist_to_binary([UDHL,[<<?IE_CONCAT16,4,Ref:16,N,I>>|UDH],
				    Data]),
	    ud_patch(Tail, N, [{UDL,UD1,UDHI}|Acc])
    end;
ud_patch([], _N, Acc) ->
    Acc.


encode_ud_(#gsms_dcs{compression=uncompressed,alphabet=default},
	   Message, UDH) ->
    case len_udh(UDH) of
	0 ->
	    {Gsm8,Message1} = gsms_0338:encode(Message,?MAX_7BIT_LEN),
	    L7 = byte_size(Gsm8),        %% number of septets
	    L8 = (L7*7 + 7) div  8,      %% number of octets needed
	    <<Gsm7:L8/binary,_/binary>> = encode_gsm7(Gsm8),
	    {L7,Gsm7,Message1,false};
	UDHL ->
	    %% UDHL as number of septets and including it self
	    UDHL8  = (UDHL+1)*8,        %% number of bits including self
	    UDHL7  = (UDHL8 + 6) div 7, %% number of septets
	    UDHData = encode_udh(UDH),
	    Message0 = prepend(UDHL7,0,Message),
	    {Gsm8,Message1} = gsms_0338:encode(Message0,?MAX_7BIT_LEN-UDHL7),
	    Pad  = UDHL7*7 - UDHL8,
	    L7 = byte_size(Gsm8)-UDHL7,  %% number of septets in message
	    L8 = (L7*7+Pad+7) div  8,    %% number of octets needed
	    <<_,_:UDHL/binary,Gsm7:L8/binary,_/binary>> = encode_gsm7(Gsm8),
	    {UDHL7+L7,[UDHL,UDHData,Gsm7],Message1,true}
    end;
encode_ud_(#gsms_dcs{compression=uncompressed,alphabet=ucs2}, Message, UDH) ->
    case len_udh(UDH) of
	0 ->
	    {Ucs2,Message1} = encode_ucs2(Message,?MAX_16BIT_LEN,[]),
	    N = byte_size(Ucs2),
	    {N, Ucs2, Message1, false};
	UDHL ->
	    UDHData = encode_udh(UDH),
	    Pad = (UDHL+1) band 1,
	    UDHL16 = (UDHL+1+Pad) div 2, %% check this
	    {Ucs2,Message1} = encode_ucs2(Message,?MAX_16BIT_LEN-UDHL16,[]),
	    N = byte_size(Ucs2),
	    Ucs21 = prepend(Pad,0,Ucs2),
	    UDL = N + UDHL + 1 + Pad,
	    {UDL,[UDHL,UDHData,Ucs21],Message1,true}
    end;
encode_ud_(#gsms_dcs{compression=uncompressed,alphabet=octet}, Message, UDH) ->
    case len_udh(UDH) of
	0 ->
	    {Octets,Message1} = encode_octets(Message,?MAX_8BIT_LEN,[]),
	    N = byte_size(Octets),
	    {N, Octets, Message1, false};
	UDHL ->
	    UDHData = encode_udh(UDH),
	    {Octets,Message1} = encode_octets(Message,?MAX_8BIT_LEN-UDHL,[]),
	    N = byte_size(Octets),
	    {UDHL+N,[UDHL,UDHData,Octets],Message1,true}
    end.



encode_octets(Cs, 0, Acc) -> 
    {iolist_to_binary(lists:reverse(Acc)), Cs};
encode_octets([C|Cs], I, Acc) -> 
    encode_octets(Cs, I-1, [C|Acc]);
encode_octets([], _I, Acc) -> 
    {iolist_to_binary(lists:reverse(Acc)), []}.

%%
%% Fixme: add BOM (byte order mark) character
%% BOM = 0xFEFF, if BOM is received as 0xFEFF then 
%% it is coded in default big endian, else it
%% will be read as 0xFFFE and is coded in little endian.
%%
encode_ucs2(Cs, 0, Acc) -> 
    {iolist_to_binary(lists:reverse(Acc)), Cs};
encode_ucs2([C|Cs], I, Acc) -> 
    encode_ucs2(Cs, I-1, [<<C:16/big>>|Acc]);
encode_ucs2([], _I, Acc) -> 
    {iolist_to_binary(lists:reverse(Acc)), []}.



message_len(Cs) ->
    message_len_auto_(Cs,0).

message_len(Cs,auto) ->
    message_len_auto_(Cs,0);
message_len(Cs,default) ->
    {default,message_len_default_(Cs,0)};
message_len(Cs,octet) ->
    {octet,message_len_octet_(Cs,0)};
message_len(Cs,ucs2) ->
    {ucs2,message_len_ucs2_(Cs,0)}.


message_len_auto_(Cs,N) ->
    try message_len_default_(Cs,N) of
	Len7 -> {default,Len7}
    catch
	error:_ ->
	    {ucs2,message_len_ucs2_(Cs,N)}
    end.

message_len_default_([C|Cs], Len) ->
    case gsms_0338:encode_char(C) of
	[_Esc,_Y] -> message_len_default_(Cs, Len+2);
	_Y -> message_len_default_(Cs, Len+1)
    end;
message_len_default_([], Len) ->
    Len.

message_len_octet_([C|Cs], Len) when ?is_byte(C) ->
    message_len_octet_(Cs, Len+1);
message_len_octet_([], Len) ->
    Len.

message_len_ucs2_([C|Cs], Len) when ?is_short(C) ->
    message_len_ucs2_(Cs, Len+1);
message_len_ucs2_([], Len) ->
    Len.

	
%%
%% Calculate number of bytes in UDH (not incuding the length byte)
%%
-spec len_udh(IEs::[gsms_ie()]) -> integer().

len_udh([]) ->
    0;
len_udh(IEs) ->
    len_udh(IEs, 0).

len_udh([{concat,Ref,_N,_I}|IEs],Len) ->
    if Ref >= 0, Ref =< 16#ff ->   len_udh(IEs, Len+5);
       Ref >= 0, Ref =< 16#ffff -> len_udh(IEs, Len+6)
    end;
len_udh([{concat8,_Ref,_N,_I}|IEs], Len) -> len_udh(IEs,Len+5);
len_udh([{concat16,_Ref,_N,_I}|IEs], Len) -> len_udh(IEs,Len+6);
len_udh([{port,Dst,Src}|IEs], Len) ->
    if Dst >= 0, Dst =< 16#ff,
       Src >= 0, Src =< 16#ff ->
	    len_udh(IEs,Len+4);
       Dst >= 0, Dst =< 16#ffff,
       Src >= 0, Src =< 16#ffff ->
	    len_udh(IEs,Len+6)
    end;
len_udh([{port8,_Dst,_Src}|IEs], Len) -> len_udh(IEs,Len+4);
len_udh([{port16,_Dst,_Src}|IEs],Len) -> len_udh(IEs,Len+6);
len_udh([], Len) -> Len.

%%
%% Encode user data header IEs 
%%
-spec encode_udh(IEs::[gsms_ie()]) -> [binary()].

encode_udh([]) ->
    [];
encode_udh(IEs) ->
    encode_udh(IEs, []).

encode_udh([{concat,Ref,N,I}|IEs], Acc) ->
    if Ref >= 0, Ref =< 16#ff ->
	    encode_udh(IEs,[ <<?IE_CONCAT8,3,Ref,N,I>>|Acc]);
       Ref >= 0, Ref =< 16#ffff ->
	    encode_udh(IEs,[ <<?IE_CONCAT16,4,Ref:16,N,I>> | Acc ])
    end;
encode_udh([{concat8,Ref,N,I}|IEs], Acc) ->
    encode_udh(IEs,[ <<?IE_CONCAT8,3,Ref,N,I>>|Acc]);
encode_udh([{concat16,Ref,N,I}|IEs], Acc) ->
    encode_udh(IEs,[ <<?IE_CONCAT16,4,Ref:16,N,I>> | Acc ]);
encode_udh([{port,Dst,Src}|IEs], Acc) ->
    if Dst >= 0, Dst =< 16#ff,
       Src >= 0, Src =< 16#ff ->
	    encode_udh(IEs,[ <<?IE_PORT8,2,Dst,Src>> | Acc]);
       Dst >= 0, Dst =< 16#ffff,
       Src >= 0, Src =< 16#ffff ->
	    encode_udh(IEs,[<<?IE_PORT16,4,Dst:16,Src:16>> | Acc])
    end;
encode_udh([{port8,DstPort,SrcPort}|IEs], Acc) ->
    encode_udh(IEs,[ <<?IE_PORT8,2,DstPort,SrcPort>> | Acc]);
encode_udh([{port16,DstPort,SrcPort}|IEs],Acc) ->
    encode_udh(IEs,[<<?IE_PORT16,4,DstPort:16,SrcPort:16>> | Acc]);
encode_udh([{ie,IE,Args}|IEs], Acc) when is_binary(Args) ->
    encode_udh(IEs,[<<IE,(byte_size(Args)),Args/binary>> | Acc]);
encode_udh([], Acc) ->
    reverse(Acc).


decode_udh(UDHBin) when is_binary(UDHBin) ->
    decode_udh(UDHBin, []).

decode_udh(<<?IE_CONCAT8,3,Ref,N,I,IEs/binary>>, Acc) ->
    decode_udh(IEs,[{concat,Ref,N,I}|Acc]);
decode_udh(<<?IE_CONCAT16,4,Ref:16,N,I,IEs/binary>>,Acc) ->
    decode_udh(IEs,[{concat,Ref,N,I}|Acc ]);
decode_udh(<<?IE_PORT8,2,DstPort,SrcPort,IEs/binary>>, Acc) ->
    decode_udh(IEs,[{port,DstPort,SrcPort}| Acc]);
decode_udh(<<?IE_PORT16,4,DstPort:16,SrcPort:16,IEs/binary>>,Acc) ->
    decode_udh(IEs,[{port,DstPort,SrcPort} | Acc]);
decode_udh(<<IE,N,Args:N/binary,IEs/binary>>,Acc) ->
    decode_udh(IEs,[{ie,IE,Args}|Acc]);
decode_udh(<<>>, Acc) ->
    reverse(Acc).


decode_gsm7(<<X1:1,Y7:7, X2:2,Y6:6, X3:3,Y5:5, X4:4,Y4:4,
	      X5:5,Y3:3, X6:6,Y2:2, X7:7,Y1:1,  More/binary>>) ->
    <<0:1,Y7:7,
      0:1,Y6:6,X1:1,
      0:1,Y5:5,X2:2,
      0:1,Y4:4,X3:3,
      0:1,Y3:3,X4:4,
      0:1,Y2:2,X5:5,
      0:1,Y1:1,X6:6,
      0:1,X7:7,
      (decode_gsm7(More))/binary>>;
decode_gsm7(<<>>) ->
    <<>>;
decode_gsm7(More) when byte_size(More) > 0, 
		       byte_size(More) < 7 ->
    Pad = 7 - byte_size(More), %% 6..1
    decode_gsm7(<<More/binary, 0:Pad/unit:8>>).
    


%% encode gsm7 bit encoding
encode_gsm7(<<0:1,Y7:7,      0:1,Y6:6,X1:1, 0:1,Y5:5,X2:2, 0:1,Y4:4,X3:3,
	      0:1,Y3:3,X4:4, 0:1,Y2:2,X5:5, 0:1,Y1:1,X6:6, 0:1,     X7:7,
	      More/binary >>) ->
    <<X1:1,Y7:7, X2:2,Y6:6, X3:3,Y5:5, X4:4,Y4:4,
      X5:5,Y3:3, X6:6,Y2:2, X7:7,Y1:1,
      (encode_gsm7(More))/binary >>;
encode_gsm7(<<>>) ->
    <<0>>;  %% extra bits to compensate for UDH padding.
encode_gsm7(More) when byte_size(More) > 0,
		       byte_size(More) < 8 ->
    Pad = 8 - byte_size(More),  %% 7..1
    encode_gsm7(<<More/binary, 0:Pad/unit:8>>).

%% prepend N number of E's to a list
prepend(N,E,List) when is_list(List) ->
    prepend_list(N,E,List);
prepend(N,E,Binary) when is_binary(Binary) ->
    <<E:N/unit:8, Binary/binary>>.

prepend_list(0,_,Acc) -> Acc;
prepend_list(I,E,Acc) -> prepend_list(I-1,E,[E|Acc]).

    

is_gsms_record(P) ->
    case P of
	#gsms_addr{} -> true;
	#gsms_submit_pdu{} -> true;
	#gsms_deliver_pdu{} -> true;
	_ -> false
    end.

separator(D,S) ->
    case is_gsms_record(D) of
	true -> "";
	false -> S
    end.

indent(I) ->
    lists:duplicate(I, $\s).

dump(P) ->
    dump_json(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Format values and records in JSON format
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dump_json(P) ->
    io:format("~s\n", [fmt_json(P)]).

fmt_json(P) ->
    case is_gsms_record(P) of
	true  -> fmt_json_record(0,P,"\n");
	false -> fmt_json_value(0,P,"\n")
    end.

fmt_json_record(I,P=#gsms_addr{},N)  ->
    fmt_json_record(I,P,record_info(fields,gsms_addr),N);
fmt_json_record(I,P=#gsms_submit_pdu{},N)  -> 
    fmt_json_record(I,P,record_info(fields,gsms_submit_pdu),N);
fmt_json_record(I,P=#gsms_deliver_pdu{},N) -> 
    fmt_json_record(I,P,record_info(fields,gsms_deliver_pdu),N).

fmt_json_record(I,P,Fs,N) ->
    [R|Ds] = tuple_to_list(P),
    [ "{",N,
      fmt_json_fields(I+2, R, [struct|Fs], [R|Ds], N),
      N, indent(I), "}" ].

fmt_json_fields(I,R,[F],[D],N) ->
    [ [indent(I),fmt_json_field(I,R,F,D,N) ] ];
fmt_json_fields(I,R,[F|Fs],[D|Ds],N) ->
    [ [indent(I),fmt_json_field(I,R,F,D,N),",",N] |
      fmt_json_fields(I,R,Fs,Ds,N)];
fmt_json_fields(_I,_R,[],[],_N) ->
    [].

fmt_json_field(I,R,F,D,N) ->
    Fk = atom_to_list(F),
    Dk = fmt_json_value(I,R,F,D,N),
    [Fk,": ",Dk].

fmt_json_value(I,D,N) ->
    fmt_json_value(I,undefined,undefined,D,N).

fmt_json_value(I,R,F,D,N) ->
    if  
	is_boolean(D) -> [atom_to_list(D)];
	is_integer(D) -> [integer_to_list(D)];
	is_atom(D)    -> [?Q,atom_to_list(D),?Q];
	is_binary(D), R =:= ipv4, (F =:= src orelse F =:= dst) ->
	    <<X1,X2,X3,X4>> = D,
	    [?Q,io_lib:format("~w.~w.~w.~w", [X1,X2,X3,X4]),?Q];
	is_binary(D) ->		 		     
	    io_lib:format("~p", [D]);
	is_tuple(D) ->
	    case is_gsms_record(D) of
		true ->
		    fmt_json_record(I+2,D,N);
		false ->
		    io_lib:format("~p", [D])
	    end;
	is_list(D) ->
	    try iolist_size(D) of
		_Sz -> [?Q,D,?Q]
	    catch
		error:_ ->  %% formt as JSON array?
		    fmt_json_array(D)
	    end
    end.

fmt_json_array(Ds) ->
    ["[", fmt_json_elems(Ds), "]"].

fmt_json_elems([D]) ->
    fmt_json_value(0,D,"");
fmt_json_elems([D|Ds]) ->
    [fmt_json_value(0,D,""),"," | fmt_json_elems(Ds)];
fmt_json_elems([]) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% YANG FORMAT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dump_yang(P) ->
    io:format("~s\n", [fmt_yang(P)]).

fmt_yang(P) ->
    case is_gsms_record(P) of
	true  -> fmt_yang_record(0,P,"\n");
	false -> fmt_yang_value(0,P,"\n")
    end.

fmt_yang_record(I,P=#gsms_addr{},N)  ->
    fmt_yang_record(I,P,record_info(fields,gsms_addr),N);
fmt_yang_record(I,P=#gsms_submit_pdu{},N)  -> 
    fmt_yang_record(I,P,record_info(fields,gsms_submit_pdu),N);
fmt_yang_record(I,P=#gsms_deliver_pdu{},N) -> 
    fmt_yang_record(I,P,record_info(fields,gsms_deliver_pdu),N).

fmt_yang_record(I,P,Fs,N) ->
    [R|Ds] = tuple_to_list(P),
    [ atom_to_list(R), " {", N,
      fmt_yang_fields(I+2, R, Fs, Ds, N),
      indent(I), "}" ].

fmt_yang_fields(I,R,[F|Fs],[D|Ds],N) ->
    [ [indent(I),fmt_yang_field(I,R,F,D,N),separator(D,";"),N] |
      fmt_yang_fields(I,R,Fs,Ds,N)];
fmt_yang_fields(_I,_R,[],[],_N) ->
    [].

fmt_yang_field(I,R,F,D,N) ->
    Fk = atom_to_list(F),
    Dk = fmt_yang_value(I,R,F,D,N),
    [Fk," ",Dk].

fmt_yang_value(I,D,N) ->
    fmt_yang_value(I,undefined,undefined,D,N).

fmt_yang_value(I,R,F,D,N) ->
    if  
	is_boolean(D) -> [atom_to_list(D)];
	is_integer(D) -> [integer_to_list(D)];
	is_atom(D)    -> [atom_to_list(D)];
	is_binary(D), R =:= ipv4, (F =:= src orelse F =:= dst) ->
	    <<X1,X2,X3,X4>> = D,
	    [?Q,io_lib:format("~w.~w.~w.~w", [X1,X2,X3,X4]),?Q];
	is_binary(D) -> %% fixme!
	    [?Q,io_lib:format("~p", [D]),?Q];
	is_tuple(D) ->
	    case is_gsms_record(D) of
		true ->
		    fmt_yang_record(I+2,D,N);
		false ->
		    io_lib:format("~p", [D])
	    end;
	is_list(D) ->
	    try iolist_size(D) of
		_Sz -> [?Q,D,?Q]
	    catch
		error:_ ->  %% formt as YANG array?
		    fmt_yang_array(D)
	    end
    end.

fmt_yang_array(Ds) ->
    fmt_yang_elems(Ds).

fmt_yang_elems([D]) ->
    fmt_yang_value(0,D,"");
fmt_yang_elems([D|Ds]) ->
    [fmt_yang_value(0,D,"")," " | fmt_yang_elems(Ds)];
fmt_yang_elems([]) -> [].
