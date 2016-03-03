%%% @author tony <tony@rogvall.se>
%%% @copyright (C) 2013, tony
%%% @doc
%%%     Generate netlink decoder & header files
%%% @end
%%% Created : 29 May 2013 by tony <tony@rogvall.se>

-module(netlink_gen).

-compile(export_all).

-define(INFILE, "netlink.inc").
-define(ERL_MOD, "netl_codec").

-define(INT_SIZE_32,  true).
-define(LONG_SIZE_32, true).

-ifdef(INT_SIZE_32).
-define(int_t,  int32_t).
-define(uint_t, uint32_t).
-else.
-ifdef(INT_SIZE_64).
-define(int_t,  int64_t).
-define(uint_t, uint64_t).
-endif.
-endif.

-ifdef(LONG_SIZE_32).
-define(long_t,  int32_t).
-define(ulong_t, uint32_t).
-else.
-ifdef(LONG_SIZE_64).
-define(long_t,  int64_t).
-define(ulong_t, uint64_t).
-endif.
-endif.

-define(short_t,  int16_t).
-define(ushort_t, uint16_t).

-define(char_t,  int8_t).
-define(uchar_t, uint8_t).


-record(gen,
	{
	  error = 0 :: integer(), %% number of errors detected
	  defs      :: term(),    %% Symbol -> Value
	  enums     :: term(),    %% Name->[{atom(),integer()}]}
	  attrs     :: term(),    %% Name->[{atom(),integer(),type()}]
	  recs      :: term()     %% Name->[{atom(),integer(),type()}]
	}).

start() ->
    G0 = #gen { defs  = dict:new(),
		enums = dict:new(),
		attrs = dict:new(),
		recs  = dict:new() },
    {ok,Fd} = file:open(?INFILE, [read]),
    try fold_file_terms(fun load_term/3, G0, Fd) of
	{ok,G1} -> 
	    ok = emit_hrl(G1),
	    ok = emit_erl(G1);
	Error -> Error
    after
	file:close(Fd)
    end.

load_term({define,Name,Value},Ln,G) ->
    Value1 = lookup_value(Value, G),
    D = case dict:find(Name, G#gen.defs) of
	    {ok,Value1} ->
		G#gen.defs;
	    {ok,_Value2} -> 
		io:format("~s:~w: warning ~s redefined\n", 
			  [?INFILE,Ln,Name]),
		dict:store(Name,Value1,G#gen.defs);
	    error ->
		dict:store(Name,Value1,G#gen.defs)
	end,
    G#gen { defs = D };
load_term({enum,Name,Enums}, Ln, G) when is_atom(Name), is_list(Enums) ->
    case dict:find(Name, G#gen.enums) of
	{ok,_Enums} ->
	    io:format("~s:~w: error ~s allready defined\n", 
		      [?INFILE,Ln,Name]),
	    G1 = inc_error(G),
	    load_enums(Name,Enums,Ln,G1);
	error ->
	    load_enums(Name,Enums,Ln,G)
    end;
load_term({attribute,Name,Attrs}, Ln, G) when is_atom(Name), is_list(Attrs) ->
    case dict:find(Name, G#gen.attrs) of
	{ok,_Attrs} ->
	    io:format("~s:~w: error ~s allready defined\n", 
		      [?INFILE,Ln,Name]),
	    G1 = inc_error(G),
	    load_attrs(Name,Attrs,Ln,G1);
	error ->
	    load_attrs(Name,Attrs,Ln,G)
    end;
load_term({record,Name,Fields}, Ln, G) when is_atom(Name), is_list(Fields) ->
    case dict:find(Name, G#gen.recs) of
	{ok,_Attrs} ->
	    io:format("~s:~w: error ~s allready defined\n", 
		      [?INFILE,Ln,Name]),
	    G1 = inc_error(G),
	    load_fields(Name,Fields,Ln,G1);
	error ->
	    load_fields(Name,Fields,Ln,G)
    end;
load_term({record,Name,Type}, Ln, G) when is_atom(Name), is_atom(Type) ->
    case dict:find(Type, G#gen.recs) of
	error ->
	    io:format("~s:~w: error ~s not defined\n", 
		      [?INFILE,Ln,Type]),
	    G1 = inc_error(G),
	    G2 = load_fields(Type,[],Ln,G1),
	    load_fields(Name,Type,Ln,G2);
	{ok,_} ->
	    case dict:find(Name, G#gen.recs) of
		{ok,_Attrs} ->
		    io:format("~s:~w: error ~s allready defined\n", 
			      [?INFILE,Ln,Name]),
		    G1 = inc_error(G),
		    load_fields(Name,Type,Ln,G1);
		error ->
		    load_fields(Name,Type,Ln,G)
	    end
    end;
load_term(Other, Ln, G) ->
    io:format("~s:~w: error badly formed term ~p\n", [?INFILE,Ln,Other]),
    inc_error(G).


%%
%% Load enums, enumerate warn about multiple usage of names and values! 
%%
load_enums(Name,Enums0,Ln,G) ->
    Enums = enumerate(Enums0,G),
    G1 = check_enumeration(Name,Enums,Ln,G),
    D = dict:store(Name, Enums, G1#gen.enums),
    G1#gen { enums = D}.

check_enumeration(Name,[{E,V}|Es],Ln,G) ->
    case lists:keyfind(E,1,Es) of
	false ->
	    case lists:keyfind(V,2,Es) of
		false ->
		    check_enumeration(Name,Es,Ln,G);
		{E1,V} ->
		    io:format("~s:~w: value for ~w also used by ~w\n",
			      [?INFILE,Ln,E,E1]),
		    G1 = inc_error(G),
		    check_enumeration(Name,Es,Ln,G1)
	    end;
	{E,_V1} ->
	    io:format("~s:~w: enum ~s multiply defined\n", 
		      [?INFILE,Ln,E]),
	    G1 = inc_error(G),
	    check_enumeration(Name,Es,Ln,G1)
    end;
check_enumeration(_Name, [], _Ln,G) ->    
    G.

%% generate uniq values for names not assigne values
enumerate(Enums,G) ->
    Enums1 = lists:map(fun({E,V}) -> {E,lookup_value(V,G)};
			  (E) -> E
		       end, Enums),
    Vs0 = lists:foldl(fun({_E,V},Acc) -> [V|Acc];
			 (_E,Acc) -> Acc
		      end, [], Enums1),
    Vs1 = lists:usort(Vs0),    
    enumerate(Enums1, 0, Vs1).

enumerate([{E,V}|Es], I, Vs) when is_atom(E) ->
    [{E,V}|enumerate(Es, I, Vs)];
enumerate(Es0=[E|Es], I, Vs) when is_atom(E) ->
    case lists:member(I,Vs) of
	true -> enumerate(Es0,I+1,Vs);
	false -> [{E,I}|enumerate(Es,I+1,Vs)]
    end;
enumerate([],_I,_Vs) ->
    [].

%%
%% Load record fields
%%
load_fields(Name,Type,_Ln,G) when is_atom(Type) ->
    D = dict:store(Name, Type, G#gen.recs),
    G#gen { recs = D };
load_fields(Name,Fields,Ln,G) ->
    Fields1 = [{F,I,T} || 
		  {I,{F,T}} <- 
		      lists:zip(lists:seq(1,length(Fields)),Fields)],
    G1 = check_fields(Name,Fields1,Ln,G),
    D = dict:store(Name, Fields1, G1#gen.recs),
    G1#gen { recs = D }.

check_fields(Name,[{'_',_I,T}|Fs],Ln,G) ->
    G1 = check_type({record,Name,'_'}, T, Ln, G),
    check_fields(Name, Fs, Ln, G1);
check_fields(Name,[{F,_I,T}|Fs],Ln,G) ->
    case lists:keymember(F,1,Fs) of
	true ->    
	    io:format("~s:~w: field ~s multiply defined\n",
		      [?INFILE,Ln,F]),
	    G1 = inc_error(G),
	    G2 = check_type({record,Name,F}, T, Ln, G1),
	    check_fields(Name, Fs, Ln, G2);
	false ->
	    G1 = check_type({record,Name,F}, T, Ln, G),
	    check_fields(Name, Fs, Ln, G1)
    end;
check_fields(_Name,[],_Ln,G) ->
    G.
%%
%% Load attribute fields
%%
load_attrs(Name,Attrs,Ln,G) ->
    Attrs1 = [{A,I,T} || 
		 {I,{A,T}} <- lists:zip(lists:seq(0,length(Attrs)-1),Attrs)],
    G1 = check_attrs(Name,Attrs1,Ln,G),
    D = dict:store(Name, Attrs1, G1#gen.attrs),
    G1#gen { attrs = D }.

check_attrs(Name,[{A,_I,T}|As],Ln,G) ->
    case lists:keymember(A,1,As) of
	true ->
	    io:format("~s:~w: attribute ~s multiply defined\n",
		      [?INFILE,Ln,A]),
	    G1 = inc_error(G),
	    G2 = check_type({attribute,Name,A}, T, Ln, G1),
	    check_attrs(Name, As, Ln, G2);
	false ->
	    G1 = check_type({attribute,Name,A}, T, Ln, G),
	    check_attrs(Name, As, Ln, G1)
    end;
check_attrs(_Name,[],_Ln,G) ->
    G.


emit_hrl(G) ->
    {ok,Fd} = file:open(?ERL_MOD++".hrl", [write]),
    try
	dict:fold(
	  fun(K,V,_A) ->
		  io:format(Fd, "-define(~s, ~w).\n", [K, V])
	  end, ok, G#gen.defs),
	dict:fold(
	  fun(K,V,_A) ->
		  Fields = lookup_fields(V, G),
		  Fs = list_to_tuple([N || {N,_I,_Type} <- Fields, N =/= '_']),
		  io:format(Fd, "-record(~s, ~w).\n", [K, Fs])
	  end, ok, G#gen.recs)
    of
	R -> R
    after
	file:close(Fd)
    end.


emit_erl(G) ->
    {ok,Fd} = file:open(?ERL_MOD++".erl", [write]),
    io:format(Fd, "-module(~s).\n", [?ERL_MOD]),
    io:format(Fd, "-include(~p).\n", [?ERL_MOD++".hrl"]),
    try emit_codec_(Fd, G) of
	ok -> ok
    after
	file:close(Fd)
    end.

emit_codec_(Fd, G) ->
    dict:fold(
      fun(Name,_Tab,_) ->
	      io:format(Fd, "-export([dec_~s/1, enc_~s/1]).\n", 
			[Name,Name])
      end, ok, G#gen.enums),
    dict:fold(
      fun(Name,_Tab,_) ->
	      io:format(Fd, "-export([dec_~s/1, enc_~s/1]).\n", 
			[Name,Name])
      end, ok, G#gen.attrs),
    dict:fold(
      fun(Name,_Tab,_) ->
	      io:format(Fd, "-export([dec_~s/1, enc_~s/1]).\n", 
			[Name,Name])
      end, ok, G#gen.recs),


    dict:fold(
      fun(Name,Tab,_) ->
	      %% decode
	      lists:foreach(
		fun({Enum,Value}) ->
			io:format(Fd,"dec_~s(~w) -> ~s;\n", [Name,Value,Enum])
		end, Tab),
	      io:format(Fd,"dec_~s(V) -> V.\n", [Name]),
	      %% encode
	      lists:foreach(
		fun({Enum,Value}) ->
			io:format(Fd,"enc_~s(~s) -> ~w;\n", [Name,Enum,Value])
		end, Tab),
	      io:format(Fd,"enc_~s(V) when is_integer(V) -> V;\n", [Name]),
	      io:format(Fd,"enc_~s(E) -> erlang:error({undefined,E}).\n", 
			[Name])
      end, ok, G#gen.enums),
    
    dict:fold(
      fun(Name,Tab0,_) ->
	      %% extend addr_t => ipv4_addr_t | ipv6_addr_t
	      Tab = lists:foldr(
		      fun({A,I,addr_t},Acc) -> 
			      [{A,I,ipv4_addr_t},{A,I,ipv6_addr_t} | Acc];
			 (E,Acc) -> [E|Acc]
		      end, [], Tab0),
	      lists:foreach(
		fun({Attr,Index,Type}) ->
			{M1,R1} = match_code(Type,native,"X",G),
			io:format(Fd,
				  "dec_~s({~w,native,<<~s>>}) ->\n"
				  "  {~s,~s};\n", 
				  [Name,Index,M1,Attr,R1]),
			{M2,R2} = match_code(Type,big,"X",G),
			io:format(Fd,
				  "dec_~s({~w,big,<<~s>>}) ->\n"
				  "  {~s,~s};\n", 
				  [Name,Index,M2,Attr,R2])
		end, Tab),
	      io:format(Fd,"dec_~s({I,_Endian,Bin}) -> {I,Bin}.\n", [Name]),

	      lists:foreach(
		fun({Attr,Index,Type}) ->
			{M1,C1} = gen_code(Type,native,"X",G),
			io:format(Fd,"enc_~s({~s,native,~s}) ->\n"
				  "  {~w,native,<<~s>>};\n", 
				  [Name,Attr,M1,Index,C1]),
			{M2,C2} = gen_code(Type,big,"X",G),
			io:format(Fd,"enc_~s({~s,big,~s}) ->\n"
				  "  {~w,big,<<~s>>};\n", 
				  [Name,Attr,M2,Index,C2])
		end, Tab),
	      io:format(Fd,"enc_~s({I,Endian,X}) -> {I,Endian,X}.\n", [Name])
      end, ok, G#gen.attrs),

    dict:fold(
      fun(Name,RecType,_) ->
	      Fields = lookup_fields(RecType, G),
	      Named = [{Fi,Ix,Ti} || {Fi,Ix,Ti} <- Fields, Fi =/= '_'],
	      {Ms,Rs} = match_record_code(Fields, native, "X", G),
	      M = io_list_join(Ms, ","),
	      Rs1 = lists:map(fun({{Fi,_,_Ti},Ri}) ->
				      [atom_to_list(Fi),"=",Ri]
			      end, lists:zip(Named,Rs)),
	      R1 = ["#",atom_to_list(Name),"{", io_list_join(Rs1,","), "}"],
	      
	      io:format(Fd,"dec_~s({native,<<~s>>}) ->\n"
			"   ~s.\n", 
			[Name,M,R1]),

	      {Ts,Bs} = gen_record_code(Fields, native, "X", G),
	      Ts1 = lists:map(fun({{Fi,_,_Ti},Ri}) ->
				      [atom_to_list(Fi),"=",Ri]
			      end, lists:zip(Named,Ts)),
	      R2 = ["#",atom_to_list(Name),"{", io_list_join(Ts1,","), "}"],
	      B = io_list_join(Bs, ","),
	      io:format(Fd,"enc_~s({_Endian,~s}) -> <<~s>>.\n", 
			[Name,R2,B]),
	      ok
      end, ok, G#gen.recs).


check_type(Ctx, {array,T}, Ln, G) ->
    case is_base_type(T) of
	true -> 
	    G;
	false ->
	    io:format("~s:~w: type ~w not allowed as base type in ~w\n", 
		      [?INFILE,Ln,T,Ctx]),
	    inc_error(G)
    end;
check_type(_Ctx, {enum,Type,Enum}, Ln, G) 
  when is_atom(Type), is_atom(Enum) ->
    case is_integer_type(Type) of
	true ->
	    case dict:find(Enum, G#gen.enums) of
		{ok,_} -> G;
		error ->
		    io:format("~s:~w: enum type ~s not defined\n", 
			      [?INFILE,Ln,Enum]),
		    inc_error(G)
	    end;
	false ->
	    io:format("~s:~w: enum must have an integer base type\n", 
		      [?INFILE,Ln]),
	    inc_error(G)
    end;
check_type(_Ctx, {flags,Type,Enum}, Ln, G) 
  when is_atom(Type), is_atom(Enum) ->
    case is_unsigned_type(Type) of
	true ->
	    case dict:find(Enum, G#gen.enums) of
		{ok,_} -> G;
		error ->
		    io:format("~s:~w: enum type ~s not defined\n", 
			      [?INFILE,Ln,Enum]),
		    inc_error(G)
	    end;
	false ->
	    io:format("~s:~w: flags must have an unsigned base type\n", 
		      [?INFILE,Ln]),
	    inc_error(G)
    end;
check_type(Ctx, {tlvs,Name}, Ln, G) ->
    %% fixme, check that Ctx is {attribute,...}
    case dict:find(Name, G#gen.attrs) of
	error ->
	    io:format("~s:~w: attribute type ~s name not found in ~w\n", 
		      [?INFILE,Ln,Name,Ctx]),
	    inc_error(G);
	{ok,_} ->
	    G
    end;
check_type(Ctx, T, Ln, G) ->
    case is_base_type(T) of
	true -> G;
	false ->
	    case T of
		string_t -> G;
		binary_t -> G;
		Type when is_atom(Type) ->
		    case lookup_type(Type, G) of
			false ->
			    io:format("~s:~w: type ~s name not found in ~w\n", 
				      [?INFILE,Ln,Type,Ctx]),
			    inc_error(G);
			_Typedef ->
			    G
		    end;
		_ ->
		    io:format("~s:~w: type ~w not allowed in ~w\n", 
			      [?INFILE,Ln,T,Ctx]),
		    inc_error(G)
	    end
    end.

is_base_type(T) ->
    case is_integer_type(T) of
	true -> true;
	false ->
	    case T of
		addr_t -> true;
		ether_addr_t -> true;
		ipv4_addr_t -> true;
		ipv6_addr_t -> true;
		_ -> false
	    end
    end.

is_integer_type(T) ->
    is_unsigned_type(T) orelse is_signed_type(T).

is_unsigned_type(T) ->
    case T of
	ulong_t   -> true;
	ushort_t  -> true;
	uchar_t   -> true;
	uint_t    -> true;
	uint8_t   -> true;
	uint16_t  -> true;
	uint32_t  -> true;
	uint64_t  -> true;
	_  -> false
    end.

is_signed_type(T) ->
    case T of
	long_t   -> true;
	int_t    -> true;
	short_t  -> true;
	char_t   -> true;
	int8_t   -> true;
	int16_t  -> true;
	int32_t  -> true;
	int64_t  -> true;
	_  -> false
    end.
%%
%% generate match code 
%% {BitMatch, TermCode}
%% 
match_code(int_t, E, X, G) -> match_code(?int_t, E, X, G);
match_code(uint_t, E, X, G) -> match_code(?uint_t, E, X, G);
match_code(long_t, E, X, G) -> match_code(?long_t, E, X, G);
match_code(ulong_t, E, X, G) -> match_code(?ulong_t, E, X, G);
match_code(short_t, E, X, G) -> match_code(?short_t, E, X, G);
match_code(ushort_t, E, X, G) -> match_code(?ushort_t, E, X, G);
match_code(char_t, E, X, G) -> match_code(?char_t, E, X, G);
match_code(uchar_t, E, X, G) -> match_code(?uchar_t, E, X, G);

match_code(uint64_t,native,X,_G) -> match_int_code(X,64,unsigned,native);
match_code(uint32_t,native,X,_G) -> match_int_code(X,32,unsigned,native);
match_code(uint16_t,native,X,_G) -> match_int_code(X,16,unsigned,native);
match_code(uint8_t,native,X,_G)  -> match_int_code(X,8,unsigned,native);
match_code(uint64_t,big,X,_G)    -> match_int_code(X,64,unsigned,big);
match_code(uint32_t,big,X,_G)    -> match_int_code(X,32,unsigned,big);
match_code(uint16_t,big,X,_G)    -> match_int_code(X,16,unsigned,big);
match_code(uint8_t,big,X,_G)    -> match_int_code(X,8,unsigned,big);
match_code(uint64_t,little,X,_G) -> match_int_code(X,64,unsigned,little);
match_code(uint32_t,little,X,_G) -> match_int_code(X,32,unsigned,little);
match_code(uint16_t,little,X,_G) -> match_int_code(X,16,unsigned,little);
match_code(uint8_t,little,X,_G) -> match_int_code(X,8,unsigned,little);

match_code(int64_t,native,X,_G)  -> match_int_code(X,64,signed,native);
match_code(int32_t,native,X,_G)  -> match_int_code(X,32,signed,native);
match_code(int16_t,native,X,_G)  -> match_int_code(X,16,signed,native);
match_code(int8_t,native,X,_G)   -> match_int_code(X,8,signed,native);
match_code(int64_t,big,X,_G)     -> match_int_code(X,64,signed,big);
match_code(int32_t,big,X,_G)     -> match_int_code(X,32,signed,big);
match_code(int16_t,big,X,_G)     -> match_int_code(X,16,signed,big);
match_code(int8_t, big,X,_G)     -> match_int_code(X,8,signed,big);
match_code(int64_t,little,X,_G)  -> match_int_code(X,64,signed,little);
match_code(int32_t,little,X,_G)  -> match_int_code(X,32,signed,little);
match_code(int16_t,little,X,_G)  -> match_int_code(X,16,signed,little);
match_code(int8_t,little,X,_G)  -> match_int_code(X,8,signed,little);
match_code(binary_t,_Endian,X,_G) -> 
    {[X,"/binary"],X};
match_code(string_t,_Endian,X,_G) -> 
    {[X,"/binary"], ["binary_to_list(hd(binary:split(",X,",","<<0>>)))"]};

match_code({enum,BaseType,Name},Endian,X,G) ->
    Xe = X++"e",
    Decode = "dec_"++atom_to_list(Name),
    {Match,Value} = match_code(BaseType,Endian,Xe,G),
    {Match,[Decode,"(",Value,")"]};
match_code({flags,BaseType,Name},Endian,X,G) ->
    Xf = X++"f",
    Decode = "dec_"++atom_to_list(Name),
    {Match,Value} = match_code(BaseType,Endian,Xf,G),
    {Match,["netlink_codec:decode_flags(",Value,",fun ",Decode,"/1)"]};
match_code({array,BaseType},Endian,X,G) ->
    %% [Xi || <<Xi:32/unsigned-big>> <= X ]
    Xi = X++"i",
    %% Well, Xi must be equal to Value here!
    {Match,Value} = match_code(BaseType,Endian,Xi,G),
    {[X,"/binary"], ["[",Value," || <<",Match,">> <= ",X,"]"] };
match_code({tlvs,Name},_Endian,X,_G) ->
    Xi = X++"i",
    Decode = "dec_"++atom_to_list(Name),
    {[X,"/binary"],
     ["[",Decode,"(",Xi,") || ",
      Xi," <- netlink_codec:decode_tlv_list(",X,")]"]};
match_code(ether_addr_t, _, X,_G) ->
    Xs = [X++integer_to_list(I) || I <- lists:seq(1,6)],
    {string:join(Xs,","), ["{", string:join(Xs,","), "}"]};
match_code(ipv4_addr_t, _, X,_G) ->
    Xi = [X++integer_to_list(I) || I <- lists:seq(1,4)],
    Xm = [J++":8" || J <- Xi],
    {string:join(Xm,","),
     ["{", string:join(Xi,","), "}"]};
match_code(ipv6_addr_t,_,X,_G) ->
    Xi = [X++integer_to_list(I) || I <- lists:seq(1,8)],
    Xm = [J++":16" || J <- Xi],
    {string:join(Xm,","),
     ["{", string:join(Xi,","), "}"]};
%% match_code(addr_t,_,X,_G) ->
%%    Xi = [X++integer_to_list(I) || I <- lists:seq(1,4)],
%%    Xm = [J++":8" || J <- Xi],
%%    {string:join(Xm,","),
%%     ["{", string:join(Xi,","), "}"]};
match_code(Name,Endian,X,G) ->
    case dict:find(Name, G#gen.recs) of
	error ->
	    %% assume attribute!
	    {ok,_} = dict:find(Name, G#gen.attrs),
	    Decode = "dec_"++atom_to_list(Name),
	    {[X,"/binary"], [Decode,"(netlink_codec:decode_tlv(",X,"))"]};
	{ok,RecType} ->
	    Fields = lookup_fields(RecType,G),
	    Named = [{Fi,Ix,Ti} || {Fi,Ix,Ti} <- Fields, Fi =/= '_'],
	    {Ms,Rs} = match_record_code(Fields, Endian, X, G),
	    M = io_list_join(Ms, ","),
	    Rs1 = lists:map(fun({{Fi,_,_Ti},Ri}) ->
				    [atom_to_list(Fi),"=",Ri]
			    end, lists:zip(Named,Rs)),
	    R = ["#",atom_to_list(Name),"{", io_list_join(Rs1,","), "}"],
	    {M, R}
    end.

match_record_code(Fs, Endian, X, G) ->
    match_record_code_(Fs, Endian, X, G, [], []).

match_record_code_([{'_',_Ix,Type}|Fs], Endian, X, G, Ms, Rs) ->
    {M,_R} = match_code(Type, Endian, "_", G),
    match_record_code_(Fs, Endian, X, G, [M|Ms], Rs);
match_record_code_([{_Field,Ix,Type}|Fs], Endian, X, G, Ms, Rs) ->
    Xi = X++integer_to_list(Ix),
    {M,R} = match_code(Type, Endian, Xi, G),
    match_record_code_(Fs, Endian, X, G, [M|Ms], [R|Rs]);
match_record_code_([], _Endian, _X, _G, Ms, Rs) ->
    {lists:reverse(Ms), lists:reverse(Rs)}.

%%
%%  X:Size/<sign>-<endian>  =>  Decode
%% {BitMatch,Decode}
%%
match_int_code(X,Size,Sign,Endian) ->
    {[X,":",integer_to_list(Size),"/",
      atom_to_list(Sign),"-",atom_to_list(Endian)], X}.

%%
%% generate  generate-code
%% return {Encode, BitConstruct}
%%
gen_code(int_t, E, X, G) -> gen_code(?int_t, E, X, G);
gen_code(uint_t, E, X, G) -> gen_code(?uint_t, E, X, G);
gen_code(long_t, E, X, G) -> gen_code(?long_t, E, X, G);
gen_code(ulong_t, E, X, G) -> gen_code(?ulong_t, E, X, G);
gen_code(short_t, E, X, G) -> gen_code(?short_t, E, X, G);
gen_code(ushort_t, E, X, G) -> gen_code(?ushort_t, E, X, G);
gen_code(char_t, E, X, G) -> gen_code(?char_t, E, X, G);
gen_code(uchar_t, E, X, G) -> gen_code(?uchar_t, E, X, G);

gen_code(uint64_t,native,X,_G) -> gen_int_code(X,64,unsigned,native);
gen_code(uint32_t,native,X,_G) -> gen_int_code(X,32,unsigned,native);
gen_code(uint16_t,native,X,_G) -> gen_int_code(X,16,unsigned,native);
gen_code(uint8_t,native,X,_G)  -> gen_int_code(X,8,unsigned,native);
gen_code(uint64_t,big,X,_G)    -> gen_int_code(X,64,unsigned,big);
gen_code(uint32_t,big,X,_G)    -> gen_int_code(X,32,unsigned,big);
gen_code(uint16_t,big,X,_G)    -> gen_int_code(X,16,unsigned,big);
gen_code(uint8_t,big,X,_G)    -> gen_int_code(X,8,unsigned,big);
gen_code(uint64_t,little,X,_G) -> gen_int_code(X,64,unsigned,little);
gen_code(uint32_t,little,X,_G) -> gen_int_code(X,32,unsigned,little);
gen_code(uint16_t,little,X,_G) -> gen_int_code(X,16,unsigned,little);
gen_code(uint8_t,little,X,_G) -> gen_int_code(X,8,unsigned,little);

gen_code(int64_t,native,X,_G) -> gen_int_code(X,64,signed,native);
gen_code(int32_t,native,X,_G) -> gen_int_code(X,32,signed,native);
gen_code(int16_t,native,X,_G) -> gen_int_code(X,16,signed,native);
gen_code(int8_t,native,X,_G)  -> gen_int_code(X,8,signed,native);
gen_code(int64_t,big,X,_G)    -> gen_int_code(X,64,signed,big);
gen_code(int32_t,big,X,_G)    -> gen_int_code(X,32,signed,big);
gen_code(int16_t,big,X,_G)    -> gen_int_code(X,16,signed,big);
gen_code(int8_t,big,X,_G)     -> gen_int_code(X,8,signed,big);
gen_code(int64_t,little,X,_G) -> gen_int_code(X,64,signed,little);
gen_code(int32_t,little,X,_G) -> gen_int_code(X,32,signed,little);
gen_code(int16_t,little,X,_G) -> gen_int_code(X,16,signed,little);
gen_code(int8_t,little,X,_G)  -> gen_int_code(X,8,signed,little);

gen_code(binary_t, _Endian, X,_G) -> 
    {X, X};
gen_code(string_t, _Endian, X,_G) -> 
    {X, ["(erlang:iolist_to_binary([",X,",0","]))/binary"]};

gen_code({enum,BaseType,Name},Endian,X,G) ->
    Encode = "enc_"++atom_to_list(Name),
    E = ["(",Encode,"(",X,"))"],
    {_M,C} = gen_code(BaseType,Endian,E,G),
    {X,C};

gen_code({flags,BaseType,Name},Endian,X,G) ->
    Encode = "enc_"++atom_to_list(Name),
    E = ["(","netlink_codec:encode_flags(",X,",fun ",Encode,"/1))"],
    {_M,C} = gen_code(BaseType,Endian,E,G),
    {X,C};

gen_code({array,BaseType},Endian,X,G) ->
    Xi = X++"i",
    {_M,C} = gen_code(BaseType,Endian,Xi,G),  %% M=Xi! only simple base types!
    {X, ["(<< <<",C,">>", " || ",Xi," <- ",X,">>)/binary"]};

gen_code({tlvs,Name},_Endian,X,_G) ->
    Xi = X++"i",
    Encode = "enc_"++atom_to_list(Name),
    {X, [ "(netlink_codec:encode_tlv_list(",
	  "[",Encode,"(",Xi,") || ", Xi," <- ",X,"]", "))/binary"]};

gen_code(ether_addr_t,_,X,_G) ->
    Xi = [X++integer_to_list(I) || I <- lists:seq(1,6)],
    Xm = [J++":8" || J <- Xi],
    {["{", string:join(Xi,","),"}"], string:join(Xm,",") };
gen_code(ipv4_addr_t,_,X,_G) ->
    Xi = [X++integer_to_list(I) || I <- lists:seq(1,4)],
    Xm = [J++":8" || J <- Xi],
    {["{", string:join(Xi,","),"}"], string:join(Xm,",")};
gen_code(ipv6_addr_t,_,X,_G) ->
    Xi = [X++integer_to_list(I) || I <- lists:seq(1,8)],
    Xm = [J++":16" || J <- Xi],
    {["{", string:join(Xi,","), "}"], string:join(Xm,",")};
%% gen_code(addr_t,_,X,_G) ->
%%    %% FIXME: use address family from message ?
%%    Xi = [X++integer_to_list(I) || I <- lists:seq(1,4)],
%%    Xm = [J++":8" || J <- Xi],
%%    {["{", string:join(Xi,","),"}"], string:join(Xm,",")};
gen_code(Name,Endian,X,G) ->
    case dict:find(Name, G#gen.recs) of
	error ->
	    %% assume attribute!
	    Encode = "enc_"++atom_to_list(Name),
	    {X, [ "(netlink_codec:encode_tlv(",Encode,"(",X,")))"]};
	{ok,RecType} ->
	    Fields = lookup_fields(RecType,G),
	    Named = [{Fi,Ix,Ti} || {Fi,Ix,Ti} <- Fields, Fi =/= '_'],
	    %% must be record
	    {Ts,Bs} = gen_record_code(Fields, Endian, X, G),
	    Ts1 = lists:map(fun({{Fi,_,_Ti},Ri}) ->
				    [atom_to_list(Fi),"=",Ri]
			    end, lists:zip(Named,Ts)),
	    R = ["#",atom_to_list(Name),"{", io_list_join(Ts1,","), "}"],
	    B = io_list_join(Bs, ","),
	    {R, B}
    end.



gen_record_code(Fs, Endian, X, G) ->
    gen_record_code_(Fs, Endian, X, G, [], []).

gen_record_code_([{'_',_Ix,Type}|Fs], Endian, X, G, Ms, Bs) ->
    {_M,B} = gen_code(Type, Endian, "0", G),
    gen_record_code_(Fs, Endian, X, G, Ms, [B|Bs]);
gen_record_code_([{_Field,Ix,Type}|Fs], Endian, X, G, Ms, Bs) ->
    Xi = X++integer_to_list(Ix),
    {M,B} = gen_code(Type, Endian, Xi, G),
    gen_record_code_(Fs, Endian, X, G, [M|Ms], [B|Bs]);
gen_record_code_([], _Endian, _X, _G, Ms, Bs) ->
    {lists:reverse(Ms), lists:reverse(Bs)}.

%% return {TermMatch,BitConstruct}    
gen_int_code(X,Size,Sign,Endian) ->
    {X, [X,":",integer_to_list(Size),"/",
	 atom_to_list(Sign),"-",atom_to_list(Endian)]}.

%%
%% io_list_join like string:join but for io_lists 
%%
io_list_join([], _Sep) -> [];
io_list_join([A], _Sep) -> [A];
io_list_join([A|As], Sep) -> [A,Sep|io_list_join(As,Sep)].

%%
%% Get record indirections
%%
lookup_fields(Name, G) when is_atom(Name) ->    
    case dict:find(Name, G#gen.recs) of
	{ok,Name1} ->
	    lookup_fields(Name1, G)
    end;
lookup_fields(Fields,_G) when is_list(Fields) ->
    Fields.

%%
%% Lookup defined value 
%%

lookup_value(Value, _G) when is_integer(Value) ->    
    Value;
lookup_value(Name, G) when is_atom(Name) ->
    case dict:find(Name, G#gen.defs) of
	{ok,Name} -> Name;  %% recursive!
	{ok,Value} when is_atom(Value) -> lookup_value(Value,G);
	{ok,Value} when is_integer(Value) -> Value;
	error -> Name
    end.

%% lookup named type (record / attribute)
lookup_type(Name, G) when is_atom(Name) ->
    case dict:find(Name, G#gen.attrs) of
	error ->
	    case dict:find(Name, G#gen.recs) of
		error ->
		    false;
		{ok,Value} -> {attribute,Name,Value}
	    end;
	{ok,Value} -> {record,Name,Value}
    end.


inc_error(G) ->
    G#gen { error = G#gen.error + 1}.


%% util to fold all terms in a file
fold_file_terms(Fun, Acc, Fd) ->
    fold_file_terms(Fun, Acc, 1, Fd).
    
fold_file_terms(Fun, Acc, Line, Fd) ->
    case io:read(Fd, '', Line) of
	{ok,Term,EndLine} ->
	    fold_file_terms(Fun, Fun(Term,Line,Acc), EndLine,Fd);
	{error,Error,Line} ->
	    io:format("parse error:~w: ~p\n", [Line,Error]),
	    {error,Error};
	{eof,_Line} ->
	    {ok,Acc}
    end.
    
