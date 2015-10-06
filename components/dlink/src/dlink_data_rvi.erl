-module(dlink_data_rvi).

-compile(export_all).

-record(dlink_data_rvi, {need, buf}).

-define(MAX_LINE, 79).

init(_Opts) ->
    undefined.

port_options() ->
    [].

encode(Elems, St) ->
    Bin = encode_(Elems, <<>>),
    Sz = byte_size(Bin),
    {ok, <<"&RVI|",
	   (integer_to_binary(Sz, 16))/binary, "\n",
	   Bin/binary>>, St}.

encode_([{Key, Val}|T], Acc) ->
    {Type, ValBin} = encode_val(Val),
    Bin = encode_elem(to_bin(Key), Type, ValBin),
    encode_(T, <<Acc/binary, Bin/binary>>);
encode_([], Acc) ->
    Acc.

encode_val(V) when is_binary(V)  -> {$B, V};
encode_val(V) when is_integer(V) -> {$i, integer_to_binary(V,16)};
encode_val(V) when is_atom(V)    -> {$a, atom_to_binary(V, latin1)};
encode_val(V) when is_float(V) ->
    Bin = <<V/float>>,
    {$f, Bin};
encode_val({T,_} = J) when T==array; T==struct ->
    JSON = exo_json:encode(J),
    {$J, iolist_to_binary(JSON)};
encode_val([T|_] = L) when is_tuple(T) ->
    {$L, encode_(L, <<>>)}.

decode_value($B, Bin) -> Bin;
decode_value($i, Bin) -> binary_to_integer(Bin, 16);
decode_value($f, <<F/float>>) -> F;
decode_value($a, Bin) -> binary_to_existing_atom(Bin, latin1);
decode_value($J, Bin) ->
    {ok, Obj} = exo_json:decode_string(binary_to_list(Bin)),
    Obj;
decode_value($L, Bin) ->
    decode_packet(Bin).

encode_elem(Key, Type, Bin) ->
    BSz = byte_size(Bin),
    case byte_size(Key) + BSz of
	Sz when Sz =< 78, Type >= $a, Type =< $z ->
	    <<Key/binary, "|", Type, ":", Bin/binary, "\n">>;
	_ ->
	    <<Key/binary, "|", Type, "|",
	      (integer_to_binary(BSz+1,16))/binary, "\n",
	      Bin/binary, "\n">>
    end.

decode(<<"&RVI|", Rest/binary>>, undefined) ->
    case erlang:decode_packet(line, Rest, [{line_length, 79}]) of
	{more, _} ->
	    {more, Rest};
	{ok, Ln, Rest1} ->
	    LSz = byte_size(Ln),
	    LSz1 = LSz-1,
	    <<Size:LSz1/binary, "\n">> = Ln,
	    Bytes = binary_to_integer(Size, 16),
	    case Rest1 of
		<<Pkt:Bytes/binary, Tail/binary>> ->
		    {ok, decode_packet(Pkt), Tail};
		_ ->
		    {more, #dlink_data_rvi{need = Bytes, buf = Rest1}}
	    end
    end;
decode(Data, #dlink_data_rvi{need = Bytes, buf = Buf} = St) ->
    case <<Buf/binary, Data/binary>> of
	<<Pkt:Bytes/binary, Tail/binary>> ->
	    {ok, decode_packet(Pkt), Tail};
	Buf1 ->
	    {more, St#dlink_data_rvi{buf = Buf1}}
    end;
decode(_, _St) ->
    {error, unknown, undefined}.

decode_packet(<<>>) ->
    [];
decode_packet(P) ->
    {ok, L, Rest} = erlang:decode_packet(line, P, [{line_length, ?MAX_LINE}]),
    case split_line(L) of
	{Key, Type, simple, Data} ->
	    [{Key, decode_value(Type, Data)}|decode_packet(Rest)];
	{Key, Type, Size} ->
	    Size1 = Size-1,
	    <<VBin:Size1/binary, "\n", Rest1/binary>> = Rest,
	    [{Key, decode_value(Type, VBin)}|decode_packet(Rest1)]
    end.

to_bin(V) when is_atom(V)   -> atom_to_binary(V, latin1);
to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_list(V)   -> iolist_to_binary(V).

split_line(L) ->
    split_line(L, <<>>).

split_line(<<"\\", $|, Rest/binary>>, Acc) ->
    split_line(Rest, <<Acc/binary, $|>>);
split_line(<<"|", T, ":", Rest/binary>>, Acc) ->
    {Acc, T, simple, remove_nl(Rest)};
split_line(<<"|", T, "|", Rest/binary>>, Acc) ->
    SzBin = remove_nl(Rest),
    {Acc, T, binary_to_integer(SzBin, 16)};
split_line(<<H, T/binary>>, Acc) ->
    split_line(T, <<Acc/binary, H>>).


remove_nl(B) ->
    Sz = byte_size(B),
    Sz1 = Sz-1,
    <<V:Sz1/binary, "\n">> = B,
    V.
