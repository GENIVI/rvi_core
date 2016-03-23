-module(dlink_data).

-export([decode/5,
	 encode/3]).

-include_lib("lager/include/log.hrl").

decode(Data, F, St, Mod, FragOpts)  when is_function(F,1) ->
    DecodeRes = case St of
		    <<>> when Data == <<>> ->
			{ok, <<>>};
		    <<>> -> do_decode(Data);
		    Rest when is_binary(Rest) ->
			do_decode(<<Rest/binary, Data/binary>>);
		    Cont when is_function(Cont, 1) ->
			Cont(Data)
		end,
    case DecodeRes of
	Cont1 when is_function(Cont1, 1) ->
	    {ok, Cont1};
	{ok, Rest1} ->
	    {ok, Rest1};
	{ok, Decoded, Rest1} ->
	    decoded(Decoded, Rest1, F, Mod, FragOpts);
	{error, _} = Err ->
	    Err
    end.

encode(Msg, PMod, PSt) ->
    PMod:encode(Msg, PSt).

do_decode(Data) ->
    case Data of
	<<8:4,_:4,_/binary>> ->
	    %% msgpack map
	    ?debug("detected msgpack map", []),
	    msgpack_decode(Data);
	<<H, _/binary>> when H==16#de; H==16#df ->
	    %% msgpack map 16 or map 32
	    ?debug("detected msgpack map 16 or map 32", []),
	    msgpack_decode(Data);
	_ ->
	    ?debug("assuming json", []),
	    jsx_decode(Data)
    end.

decoded(Decoded, Rest, F, Mod, FragOpts) ->
    case rvi_frag:maybe_fragment(Decoded, Mod, FragOpts) of
	true ->
	    decode(Rest, F, <<>>, Mod, FragOpts);
	{true, Msg} ->
	    case do_decode(Msg) of
		{ok, DecMsg, <<>>} ->
		    F(DecMsg),
		    decode(Rest, F, <<>>, Mod, FragOpts);
		{error, _} = Err1 ->
		    Err1
	    end;
	false ->
	    F(Decoded),
	    decode(Rest, F, <<>>, Mod, FragOpts)
    end.

msgpack_decode(Data) ->
    case msgpack:unpack_stream(Data, [jsx]) of
	{error, incomplete} ->
	    fun(NewData) ->
		    msgpack_decode(
		      <<Data/binary, NewData/binary>>)
	    end;
	{error, E} ->
	    {error, E};
	{Decoded, Rest} when is_binary(Rest) ->
	    {ok, Decoded, Rest}
    end.

jsx_decode(Data) ->
    try jsx_decode_res(jsx:decode(Data, [stream, return_tail]))
    catch
	error:E ->
	    ?error("jsx decode failed: ~p", [E]),
	    {error, E}
    end.

jsx_decode_res(Res) ->
    case Res of
	{incomplete, Cont} ->
	    fun(NewData) ->
		    jsx_decode_res(Cont(NewData))
	    end;
	{with_tail, Decoded, Rest} ->
	    {ok, Decoded, Rest}
    end.
