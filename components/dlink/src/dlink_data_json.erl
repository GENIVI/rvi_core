-module(dlink_data_json).

-export([encode/2,
	 decode/3]).
-export([init/1,
	 port_options/0]).


init(_Opts) ->
    [].

port_options() ->
    [list, {packet, 0}].

decode(Msg, F, St) when is_function(F, 1) ->
    jsx_decode_stream(Msg, F, St).

encode(Msg, St) ->
    {ok, rvi_common:term_to_json(Msg), St}.

jsx_decode_stream(Data, F, St) ->
    case jsx_decode(Data, St) of
        {incomplete, Cont} ->
	    {ok, Cont};
        {with_tail, Elems, <<>>} ->
	    F(Elems),
	    {ok, []};
        {with_tail, Elems, Rest} ->
	    F(Elems),
	    jsx_decode_stream(Rest, F, [])
    end.

jsx_decode(Data, []) ->
    jsx:decode(Data, [stream, return_tail]);
jsx_decode(Data, Cont) when is_function(Cont, 1) ->
    Cont(Data).
