-module(dlink_data_msgpack).

-compile(export_all).

-record(st, {opts = [{allow_atom, pack},
		     {enable_str, true},
		     jsx],
	     buf = <<>>}).

port_options() ->
    [binary, {packet, 0}].

init(_Opts) ->
    #st{}.

decode(Msg0, #st{buf = Prev, opts = Opts} = St) ->
    Msg = append(Prev, Msg0),
    case msgpack:unpack_stream(Msg, Opts) of
	{error, incomplete} ->
	    {more, St#st{buf = Msg}};
	{error, E} ->
	    {error, E, St};
	{Decoded, Rest} when is_binary(Rest) ->
	    {ok, Decoded, St#st{buf = Rest}}
    end.

encode({struct, Elems}, #st{opts = Opts} = St) ->
    {ok, msgpack:pack(to_jsx(Elems), Opts), St};
encode({array, Elems}, #st{opts = Opts} = St) ->
    {ok, msgpack:pack(to_jsx(Elems), Opts), St};
encode([{_,_}|_] = Data, #st{opts = Opts} = St) ->
    {ok, msgpack:pack(Data, Opts), St}.

append(<<>>, Msg) ->
    Msg;
append(Prev, Msg) ->
    <<Prev/binary, Msg/binary>>.

to_jsx({struct, Elems}) ->
    {[to_jsx(E) || E <- Elems]};
to_jsx({array, Elems}) ->
    [to_jsx(E) || E <- Elems];
to_jsx({K,V}) ->
    {K, to_jsx(V)};
to_jsx(X) ->
    X.
