-module(dlink_data_json).

-compile(export_all).

init(_Opts) ->
    [].

port_options() ->
    [list, {packet, 0}].

decode(Msg, St) ->
    {Msg1, St1} = append(St, Msg),
    try exo_json:decode(St1, Msg1) of
	{done, {ok, {struct, Elems}}, Rest} ->
	    {ok, [Elems], Rest};
	{done, {ok, {array, Structs}}, Rest} ->
	    {ok, [Str || {struct, Str} <- Structs], Rest};
	{done, {error, Reason}, Rest} ->
	    {error, Reason, Rest};
	{more, Cont} ->
	    {more, Cont}
    catch
	error:Error ->
	    {error, Error, St1};
	exit:Exit ->
	    {error, Exit, St1}
    end.

encode({struct, _} = JSON, St) ->
    try {ok, exo_json:encode(JSON), St}
    catch exit:Error -> erlang:error(Error)
    end;
encode({array, Structs} = JSON, St) ->
    case lists:all(fun({struct,_}) -> true;
		      (_) -> false
		   end, Structs) of
	true ->
	    {ok, exo_json:encode(JSON), St};
	false ->
	    erlang:error(invalid_json_structure)
    end.

append([], Msg) ->
    {Msg, []};
append([_|_] = St, Msg) ->
    {St ++ Msg, []};
append(Cont, Msg) when is_tuple(Cont) ->
    {Msg, Cont}.
