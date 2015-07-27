-module(gsms_lib).

-export([get_opt/2,
	 get_opt/3]).

get_opt(K, Opts) when is_atom(K) ->
    case lists:keyfind(K, 1, Opts) of
	false  -> erlang:error({mandatory, K});
	{_, V} -> V
    end;
get_opt({K, Def}, Opts) ->
    get_opt(K, Opts, Def).

get_opt(K, Opts, Def) ->
    case lists:keyfind(K, 1, Opts) of
	false when is_function(Def, 0) ->
	    Def();
	false when Def == '$mandatory' ->
	    error({mandatory, K});
	false ->
	    Def;
	{_, V} ->
	    V
    end.

