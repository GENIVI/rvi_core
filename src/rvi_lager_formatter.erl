-module(rvi_lager_formatter).

-export([format/2,
	 format/3]).


format(Msg, Config) ->
    format(Msg, Config, []).

format(Msg, Config, Colors) ->
    C = get_config(lager_msg:severity(Msg), Config),
    lager_default_formatter:format(Msg, C, Colors).

get_config(Severity, Config) ->
    case lists:keyfind(Severity, 1, Config) of
	{_, Res} ->
	    Res;
	_ ->
	    []
    end.
