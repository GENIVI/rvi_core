%%
%% Copyright (C) 2014, Jaguar Land Rover
%%p
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(rvi_config).

%%-include_lib("lhttpc/include/lhttpc.hrl").
%%-include_lib("lager/include/log.hrl").

-export([substitute/2]).




%% We hit the end of the list before we saw a closing paranthesis.
get_default(_FieldName, [], Acc) ->
    { error, missing_end_parenthesis, lists:reverse(Acc)  };


%% We have an end paranthesis
get_default(Field, [$) | Remainder], Acc) ->
    { ok, Field, lists:reverse(Acc), Remainder };

get_default(Field, [C | DefaultRem], Acc) ->
    get_default(Field, DefaultRem, [ C  | Acc ]).


%% We hit the end of the list before we saw a default value or a
%% closing parenthesis.
get_field_and_default([], Acc) ->
    { error, missing_end_parenthesis, lists:reverse(Acc) };


%% We never saw a default value before we hit a closing paranthesis
%% value.
get_field_and_default([$) | Remain], Acc) ->
    { error, missing_default, lists:reverse(Acc)  ++ Remain };


%% We have a field name. Extract the default value
get_field_and_default([$, | Remain], Acc) ->
    get_default(lists:reverse(Acc), Remain, []);


%% Extract field name into Acc
get_field_and_default([C | FieldNameRem], Acc) ->
    get_field_and_default(FieldNameRem, [ C  | Acc ]).



%% Parse "field:default)remainder", which is a part of
%% something like "$file(Field:Default)Remainder.
%% Return
%% { ok, Field, Default, Remainder)
%% { error, Reason }
%%
get_field_and_default(FieldAndDefaultAndRemainder) ->
    get_field_and_default(FieldAndDefaultAndRemainder, []).
    

%% We have an end paranthesis
substitute_file(FieldAndDefaultAndRemainder) ->
    case get_field_and_default(FieldAndDefaultAndRemainder) of 
	{ error, Reason, Rem } ->
	    { lists:reverse("ERR:"++atom_to_list(Reason)), Rem };

	{ ok, FileName, Default, Rem } ->

	    case file:open(FileName, [ read ]) of
		{ error, _ } ->
		    { lists:reverse(Default), Rem };

		{ok, IOD } ->
		    case file:read_line(IOD) of
			{ ok, Data } ->
			    file:close(IOD),
			    %% Reverse and drop trailing newline
			    [ _ | Line ] = lists:reverse(Data),
			    { Line, Rem};

			{error, _ } ->
			    file:close(IOD),
			    { Default, Rem }
		    end

	    end
    end.


%% Substitute { key, "hell
substitute_env(FieldAndDefaultAndRemainder) ->
    case get_field_and_default(FieldAndDefaultAndRemainder) of 
	{ error, Reason, Rem } ->
	    { lists:reverse("ERR:"++atom_to_list(Reason)), Rem };

	{ ok, FieldName, Default, Rem } ->
	    case os:getenv(FieldName) of
		false ->
		    {lists:reverse(Default), Rem};

		Val ->
		    {lists:reverse(Val), Rem}
	    end
    end.


substitute({K,V}, _Acc)  ->
    { K, substitute(V, []) };


substitute([ $$, $r, $v, $i, $_, $f, $i, $l, $e, $( | Remainder], Acc) ->
    { Val, Rem } = substitute_file(Remainder),
    substitute(Rem,  Val ++ Acc);

substitute([ $$, $r, $v, $i, $_, $e, $n, $v, $(| Remainder], Acc)  ->
    { Val, Rem } = substitute_env(Remainder),
    substitute(Rem,  Val ++ Acc);

substitute([], Acc) ->
    lists:reverse(Acc);

substitute([ Val | Rem], Acc) when is_list(Val) ->
    substitute(Rem,  [substitute(Val, []) | Acc]);

substitute([ Val | Rem], Acc) ->
    substitute(Rem, [ substitute(Val, []) | Acc]);

substitute(Other, [])  ->
    Other;

substitute(Other, Acc)  ->
    [ Other | Acc ].

