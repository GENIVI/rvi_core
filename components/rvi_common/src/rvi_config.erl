%%
%% Copyright (C) 2014, Jaguar Land Rover
%%p
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(rvi_config).

-include_lib("lager/include/log.hrl").


-export([setup_substitution/1]).
-export([substitute/2]).
-define(BOOT_PARAMS, "/proc/cmdline").

	

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
	    ?warning("Could not retrieve $file field from: ~p: ~p", 
		     [ FieldAndDefaultAndRemainder, Reason ]),
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
	    ?warning("Could not retrieve $env field from: ~p: ~p", 
		     [ FieldAndDefaultAndRemainder, Reason ]),
	    { lists:reverse("ERR:"++atom_to_list(Reason)), Rem };

	{ ok, FieldName, Default, Rem } ->
	    case os:getenv(FieldName) of
		false ->
		    {lists:reverse(Default), Rem};

		Val ->
		    {lists:reverse(Val), Rem}
	    end
    end.


%% Found it
lookup_boot_arg_([$r,$o,$o,$t,$=,$U,$U,$I,$D,$= | UUID]) ->
    UUID;

lookup_boot_arg_(_) ->
    false.


retrieve_uuid_([]) ->
    not_found;

%% /proc/cmdline has format:
%% BOOT_IMAGE=/boot/vmlinuz-3.13.0-37-generic \
%% root=UUID=afc0a6d8-0264-4f8a-bb3e-51ff8655b51c ro quiet splash

retrieve_uuid_([Arg | RemArg]) ->
    case lookup_boot_arg_(Arg) of 
	false ->
	    retrieve_uuid_(RemArg);

	UUID -> UUID
    end.
		      
substitute_uuid(Remain) ->
    %% Get the default value
    { ok, _, Default, Rem } = get_default([], Remain, []),

    case file:open(?BOOT_PARAMS, [ read ]) of
	{ error, Err1 } ->
	    ?warning("Could not open $uuid boot param file: ~p: ~p",
		     [ ?BOOT_PARAMS, Err1 ]),
	    { lists:reverse(Default), Rem };

	{ok, IOD } ->
	    case file:read_line(IOD) of
		{ ok, Data } ->
		    file:close(IOD),
		    %% Reverse and drop trailing newline
		    Line = lists:delete($\n, Data),

		    %% Tokenize boot params
		    ArgList = string:tokens(Line, " "),

		    %% Retrieve root=UUUID=[Value] 
		    case retrieve_uuid_(ArgList) of
			not_found ->
			    ?warning("Could not retrieve root=UUID=_Val_ from cmdline: ~p",
				     [ Line ]),

			    { lists:reverse(Default), Rem } ;

			UUID ->
			    { lists:reverse(UUID), Rem} 
		    end;
		
		{error, Err2 } ->
		    ?warning("Could not read opened $uuid boot param file: ~p: ~p",
			     [ ?BOOT_PARAMS, Err2 ]),
		    file:close(IOD),

		    { lists:reverse(default), Rem }
				
	    end
    end.

substitute({K,V}, _Acc)  ->
    { K, substitute(V, []) };


substitute([ $$, $r, $v, $i, $_, $u, $u, $i, $d, $( | Rem], Acc) ->
    %% Retreive boot uid

    { Val, R } = substitute_uuid(Rem),
    substitute(R,  Val ++ Acc);

substitute([ $$, $r, $v, $i, $_, $f, $i, $l, $e, $( | Rem], Acc) ->
    { Val, R } = substitute_file(Rem),
    substitute(R,  Val ++ Acc);

substitute([ $$, $r, $v, $i, $_, $e, $n, $v, $(| Rem], Acc)  ->
    { Val, R } = substitute_env(Rem),
    substitute(R,  Val ++ Acc);

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

setup_substitution(App) ->
    Env = application:get_all_env(App),
    Subst = substitute(Env,[]),
    case is_list(Subst) of
	false -> ok;
	true ->
	    lists:map(fun({K, V}) ->
			      application:set_env(App, K, V)
		      end, Subst),
	    ok
    end,
    ok.


    
