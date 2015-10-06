%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    URL parsing
%%% @end
%%% Created : 16 Dec 2011 by Tony Rogvall <tony@rogvall.se>

-module(exo_url).

-include("../include/exo_url.hrl").

-export([parse/1, parse/2, format/1, format_path/1]).
-export([parse_path/2]).

%% returns a #url{}
parse(Str) ->
    parse(Str, strict).

parse(Str, Strict) ->
    case str(Str) of
	"http://" ++ Rest ->
	    parse_host(Strict, #url{scheme = http}, Rest, []);
	"HTTP://" ++ Rest ->
	    parse_host(Strict, #url{scheme = http}, Rest, []);
	"https://" ++ Rest ->
	    parse_host(Strict, #url{scheme = https}, Rest, []);
	"HTTPS://" ++ Rest ->
	    parse_host(Strict, #url{scheme = https}, Rest, []);
	"ftp://" ++ Rest ->
	    parse_host(Strict, #url{scheme = ftp}, Rest, []);
	"file://" ++ Rest ->
	    parse_host(Strict, #url{scheme = file}, Rest, []);
	_ when Strict == sloppy ->
	    parse_host(Strict, #url{scheme = http}, Str, [])
    end.

str(S) when is_binary(S) ->
    binary_to_list(S);
str(S) when is_list(S) ->
    S.

parse_host(Strict, U, Str, Ack) ->
    case Str of
	[] ->
	    U#url{host = lists:reverse(Ack),
		  path = "/"
		 };
	[$/|Tail] ->
	    U2 = U#url{host = lists:reverse(Ack)},
	    parse_path(Strict, U2, Tail,"/");
	[$:|T] ->
	    U2 = U#url{host = lists:reverse(Ack)},
	    parse_port(Strict, U2, T,[]);
	[H|T] ->
	    parse_host(Strict, U, T, [H|Ack])
    end.

parse_port(Strict, U, Str, Ack) ->
    case Str of
	[] ->
	    U#url{port = list_to_integer(lists:reverse(Ack)),
		  path = "/"};
	[$/|T] ->
	    U2 = U#url{port = list_to_integer(lists:reverse(Ack))},
	    parse_path(Strict, U2, T,"/");
	[H|T] ->
	    parse_port(Strict, U,T,[H|Ack])
    end.

parse_path(U, Str) ->
    parse_path(false,U,Str,[]).

parse_path(Strict, U, Str, Ack) ->
    case Str of
	[] ->
	    U#url{path = lists:reverse(Ack)};
	[$?|T] ->
	    U#url{path = lists:reverse(Ack), querypart = T};
	[H|T] ->
	    parse_path(Strict, U, T, [H|Ack])
    end.


format(Url) when is_record(Url, url) ->
    if Url#url.scheme == undefined ->
	    format_path(Url);
       true ->
	    [
	     atom_to_list(Url#url.scheme), "://",
	     Url#url.host,
	     if
		 Url#url.port == undefined ->
		     [];
		 true  ->
		     [$: | integer_to_list(Url#url.port)]
	     end,
	     format_path(Url)
	    ]
     end.

format_path(Url) when is_record(Url, url) ->
    [
     Url#url.path,
     if
	 Url#url.querypart == [] ->
	     [];
	 true ->
	     [$?|Url#url.querypart]
     end].
