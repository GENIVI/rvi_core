%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
-module(rvi_parse_ini).

-export([file/1]).


file(F) ->
    case file:read_file(F) of
        {ok, B} ->
            parse(B, F);
        {error, _} = Error ->
            Error
    end.

parse(B, F) ->
    Lines = [L || L <- re:split(B, "\\n", [{return,binary}, notempty]),
                  L =/= <<>>],  %% may still contain a trailing <<>>
    compact(group(Lines)).

compact(Gs) ->
    lists:foldl(
      fun({G,Vs}, Acc) ->
              case orddict:find(G, Acc) of
                  {ok, Vs0} -> orddict:store(G, Vs0 ++ Vs, Acc);
                  error     -> orddict:store(G, Vs, Acc)
              end
      end, orddict:new(), Gs).

group([H|T] = Ls) ->
    case heading(H) of
        false ->
            {G, T1} = collect(Ls, <<>>),
            [G | group(T1)];
        Head when is_binary(Head) ->
            {G, T1} = collect(T, Head),
            [G | group(T1)]
    end;
group([]) ->
    [].

heading(L) when is_binary(L) ->
    case re:run(L, "\\[\\h*([^\\h]+)\\h*\\]", [{capture,[1],binary}]) of
        {match, [H]} ->
            H;
        nomatch ->
            false
    end.

collect(Ls, G) ->
    collect(Ls, G, []).

collect([H|T] = Ls, G, Acc) ->
    case var(H) of
        {K, V} -> collect(T, G, [{K,V}|Acc]);
        false  -> {{G, lists:reverse(Acc)}, Ls}
    end;
collect([], G, Acc) ->
    {{G, lists:reverse(Acc)}, []}.

var(L) ->
    case re:run(L, "\\h*([^\\h]+)\\h*=\\h*(.*)", [{capture,[1,2],binary}]) of
        {match, [K, V]} ->
            {K, cmd(V)};
        _ ->
            false
    end.

cmd(V) ->
    Env = env(),
    case os:cmd(binary_to_list(<<Env/binary, "echo ", V/binary>>)) of
        "/bin/sh:" ++ _ ->
            V;
        Res ->
            re:replace(Res, "^(.*)\\n$", "\\1", [{return, binary}])
    end.

env() ->
    case code:priv_dir(rvi_core) of
        {error, _} ->
            <<>>;
        Priv ->
            iolist_to_binary(["PRIV=", Priv, "; "])
    end.
