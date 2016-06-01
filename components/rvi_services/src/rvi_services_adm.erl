%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%
%% Copyright (C) 2016, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(rvi_services_adm).

-export([handle_rpc/2]).

-include_lib("lager/include/log.hrl").

handle_rpc(Request, Args) ->
    ?debug("handle_rpc(~p, ~p)", [Request, Args]),
    {ok, [{status, ok},
          {value, <<"foobar">>}]}.
