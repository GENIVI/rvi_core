%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%
%% Copyright (C) 2014-15, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%
-module(dlink_app).
-behaviour(application).

-export([start/2,
	 start_phase/3,
	 stop/1]).

start(_StartType, _StartArgs) ->
    dlink_sup:start_link().

start_phase(announce, _, _) ->
    rvi_common:announce({n, l, dlink}).

stop(_State) ->
    ok.
