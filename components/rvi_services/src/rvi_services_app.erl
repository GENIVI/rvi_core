%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%
%% Copyright (C) 2016, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(rvi_services_app).

-export([start/2,
	 start_phase/3,
	 stop/1]).

-include_lib("lager/include/log.hrl").

start(_StartType, _StartArgs) ->
    rvi_services_sup:start_link().

start_phase(register_services, _, _) ->
    ?debug("start_phase: register_services", []),
    rvi_services:register_services();
start_phase(announce, _, _) ->
    ?debug("start_phase: announce", []),
    rvi_common:announce({n, l, rvi_services}).

stop(_State) ->
    ok.
