%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(schedule_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 start_phase/3,
	 stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    schedule_sup:start_link().

start_phase(init, _, _) ->
    schedule_rpc:init(),
    ok;


start_phase(json_rpc, _, _) ->
    schedule_rpc:start_json_server(),
    ok.

stop(_State) ->
    ok.
