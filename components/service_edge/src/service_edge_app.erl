%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(service_edge_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 start_phase/3,
	 stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    service_edge_sup:start_link().

start_phase(json_rpc, _, _) ->
    service_edge_rpc:start_json_server(),
    ok;

start_phase(websocket, _, _) ->
    service_edge_rpc:start_websocket(),
    ok.

stop(_State) ->
    ok.
