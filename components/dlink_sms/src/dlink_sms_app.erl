%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(dlink_sms_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 start_phase/3,
	 stop/1]).

-include_lib("lager/include/log.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    dlink_sms_sup:start_link().

start_phase(init, _, _) ->
    dlink_sms_rpc:init_rvi_component();

start_phase(json_rpc, _, _) ->
    dlink_sms_rpc:start_json_server(),
    ok;

start_phase(connection_manager, _, _) ->
    dlink_sms_rpc:start_connection_manager(),
    ok;

start_phase(announce, _, _) ->
    rvi_common:announce({n, l, dlink_sms}).

stop(_State) ->
    ok.
