-module(service_discovery_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 start_phase/3,
	 stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    service_discovery_sup:start_link().

start_phase(init, _, _) ->
    service_discovery_rpc:init(),
    ok.

stop(_State) ->
    ok.
