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
    ok.


stop(_State) ->
    ok.
