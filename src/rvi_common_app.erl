-module(rvi_common_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 start_phase/3,
	 stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rvi_common_sup:start_link().

start_phase(_, _, _) ->
    ok.

stop(_State) ->
    ok.
