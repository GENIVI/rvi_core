-module(rvi_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 start_phase/3,
	 stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rvi_sup:start_link().

start_phase(ping, _, _) ->
%%    exoport:ping(),
    ok.

stop(_State) ->
    ok.
