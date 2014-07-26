-module(schedule_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10},
	   [
%%          ?CHILD(schedule_alarms, worker),
	    ?CHILD(schedule, worker)
%%	    ?CHILD(schedule_can, worker),
%%	    ?CHILD(schedule_waypoints, worker)
	    %% ?CHILD(schedule_gps, worker),
	    %% ?CHILD(exodmo_config, worker)
	   ]} }.

