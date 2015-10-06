-module(rvi_log).

-behaviour(gen_server).

-export([start_link/0,
	 log/3,
	 fetch/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(st, {n = 100}).
-record(evt, {id,
	      component,
	      event}).
-define(TIDS, rvi_log_tids).
-define(EVENTS, rvi_log_events).

start_link() ->
    create_tabs(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log(TID, Component, Event) ->
    gen_server:cast(?MODULE, {log, TID, Component, Event}).

fetch(Tid) ->
    ets:select(?EVENTS, [{#evt{id = {Tid,'$1'},
			       component = '$2',
			       event = '$3'}, [], [{{Tid, '$1', '$2', '$3'}}] }]).

init(_) ->
    {ok, #st{}}.


handle_cast({log, TID, Component, Event}, #st{n = N} = St) ->
    do_log(TID, Component, Event, N),
    {noreply, St};
handle_cast(_, St) ->
    {noreply, St}.

handle_call(_, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_info(_, St) ->
    {noreply, St}.

terminate(_, _) ->
    ok.

code_change(_, St, _) ->
    {ok, St}.


%% ======================================================================
%% Local functions
%% ======================================================================

create_tabs() ->
    maybe_new(?TIDS, [ordered_set, named_table]),
    maybe_new(?EVENTS, [ordered_set, named_table]).

maybe_new(T, Opts) ->
    case ets:info(T, name) of
	undefined ->
	    ets:new(T, Opts);
	_ ->
	    true
    end.

do_log(Tid, Component, Event, N) ->
    case ets:member(?TIDS, Tid) of
	true ->
	    store_event(Tid, Component, Event);
	false ->
	    case ets:info(?TIDS, size) of
		Sz when Sz >= N ->
		    purge_tid();
		_ ->
		    ok
	    end,
	    store_event(Tid, Component, Event)
    end.

store_event(Tid, Component, Event) ->
    ets:insert(?EVENTS, #evt{id = {Tid, seq()},
			     component = Component,
			     event = Event}).

purge_tid() ->
    case ets:first(?TIDS) of
	'$end_of_table' ->
	    %% Should not be possible ...
	    ok;
	Tid ->
	    ets:delete(?TIDS, Tid),
	    ets:match_delete(?EVENTS, #evt{id = {Tid,'_'}, _ = '_'})
    end.

seq() ->
    erlang:now().
