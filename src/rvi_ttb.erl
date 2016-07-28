%% Stolen with pride from https://github.com/uwiger/locks
-module(rvi_ttb).
-behaviour(tr_ttb).

-export([on_nodes/2,
	 stop/0,
	 stop_nofetch/0,
	 format/2]).

-export([patterns/0,
	 flags/0]).

-export([event/1]).

-include_lib("trace_runner/include/trace_runner.hrl").

%% This function is also traced. Can be used to insert markers in the trace
%% log.
event(E) ->
    event(?LINE, E, none).

event(_, _, _) ->
    ok.

on_nodes(Ns, File) ->
    tr_ttb:on_nodes(Ns, File, ?MODULE).

patterns() ->
    [{authorize_rpc        , event, 3, []},
     {service_edge_rpc     , event, 3, []},
     {service_discovery_rpc, event, 3, []},
     {dlink_tcp_rpc        , event, 3, []},
     {connection           , event, 3, []},
     {dlink_tls_rpc        , event, 3, []},
     {dlink_tls_conn       , event, 3, []},
     {dlink_bt_rpc         , event, 3, []},
     {bt_connection        , event, 3, []},
     {dlink_sms_rpc        , event, 3, []},
     {schedule_rpc         , event, 3, []},
     {proto_json_rpc       , event, 3, []},
     {proto_msgpack_rpc    , event, 3, []},
     {rvi_common           , event, 3, []},
     {?MODULE              , event, 3, []}
     | tr_ttb:default_patterns()].

flags() ->
    {all, call}.

stop() ->
    tr_ttb:stop().

stop_nofetch() ->
    tr_ttb:stop_nofetch().

format(Dir, Out) ->
    tr_ttb:format(Dir, Out).
