-module(rvi_log).

-behaviour(gen_server).

-export([start_link/0,
	 log/3,
	 flog/4,
	 new_id/1,
	 fetch/1,
	 timestamp/0,
	 format/2
	]).

-export([start_json_server/0,
	 handle_rpc/2,
	 handle_notification/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include_lib("lager/include/log.hrl").

-record(st, {n = 100,
	     seq = 1,
	     node_tag}).
-record(evt, {id,
	      component,
	      event}).
-define(IDS, rvi_log_ids).
-define(EVENTS, rvi_log_events).

-define(MAX_LENGTH, 60).


start_link() ->
    create_tabs(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log(TID, Component, Event) when is_binary(TID), is_binary(Component) ->
    gen_server:cast(?MODULE, {log, TID, timestamp(), Component, bin(Event)}).

new_id(Prefix) ->
    gen_server:call(?MODULE, {new_id, bin(Prefix)}).

format(Fmt, Args) ->
    try format_(Fmt, Args)
    catch
	error:_ ->
	    format_("FORMAT ERROR ~p, ~p", [Fmt, Args])
    end.

format_(Fmt, Args) ->
    trunc_msg(iolist_to_binary(io_lib:fwrite(Fmt, Args))).

trunc_msg(Bin) when byte_size(Bin) =< ?MAX_LENGTH ->
    Bin;
trunc_msg(Bin) ->
    binary:part(Bin, 0, ?MAX_LENGTH).


flog(Fmt, Args, Component, CS) ->
    LogTID = rvi_common:get_log_id(CS),
    log(LogTID, Component, format(Fmt, Args)).

timestamp() ->
    os:timestamp().

fetch(Tids) ->
    TidSet = select_ids(Tids),
    [{Tid, ets:select(?EVENTS, [{#evt{id = {Tid,'$1'},
				      component = '$2',
				      event = '$3'}, [], [{{'$1', '$2', '$3'}}] }])}
     || Tid <- TidSet].

init(_) ->
    {ok, Tag} = rvi_common:get_module_config(
		  rvi_common, rvi_log, node_tag, base64url:encode(crypto:rand_bytes(3)),
		  rvi_common:get_component_specification()),
    gen_server:cast(self(), log_start),
    {ok, #st{node_tag = bin(Tag)}}.

start_json_server() ->
    ?debug("rvi_log:start_json_server()", []),
    case rvi_common:start_json_rpc_server(rvi_common,
					  ?MODULE,
					  rvi_common_sup) of
	ok -> ok;
	Err ->
	    ?warning("rvi_log:start_json_server(): Failed to start: ~p", [Err]),
	    Err
    end.

handle_rpc(<<"log">>, Args) ->
    handle_notification(<<"log">>, Args),
    {ok, [{status, rvi_common:json_rpc_status(ok)}]};
handle_rpc(<<"fetch">>, Args) ->
    TIDs = get_json_ids(Args),
    Res = [{TID, fetch(TID)} || TID <- TIDs],
    {ok, [{status, rvi_common:json_rpc_status(ok)},
	  {<<"log">>, format_result(Res)}]};
handle_rpc(Other, _Args) ->
    ?warning("rvi_log:handle_rpc(~p): unknown command", [ Other ]),
    {ok, [{status, rvi_common:json_rpc_status(invalid_command)}]}.

handle_notification(<<"log">>, Args) ->
    {ok, TID} = rvi_common:get_json_element([<<"tid">>], Args),
    {ok, Component} = rvi_common:get_json_element([<<"cmp">>], Args),
    {ok, Event} = rvi_common:get_json_element([<<"evt">>], Args),
    log(TID, Component, Event),
    ok;
handle_notification(Other, _Args) ->
    ?warning("rvi_log:handle_notification(~p): unknown command", [Other]),
    ok.

handle_cast(log_start, #st{n = N, node_tag = Tag} = St) ->
    do_log(tid_(<<"rvi_log">>, 0, Tag), timestamp(), <<"rvi_common">>,
	   format("Started - Tag = ~s", [Tag]), N),
    {noreply, St};
handle_cast({log, TID, TS, Component, Event}, #st{n = N} = St) ->
    do_log(TID, TS, Component, Event, N),
    {noreply, St};
handle_cast(_, St) ->
    {noreply, St}.

handle_call({new_id, Prefix}, _From, #st{seq = Seq, node_tag = Tag} = S) ->
    {reply, <<Prefix/binary, ":", (integer_to_binary(Seq))/binary, "-", Tag/binary>>,
     S#st{seq = Seq + 1}};
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
    maybe_new(?IDS, [ordered_set, public, named_table]),
    maybe_new(?EVENTS, [ordered_set, public, named_table, {keypos, #evt.id}]).

maybe_new(T, Opts) ->
    case ets:info(T, name) of
	undefined ->
	    ets:new(T, Opts);
	_ ->
	    true
    end.

tid_(Prefix, Seq, Tag) ->
    <<Prefix/binary, ":", (integer_to_binary(Seq))/binary, "-", Tag/binary>>.

do_log(Tid, TS, Component, Event, N) ->
    case ets:member(?IDS, Tid) of
	true ->
	    store_event(Tid, TS, Component, Event);
	false ->
	    case ets:info(?IDS, size) of
		Sz when Sz >= N ->
		    purge_id();
		_ ->
		    ok
	    end,
	    ets:insert(?IDS, {Tid}),
	    store_event(Tid, TS, Component, Event)
    end.

store_event(Tid, TS, Component, Event) ->
    rvi_log_log:info("~-20s ~-12s ~s", [Tid, Component, Event]),
    ?info("RVI_LOG: ~p/~p/~p", [Tid, Component, Event]),
    ets:insert(?EVENTS, #evt{id = {Tid, TS},
			     component = Component,
			     event = Event}).

purge_id() ->
    case ets:first(?IDS) of
	'$end_of_table' ->
	    %% Should not be possible ...
	    ok;
	{Tid} ->
	    ets:delete(?IDS, Tid),
	    ets:match_delete(?EVENTS, #evt{id = {Tid,'_'}, _ = '_'})
    end.

get_json_ids(Args) ->
    Res = case rvi_common:get_json_element([<<"tid">>], Args) of
	      {ok, TID} when is_binary(TID) ->
		  [TID];
	      {ok, TIDs} when is_list(TIDs) ->
		  TIDs;
	      {error, _} ->
		  []
	  end,
    lists:filter(fun valid_id_pat/1, Res).

valid_id_pat(TP) ->
    %% We'll accept anything that isn't blatantly *invalid*
    try begin _ = re:run([], TP, []),
	      true
	end
    catch
	_:_ ->
	    false
    end.

select_ids(TIDs) ->
    ets:foldr(
      fun({Tid}, Acc) ->
	      case match_id(Tid, TIDs) of
		  true -> [Tid|Acc];
		  false -> Acc
	      end
      end, [], ?IDS).

match_id(Tid, [Pat|Pats]) ->
    case re:run(Tid, Pat, []) of
	{match, _} -> true;
	nomatch -> match_id(Tid, Pats)
    end;
match_id(_, []) ->
    false.

format_result(Log) ->
    [{TID, format_events(Es)} || {TID, Es} <- Log].

format_events([{TS, Comp, Evt}|Es]) ->
    [[{<<"ts">>, rvi_common:utc_timestamp(TS)},
      {<<"cmp">>, bin(Comp)},
      {<<"evt">>, bin(Evt)}] || format_events(Es)];
format_events([]) ->
    [].

bin(A) when is_atom(A)   -> atom_to_binary(A, latin1);
bin(B) when is_binary(B) -> B;
bin(L) when is_list(L)   -> iolist_to_binary(L);
bin(Other) ->
    iolist_to_binary(io_lib:fwrite("~w", [Other])).
