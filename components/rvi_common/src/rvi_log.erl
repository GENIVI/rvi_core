-module(rvi_log).

-behaviour(gen_server).

-compile(export_all).

-export([start_link/0,
	 log/3,
	 flog/4, flog/5,
	 new_id/1,
	 fetch/1,
	 timestamp/0,
	 format/2
	]).

-export([entry/3,
	 exit_good/3,
	 exit_warn/3,
	 exit_fail/3]).

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
	      level,
	      component,
	      event}).
-define(IDS, rvi_log_ids).
-define(EVENTS, rvi_log_events).

-define(MAX_LENGTH, 60).

%% levels
-define(INFO, 0).
-define(GOOD, 1).
-define(WARN, 2).
-define(FAIL, 3).


start_link() ->
    create_tabs(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

entry(TID, Component, Event) -> log(TID, ?INFO, Component, Event).
exit_good(TID, Component, Event) -> log(TID, ?GOOD, Component, Event).
exit_warn(TID, Component, Event) -> log(TID, ?WARN, Component, Event).
exit_fail(TID, Component, Event) -> log(TID, ?FAIL, Component, Event).

log(TID, Component, Event) when is_binary(TID), is_binary(Component) ->
    log(TID, ?INFO, Component, Event).

log(TID, Level, Component, Event) when is_binary(TID), is_binary(Component) ->
    gen_server:cast(?MODULE, {log, TID, level_num(Level), timestamp(), Component, bin(Event)}).


level_num(L) when is_integer(L), L >= ?INFO, L =< ?FAIL ->
    L;
level_num(info   ) -> 0;
level_num(result ) -> 1;
level_num(warning) -> 2;
level_num(error  ) -> 3.

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
    flog(0, Fmt, Args, Component, CS).

flog(Level, Fmt, Args, Component, CS) ->
    LogTID = rvi_common:get_log_id(CS),
    log(LogTID, Level, Component, format(Fmt, Args)).

timestamp() ->
    os:timestamp().

%% shorthand for the select pattern
-define(ELEM(A), {element, #evt.A, '$_'}).
-define(PROD, {{'$1', ?ELEM(level), ?ELEM(component), ?ELEM(event)}}).

fetch(Tid) ->
    fetch(Tid, []).

fetch(TidPat, Args) ->
    TidSet = select_ids(TidPat),
    lists:foldr(
      fun(Tid, Acc) ->
	      case match_events(
		     Args,
		     ets:select(?EVENTS, [{#evt{id = {Tid,'$1'},
						level = '$2',
						component = '$3',
						event = '$4'}, [], [?PROD] }])) of
		  [] -> Acc;
		  Events ->
		      [{Tid, Events}|Acc]
	      end
      end, [], TidSet).

match_events(_, []) ->
    [];
match_events(Args, Events) ->
    lists:foldl(
      fun({<<"level">>, Str}, Acc) ->
	      filter_by_level(Acc, parse_level_expr(Str));
	 (_, Acc) ->
	      Acc
      end, Events, Args).

%% The level comparison expressions use the following syntax:
%% Expr :: Int
%% | BinOp Expr
%% | UnaryOp Expr
%% | '(' Expr ')'
%%
%% BinOp :: '==' | '>=' | '=>' | '<=' | '=<' | '!=' | '|' | '&'
%% UnaryOp :: '!' | 'not'
%%
%% The token scanning pass translates the tokens to standard Erlang tokens
%% The parsing pass first infills variable references where needed, e.g.
%% "> 1" would be translated to "L > 1" (where L is a variable bound to the
%% current Event Level at evaluation time), and
%% "1" would be translated to "L == 1".
%%
%% The infill pass is needed in order to produce a legal Erlang grammar. It's followed
%% by a call to the erlang parser. The short forms are expanded in the parsed form.
%%
%% When parsing and evaluating the expressions any exception is translated into a failed
%% comparison.
%%
parse_level_expr(Str) ->
    try level_grammar(scan_level_expr(Str, []))
    catch
	error:_ ->
	    [{atom, 1, false}]
    end.

scan_level_expr(<<"!=", Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'=/=',1}|Acc]);
scan_level_expr(<<"==", Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'=:=',1}|Acc]);
scan_level_expr(<<"=>", Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'=<',1}|Acc]);
scan_level_expr(<<"<=", Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'>=',1}|Acc]);
scan_level_expr(<<">=", Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'>=',1}|Acc]);
scan_level_expr(<<"=<", Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'=<',1}|Acc]);
scan_level_expr(<<">",  Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'>',1}|Acc]);
scan_level_expr(<<"<",  Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'<',1}|Acc]);
scan_level_expr(<<"|", Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'orelse',1}|Acc]);
scan_level_expr(<<"&", Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'andalso',1}|Acc]);
scan_level_expr(<<I, Rest/binary>>, Acc) when I >= $0, I =< $3 ->
    scan_level_expr(Rest, [{integer, 1, I-$0} | Acc]);
scan_level_expr(<<"(",  Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'(',1}|Acc]);
scan_level_expr(<<")",  Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{')',1}|Acc]);
scan_level_expr(<<"not",  Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'not',1}|Acc]);
scan_level_expr(<<"!",  Rest/binary>>, Acc) ->
    scan_level_expr(Rest, [{'not',1}|Acc]);
scan_level_expr(<<"\s", Rest/binary>>, Acc) ->  scan_level_expr(Rest, Acc);
scan_level_expr(<<"\t", Rest/binary>>, Acc) ->  scan_level_expr(Rest, Acc);
scan_level_expr(<<"\n", Rest/binary>>, Acc) ->  scan_level_expr(Rest, Acc);
scan_level_expr(<<>>, Acc) ->
    lists:reverse(Acc).

level_grammar(Toks) ->
    case erl_parse:parse_exprs(infill_vars(Toks)) of
	{ok, Exprs} ->
	    expand_shorthand(Exprs);
	Error ->
	    error(Error)
    end.

-define(is_op(H), H=='=:='; H=='=!='; H=='>'; H=='<'; H=='>='; H=='=<').

infill_vars([{O,_}=H|T]) when ?is_op(O) ->
    [{var,1,'L'},H|infill_vars(T)];
infill_vars([H|T]) ->
    [H|infill_vars(T)];
infill_vars([]) ->
    [{dot,1}].

expand_shorthand([Expr|Exprs]) ->
    [expand_shorthand_(Expr)|expand_shorthand(Exprs)];
expand_shorthand([]) ->
    [].

expand_shorthand_({integer,_,_} = I) ->
    {op, 1, '=:=', {var,1,'L'}, I};
expand_shorthand_({op,Ln,Op,A,B}) when Op=='andalso';
				       Op=='orelse' ->
    {op,Ln,Op,expand_shorthand_(A), expand_shorthand_(B)};
expand_shorthand_({op,Ln,'not',E}) ->
    {op,Ln,'not',expand_shorthand_(E)};
expand_shorthand_(Expr) ->
    Expr.

filter_by_level([H|T], Expr) ->
    Res = try erl_eval:exprs(Expr, [{'L', element(2,H)}]) of
	      {value, true, _} -> true;
	      _ -> false
	  catch
	      _:_ -> false
	  end,
    if Res ->
	    [H|filter_by_level(T, Expr)];
       true ->
	    filter_by_level(T, Expr)
    end.

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
    ?debug("fetch: TIDs = ~p", [TIDs]),
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
    do_log(tid_(<<"rvi_log">>, 0, Tag), ?INFO, timestamp(), <<"rvi_common">>,
	   format("Started - Tag = ~s", [Tag]), N),
    {noreply, St};
handle_cast({log, TID, Level, TS, Component, Event}, #st{n = N} = St) ->
    do_log(TID, Level, TS, Component, Event, N),
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

do_log(Tid, Level, TS, Component, Event, N) ->
    case ets:member(?IDS, Tid) of
	true ->
	    store_event(Tid, Level, TS, Component, Event);
	false ->
	    case ets:info(?IDS, size) of
		Sz when Sz >= N ->
		    purge_id();
		_ ->
		    ok
	    end,
	    ets:insert(?IDS, {Tid}),
	    store_event(Tid, Level, TS, Component, Event)
    end.

store_event(Tid, Level, TS, Component, Event) ->
    rvi_log_log:info("~-20s ~-2w ~-12s ~s", [Tid, Level, Component, Event]),
    ?info("RVI_LOG: ~p/~w/~p/~p", [Tid, Level, Component, Event]),
    ets:insert(?EVENTS, #evt{id = {Tid, TS},
			     level = Level,
			     component = Component,
			     event = Event}).

purge_id() ->
    case ets:first(?IDS) of
	'$end_of_table' ->
	    %% Should not be possible ...
	    ok;
	Tid ->
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

select_ids(TidPat) ->
    ets:foldr(
      fun({Tid}, Acc) ->
	      case match_id(Tid, TidPat) of
		  true -> [Tid|Acc];
		  false -> Acc
	      end
      end, [], ?IDS).

match_id(Tid, Pat) ->
    case re:run(Tid, Pat, []) of
	{match, _} -> true;
	nomatch -> false
    end.

format_result(Log) ->
    ?debug("format_result(~p)", [Log]),
    Events = lists:foldl(
	       fun({_Pat, Matches}, Acc) ->
		       lists:foldl(
			 fun({Id,Es}, D) ->
				 orddict:store(Id, Es, D)
			 end, Acc, Matches)
	       end, orddict:new(), Log),
    ?debug("Events = ~p", [Events]),
    [{TID, format_events(Es)} || {TID, Es} <- Events].

format_events([{TS, Level, Comp, Evt} = E|Es]) ->
    ?debug("format_events(), E = ~p", [E]),
    [[{<<"ts">>, utc_hr_timestamp(TS)},
      {<<"lvl">>, bin(Level)},
      {<<"cmp">>, bin(Comp)},
      {<<"evt">>, bin(Evt)}] | format_events(Es)];
format_events([]) ->
    [].

bin(A) when is_atom(A)   -> atom_to_binary(A, latin1);
bin(B) when is_binary(B) -> B;
bin(L) when is_list(L)   -> iolist_to_binary(L);
bin(Other) ->
    iolist_to_binary(io_lib:fwrite("~w", [Other])).


utc_hr_timestamp({_,_,US} = TS) ->
    %% The 'rem' op is just a precaution; a properly generated 'now' TS
    %% should not have US > 1000000, but a derived TS could (since just
    %% about all operations on such timestamps will work anyway).
    Secs = rvi_common:utc_timestamp(TS),
    Secs + (US rem 1000000)/1000000.
