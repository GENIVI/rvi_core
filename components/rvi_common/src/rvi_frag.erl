-module(rvi_frag).
-compile(export_all).

-record(st, {}).
-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-type msg() :: binary().
-type chunk() :: binary().
-type tid() :: any().
-type offset() :: non_neg_integer().
-type chunk_size() :: non_neg_integer().
-type is_last() :: boolean().
-type frag() :: {offset(), offset(), chunk(), is_last()}.

fragment(TID, {Offset, _, Bin, IsLast}) ->
    fragment(TID, Offset, Bin, IsLast).

-spec fragment(tid(), offset(), msg(), is_last()) ->
		      ok
			  | {message, msg()}
			  | {missing, [{offset(), offset()}]}.
fragment(TID, Offset, Bin, IsLast) ->
    gen_server:call(?SERVER, {fragment, TID, Offset, Bin, IsLast}).

start_link() ->
    create_ets(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_ets() ->
    case ets:info(?TAB, name) of
	undefined ->
	    ets:new(?TAB, [ordered_set, public, named_table]);
	_ ->
	    true
    end.

init([]) ->
    {ok, #st{}}.

-spec first_fragment(msg(), chunk_size()) -> frag().
first_fragment(Msg, infinity) ->
    {1, byte_size(Msg), Msg, true};
first_fragment(Msg, ChunkSz) when is_integer(ChunkSz), ChunkSz > 0 ->
    MsgBin = iolist_to_binary(Msg),
    Sz = erlang:min(byte_size(MsgBin), ChunkSz),
    <<Frag:Sz/binary, Rest/binary>> = MsgBin,
    {1, Sz, Frag, Rest =:= <<>>}.

-spec next_fragment(msg(), offset() | frag(), chunk_size()) -> done | frag().
next_fragment(Msg, {_, Last, _PrevFrag, _IsLast}, ChunkSz) ->
    next_fragment(Msg, Last, ChunkSz);

next_fragment(Msg, Last, ChunkSz) ->
    MsgBin = iolist_to_binary(Msg),
    case byte_size(MsgBin) - Last of
	NewSz when NewSz =< 0 ->
	    done;
	NewSz ->
	    Sz = erlang:min(NewSz, ChunkSz),
	    <<_:Last/binary, Frag:Sz/binary, Rest/binary>> = MsgBin,
	    Start = Last+1,
	    Stop = Last + Sz,
	    {Start, Stop, Frag, Rest =:= <<>>}
    end.

handle_call({fragment, TID, Offs, Bin, IsLast}, _, S) ->
    End = Offs + byte_size(Bin) -1,
    ets:insert(?TAB, {{TID, Offs, End}, Bin}),
    {reply, check_message(TID, Offs, IsLast), S};
handle_call(_, _, S) ->
    {reply, error, S}.

handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _)   -> ok.
code_change(_, S, _) -> {ok, S}.

check_message(TID, Offs, IsLast) ->
    Frags = fragments(TID, Offs),
    case find_holes(Frags) of
	[] when IsLast ->
	    ets:select_delete(?TAB, [{ {{TID,'_','_'},'_'}, [], [true] }]),
	    {message, join_fragments(Frags)};
	[] ->
	    ok;
	Holes ->
	    {missing, Holes}
    end.

fragments(TID, Offs) ->
    ets:select(?TAB, [{ {{TID, '$1', '$2'}, '$3'},
			[{'=<', '$1', Offs}],
			[{{'$1','$2','$3'}}] }]).

find_holes(Frags) ->
    {_, Missing} =
	lists:foldl(
	  fun({A, B, _}, {Prev, Acc}) ->
		  case A - Prev of
		      1 ->
			  {B, Acc};
		      Diff when Diff > 1 ->
			  {B, [{Prev+1, A-1}|Acc]}
		  end
	  end, {0, []}, Frags),
    Missing.

%% Allow fragments to overlap
join_fragments([{1,_,F}|Frags]) ->
    join_fragments(Frags, F).

join_fragments([{A,_,F}|Frags], Acc) when A - byte_size(Acc) =:= 1 ->
    join_fragments(Frags, <<Acc/binary, F/binary>>);
join_fragments([{A,_,F}|Frags], Acc) ->
    Prev = A-1,
    <<Prefix:Prev/binary, _/binary>> = Acc,
    join_fragments(Frags, <<Prefix/binary, F/binary>>);
join_fragments([], Acc) ->
    Acc.
