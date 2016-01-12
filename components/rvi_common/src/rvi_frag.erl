-module(rvi_frag).
-behaviour(gen_server).

-export([send/4,     % (Msg, Window, Mod, Opts)
	 maybe_fragment/3]).
-export([start_link/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include_lib("lager/include/log.hrl").

-record(st, {msg_id = 0,
	     frags = dict_new()}).

-record(frag, {id,
	       window,
	       msg,
	       tref}).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(PACKET_MOD, dlink_data_msgpack).
-define(PACKET_ST, []).

send(Msg, Opts, Mod, SendF) ->
    ?debug("send(Msg, ~p, ~p, ~p)", [Opts, Mod, SendF]),
    case use_frag(Msg, Opts) of
	{true, Window} ->
	    gen_server:call(?SERVER, {send, Msg, Window, Mod, Opts});
	false ->
	    SendF()
    end.

maybe_fragment([{<<"frg">>,[_|_] = Info}], Mod, Opts) ->
    gen_server:call(?SERVER, {fragment_received, Info, Mod, Opts});
maybe_fragment([{<<"frg-get">>, [_|_] = Args}], Mod, Opts) ->
    gen_server:call(?SERVER, {frag_get_received, Args, Mod, Opts});
maybe_fragment([{<<"frg-end">>, [_|_] = Args}], _Mod, _Opts) ->
    gen_server:call(?SERVER, {frag_end_received, Args});
maybe_fragment(_, _, _) ->
    false.

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

fragment_from_offset(Msg, Offs, ChunkSz) ->
    ?debug("fragment_from_offset(Msg, ~p, ~p)", [Offs, ChunkSz]),
    MsgBin = iolist_to_binary(Msg),
    ?debug("MsgBin size = ~p", [byte_size(MsgBin)]),
    case byte_size(MsgBin) - Offs + 1 of
	NewSz when NewSz =< 0 ->
	    ?debug("NewSz = ~p - empty fragment!!", [NewSz]),
	    <<>>;
	NewSz ->
	    Sz = erlang:min(NewSz, ChunkSz),
	    Prev = Offs - 1,
	    ?debug("NewSz = ~p, Sz = ~p, Prev = ~p", [NewSz, Sz, Prev]),
	    <<_:Prev/binary, Frag:Sz/binary, _Rest/binary>> = MsgBin,
	    Frag
    end.

handle_call(Req, From, S) ->
    try handle_call_(Req, From, S)
    catch
	error:R ->
	    {reply, {error, R}, S}
    end.

handle_call_({frag_get_received, Info, Mod, Opts}, {Pid,_},
	     #st{frags = Fs} = S) ->
    [ID, Offset, Bytes] = Info,
    TID = {Pid, ID},
    case dict_find(TID, Fs) of
	{ok, #frag{msg = Msg}} ->
	    Bin = fragment_from_offset(Msg, Offset, Bytes),
	    Sz = byte_size(Msg),
	    Mod:send_data(Pid, encode_fragment(ID, Sz, Offset, Bin, Opts)),
	    {reply, true, S};
	error ->
	    %% Ignore, but reflect that it was a fragment message
	    %% (perhaps we should send an error message to the client?)
	    {reply, true, S}
    end;

handle_call_({send, Msg, Window, Mod, Opts}, {Pid, _}, St) ->
    try init_frag(Msg, Window, Opts, Mod, Pid, St)
    catch
	error:R ->
	    ?error("init_frag ERROR: ~p~n~p", [R, erlang:get_stacktrace()]),
	    {reply, {error, R}, St}
    end;
handle_call_({fragment_received, FragInfo, Mod, Opts}, {Pid,_}, S) ->
    ?debug("fragment_received", []),
    handle_fragment_received(FragInfo, Mod, Opts, Pid, S);

handle_call_({frag_end_received, FragInfo}, {Pid,_},
	     #st{frags = Fs} = S) ->
    [ID, ResultCode] = FragInfo,
    ?debug("fragment-end; ID = ~p; ResultCode = ~p", [ID, ResultCode]),
    {reply, true, S#st{frags = dict_erase({Pid,ID}, Fs)}};

handle_call_(_, _, S) ->
    {reply, error, S}.

handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _)   -> ok.
code_change(_, S, _) -> {ok, S}.

handle_fragment_received(FragInfo, Mod, Opts, Pid, S) ->
    [ID, Size, Offs, Bin] = FragInfo,
    FragSz = byte_size(Bin),
    End = Offs + FragSz - 1,
    TID = {Pid, ID},
    case Bin of
	<<>> ->
	    ?debug("Empty fragment (~p); don't store", [FragInfo]),
	    ok;
	_ ->
	    ?debug("ID = ~p, Size = ~p, Offs = ~p, End = ~p",
		   [ID, Size, Offs, End]),
	    ets:insert(?TAB, {{TID, Offs, End}, Bin})
    end,
    if Offs == 1, End >= Size ->
	    send_msg_complete(ID, 0, Mod, Pid, Opts),
	    {reply, {true, Bin}, S};
       true ->
	    Check = check_message(TID, Offs, Size),
	    ?debug("check_message() -> ~p", [Check]),
	    case Check of
		{message, Msg} ->
		    send_msg_complete(ID, 0, Mod, Pid, Opts),
		    {reply, {true, Msg}, S};
		{missing, [{Start, End}|_]} ->
		    ReqSz = erlang:min(FragSz, End-Start+1),
		    request_fragment(ID, Start, ReqSz, Mod, Pid, Opts),
		    {reply, true, S};
		ok ->
		    request_fragment(ID, End+1, FragSz, Mod, Pid, Opts),
		    {reply, true, S}
	    end
    end.

check_message(TID, Offs, Size) ->
    Frags = fragments(TID, Offs),
    case find_holes(Frags, Size) of
	{[], true} ->
	    ?debug("no holes, complete message", []),
	    ets:select_delete(?TAB, [{ {{TID,'_','_'},'_'}, [], [true] }]),
	    {message, join_fragments(Frags)};
	{[], _} ->
	    ?debug("no holes, not complete", []),
	    ok;
	{Holes, _} ->
	    ?debug("found holes = ~p", [Holes]),
	    {missing, Holes}
    end.

fragments(TID, Offs) ->
    ets:select(?TAB, [{ {{TID, '$1', '$2'}, '$3'},
			[{'=<', '$1', Offs}],
			[{{'$1','$2','$3'}}] }]).

find_holes(Frags, Size) ->
    {_, Missing, IsLast} =
	lists:foldl(
	  fun({Beg, End, _Bin}, {Prev, Acc, IsLast0}) ->
		  ?debug("IsLast0 = ~p, Beg = ~p, End = ~p, Size = ~p",
			 [IsLast0, Beg, End, Size]),
		  IsLast1 = IsLast0 orelse (End >= Size),
		  case Beg - Prev of
		      1 ->
			  {End, Acc, IsLast1};
		      Diff when Diff > 1 ->
			  {End, [{Prev+1, Beg-1}|Acc], IsLast1}
		  end
	  end, {0, [], false}, Frags),
    {Missing, IsLast}.

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

init_frag(Msg, Window, Opts, Mod, Pid, St) ->
    ?debug("init_frag(Msg, ~p, ~p, ~p, ~p, ~p", [Window,Opts,Mod,Pid,St]),
    {Id, St1} = next_id(St),
    TID = {Pid, Id},
    Sz = byte_size(Msg),
    AdjWindow = adjust_window(Window, Id, Sz, Opts),
    ?debug("Adjusted window: ~p", [AdjWindow]),
    Frag = fragment_from_offset(Msg, 1, AdjWindow),
    Data = encode_fragment(Id, Sz, 1, Frag, Opts),
    ?debug("size of encoded fragment (Win=~p): ~p", [Window, size(Data)]),
    ok = Mod:send_data(Pid, Data),
    TRef = start_timer(init_timeout, TID, Pid, Opts),
    {reply, ok, store_frag(TID, #frag{id = Id,
				      window = AdjWindow,
				      msg = Msg,
				      tref = TRef}, St1)}.

next_id(#st{msg_id = Prev} = St) ->
    Id = Prev+1,
    {Id, St#st{msg_id = Id}}.

start_timer(Type, Id, Pid, Opts) ->
    erlang:start_timer(timeout_value(Type, Opts), self(), {Type, Id, Pid}).

store_frag(TID, #frag{} = Frag, #st{frags = Frags} = St) ->
    St#st{frags = dict_store(TID, Frag, Frags)}.

timeout_value(Type, Opts) ->
    case lists:keyfind(Type, 1, Opts) of
	{_, Value} -> Value;
	false      -> timeout_default(Type)
    end.

timeout_default(init_timeout) -> timer:hours(1);
timeout_default(request_timeout) -> timer:seconds(30).


dict_new() ->
    orddict:new().

dict_store(Key, Value, Dict) ->
    orddict:store(Key, Value, Dict).

dict_find(Key, Dict) ->
    orddict:find(Key, Dict).

dict_erase(Key, Dict) ->
    orddict:erase(Key, Dict).

adjust_window(Window, Id, Sz, Opts) ->
    %% Subtract framing size (encoded empty fragment) from Window,
    %% but arbitrarily set minimum window to 10 (must at least be > 0)
    Enc = encode_msg([{<<"frg">>, [Id, Sz, Sz, <<>>]}], Opts),
    ?debug("Empty frag: ~p", [Enc]),
    erlang:max(10, Window - byte_size(Enc)).

encode_fragment(Id, Sz, Offs, Frag, Opts) ->
    encode_msg([{<<"frg">>, [Id, Sz, Offs, Frag]}], Opts).

request_fragment(ID, Start, Bytes0, Mod, Pid, Opts) ->
    Bytes = erlang:max(get_window(Opts), Bytes0),
    FragInfo = [ID, Start, Bytes],
    ?debug("request_fragment: ~p", [FragInfo]),
    Mod:send_data(Pid, encode_msg([{<<"frg-get">>, FragInfo}], Opts)).

send_msg_complete(ID, ResultCode, Mod, Pid, Opts) ->
    ?debug("send_msg_complete(~p, ~p, ~p, ~p, ...)", [ID, ResultCode,
						      Mod, Pid]),
    Mod:send_data(Pid, encode_msg([{<<"frg-end">>, [ID, ResultCode]}], Opts)).

encode_msg(Msg, Opts) ->
    {PMod, PSt} = get_packet_mod(Opts),
    {ok, Bin, _} = PMod:encode(Msg, PSt),
    Bin.

get_packet_mod(Opts) ->
    case lists:keyfind(packet_mod, 1, Opts) of
	false ->
	    {?PACKET_MOD, ?PACKET_MOD:init([])};
	{_, {Mod,_} = Res} when is_atom(Mod) ->
	    Res;
	{_, Mod} when is_atom(Mod) ->
	    case lists:keyfind(packet_st, 1, Opts) of
		false ->
		    {Mod, Mod:init([])};
		{_, St} ->
		    {Mod, St}
	    end
    end.

get_window([{"rvi.max_msg_size", Sz}|_]) -> Sz;
get_window([{max_msg_size, Sz}|_]      ) -> Sz;
get_window([_|T]) ->
    get_window(T);
get_window([]) ->
    [].

use_frag(Bin, Opts) ->
    {PR, DR, PW, DW} = frag_opts(Opts),
    Reliable = case {PR, DR} of
		   {_, _} when is_boolean(PR) -> PR;
		   {undefined, _} when is_boolean(DR) -> DR;
		   _ -> undefined
	       end,
    Win = calc_window(PW, DW),
    Sz = byte_size(Bin),
    case Reliable of
	true -> {true, Win};
	false ->
	    case Win of
		_ when is_integer(Win) ->
		    if Sz < Win -> false;
		       true -> {true, Win}
		    end;
		infinity ->
		    false
	    end;
	undefined ->
	    if is_integer(Win) -> {true, Win};
	       true -> false
	    end
    end.

frag_opts(Opts) ->
    ?debug("frag_opts(~p)", [Opts]),
    frag_opts(Opts, undefined, undefined, undefined, undefined).

frag_opts([{"rvi.max_msg_size", PW}|T], PR, DR, _, DW) ->
    frag_opts(T, PR, DR, PW, DW);
frag_opts([{<<"rvi.max_msg_size">>, PW}|T], PR, DR, _, DW) ->
    frag_opts(T, PR, DR, PW, DW);
frag_opts([{max_msg_size, DW}|T], PR, DR, PW, _) ->
    frag_opts(T, PR, DR, PW, DW);
frag_opts([{"rvi.reliable", PR}|T], _, DR, PW, DW) ->
    frag_opts(T, PR, DR, PW, DW);
frag_opts([{reliable, DR}|T], PR, _, PW, DW) ->
    frag_opts(T, PR, DR, PW, DW);
frag_opts([_|T], PR, DR, PW, DW) ->
    frag_opts(T, PR, DR, PW, DW);
frag_opts([], PR, DR, PW, DW) ->
    {PR, DR, PW, DW}.

calc_window(PW, DW) when is_integer(PW), is_integer(DW) ->
    erlang:min(PW, DW);
calc_window(undefined, DW) ->
    calc_window(DW);
calc_window(PW, undefined) ->
    calc_window(PW).

calc_window(undefined) -> infinity;
calc_window(infinity ) -> infinity;
calc_window(W) when is_integer(W), W > 0 -> W.
