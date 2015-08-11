-module(gsms_session).
-behaviour(gen_server).

-export([new/2,
	 send/3,
	 get_signal_strength/1,
	 subscribe/2,
	 unsubscribe/2
	]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("gsms.hrl").

-record(st, {mod,
	     mod_state,
	     subscribers = []}).

-type mod_state() :: any().
-type dest() :: any().
-type body() :: any().
-type option() :: {atom(), any()}.
-type options() :: [option()].

-type cb_return() :: {ok, mod_state()} | {error, any()}.

-callback init(options()) -> {ok,gsms_addr(),[{atom(), term()}]}
				 | {error, any()}.
-callback handle_send(dest(), body(), mod_state()) -> cb_return().
-callback mandatory_options() -> [atom()].

new(Mod, Opts) ->
    true = valid_opts(Opts, Mod),
    {ok, Pid} = gen_server:start_link(?MODULE, {Mod, Opts}, []),
    Pid.

send(Session, Opts, Body) ->
    call_(Session, {send, Opts, Body}).

get_signal_strength(Session) ->
    call_(Session, get_signal_strength).

subscribe(Session, Filter) ->
    case lists:keytake(reg_exp, 1, Filter) of
	{value, RegExp, Rest} when is_list(RegExp) ->
	    case re:compile(RegExp, [unicode]) of
		{ok, MP} ->
		    call_(Session, {subscribe, [{reg_exp, MP} | Rest]});
		{error, _} = E ->
		    E
	    end;
	{value, _, _} ->
	    %% Assume MP format
	    call_(Session, {subscribe, Filter});
	false ->
	    call_(Session, {subscribe, Filter})
    end.

unsubscribe(Session, Ref) ->
    call_(Session, {unsubscribe, Ref}).

mandatory_options() ->
    [].

init({Mod, Opts}) ->
    case Mod:init(Opts) of
	{ok, BNumber, Attrs, ModSt} ->
	    gsms_router:join(BNumber, ?MODULE, Attrs),
	    {ok, #st{mod = Mod,
		     mod_state = ModSt}};
	Other ->
	    Other
    end.

handle_call({send, Opts, Body}, _From, #st{mod = Mod,
					   mod_state = ModSt} = S) ->
    case Mod:handle_send(Opts, Body, ModSt) of
	{ok, Reply, ModSt1} ->
	    {reply, Reply, S#st{mod_state = ModSt1}};
	{error, _} = Error ->
	    {reply, Error, S}
    end;
handle_call(get_signal_strength, _From, #st{mod = Mod,
					    mod_state = ModSt} = S) ->
    case Mod:get_signal_strength(ModSt) of
	{ok, Res, St1} ->
	    {reply, Res, S#st{mod_state = St1}};
	{error, _} = E ->
	    {reply, E, S}
    end;
handle_call({subscribe, _Pattern}, _From, #st{subscribers = _Subs} = S) ->
    {reply, {error, nyi}, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

valid_opts(Opts, Mod) ->
    Mandatory = mandatory_options() ++ Mod:mandatory_options(),
    case [O || O <- Mandatory,
	       not lists:keymember(O, 1, Opts)] of
	[] ->
	    true;
	[_|_] = Missing ->
	    erlang:error({mandatory, lists:usort(Missing)})
    end.

call_(Session, Req) ->
    gen_server:call(Session, Req).

