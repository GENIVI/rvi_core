-module(rvi_server).
-behaviour(gen_server).

-export([ensure_ready/1,
	 ensure_ready/2]).

-export([start_link/0,
	 await/0,
	 info/0,
	 info/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include_lib("lager/include/log.hrl").

-record(st, {wait_for = [],
	     tref,
	     ready = []}).

ensure_ready(Timeout) ->
    ensure_ready(rvi_core, Timeout).

ensure_ready(App, Timeout) ->
    gproc:await({n,l,App}, Timeout).

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init([]) ->
    WaitFor = lists:flatmap(
		fun({_, Names}) ->
			Names
		end, setup:find_env_vars(rvi_core_await)),
    {ok, start_timer(#st{wait_for = WaitFor})}.

await() ->
    call(await).

info() ->
    call(info).

info(waiting) -> call(waiting);
info(ready  ) -> call(ready);
info(_) ->
    undefined.

handle_call(ready, _From, #st{ready = Ready} = S) ->
    {reply, Ready, S};
handle_call(waiting, _From, #st{wait_for = WF} = S) ->
    {reply, WF, S};
handle_call(info, _From, #st{ready = Ready,
			     wait_for = WF} = S) ->
    {reply, [{ready, Ready},
	     {waiting_for, WF}], S};
handle_call(await, _From, #st{wait_for = WF} = S) ->
    [gproc:nb_wait(Name) || Name <- WF],
    {reply, ok, S};
handle_call(_, _, S) ->
    {reply, error, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info({gproc, _, registered, {Key, _, _}}, #st{wait_for = WF,
						     ready = Ready} = S) ->
    WF1 = WF -- [Key],
    S1 = S#st{ready = [Key | Ready], wait_for = WF1},
    if WF1 == [] andalso WF =/= [] ->
	    rvi_common:announce({n, l, rvi_core}),
	    {noreply, cancel_timer(S1)};
       true ->
	    {noreply, S1}
    end;
handle_info({timeout, TRef, timeout}, #st{tref = TRef,
					  wait_for = WF} = S) ->
    case WF of
	[] ->
	    {noreply, S#st{tref = undefined}};
	[_|_] ->
	    ?warning("Still waiting for ~p", [WF]),
	    {noreply, start_timer(S)}
    end;
handle_info(_, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.


call(Req) ->
    gen_server:call(?MODULE, Req).

start_timer(S) ->
    TRef = erlang:start_timer(timer:seconds(10), self(), timeout),
    S#st{tref = TRef}.

cancel_timer(#st{tref = undefined} = S) ->
    S;
cancel_timer(#st{tref = TRef} = S) ->
    erlang:cancel_timer(TRef),
    S#st{tref = undefined}.
