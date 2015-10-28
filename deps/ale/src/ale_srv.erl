%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Malotte Westman Lönne <malotte@malotte.net>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%  ale server.
%%%
%%% Created:  2012 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------
-module(ale_srv).

-behaviour(gen_server).

-include_lib("lager/include/log.hrl").


%% API
-export([start_link/1, 
	 stop/0]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

%% Testing
-export([start/0, 
	 start/1, 
	 traces/0,
	 clients/0,
	 dump/0,
	 debug/1]).

-record(trace_item,
	{
	  trace,
	  lager_ref,
	  client
	}).

-record(client_item,
	{
	  pid,
	  monitor
	}).

-record(ctx,
	{
	  trace_list = [],
	  client_list = [],
	  debug  %% Debug of own process
	}).

-define(dbg(Format, Args),
 	lager:debug("~s(~p): " ++ Format, [?MODULE, self() | Args])).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server that will keep track of traces.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Options::list(tuple())) -> 
			{ok, Pid::pid()} | 
			ignore | 
			{error, Error::term()}.

start_link(Args) ->
    ?dbg("start_link: starting, args ~p",[Args]),
    Opts = proplists:get_value(options, Args, []),    
    F =	case proplists:get_value(linked,Opts,true) of
	    true -> start_link;
	    false -> start
	end,
    gen_server:F({local, ?MODULE}, ?MODULE, Args, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Error::term()}.

stop() ->
    ?dbg("start_link: stopping",[]),
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Lists existing traces.
%% @end
%%--------------------------------------------------------------------
-spec traces() -> list(tuple()).

traces() ->
    gen_server:call(?MODULE, traces).

%%--------------------------------------------------------------------
%% @doc
%% Lists existing clients.
%% @end
%%--------------------------------------------------------------------
-spec clients() -> list(tuple()).

clients() ->
    gen_server:call(?MODULE, clients).


%%--------------------------------------------------------------------
%% Test functions
%% @private
start() ->
    start([]).
%% @private
start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

%% @private
dump() ->
    gen_server:call(?MODULE, dump).

%% @private
debug(TrueOrFalse) ->
    gen_server:call(?MODULE, {debug, TrueOrFalse}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(list(tuple())) -> 
		  {ok, Ctx::#ctx{}} |
		  {stop, Reason::term()}.

init(Args) ->
    Opts = proplists:get_value(options, Args, []),    
    {ok,Debug} = set_debug(proplists:get_value(debug, Opts, false), undefined),
    ?dbg("init: args ~p",[Args]),
    InitTraces = proplists:get_value(init_traces, Args, []),
    TL = 
	lists:foldl(fun({Filter, Level}, TraceList) ->
			    {_Result, TmpL} =
				add_trace({Filter, Level, console},
					  self(), TraceList),
			    TmpL;
		       ({Filter, Level, File}, TraceList) ->
			    %% Do we want this check ??
			    case filelib:is_regular(File) of
				true ->
				    {_Result, TmpL} =
					add_trace({Filter, Level, File},
						  self(), TraceList),
				    TmpL;
				false ->
				    error_logger:error_msg(
				      "ale: non existing file ~p, " ++
				      "no trace added.~n", [File]),
				    TraceList
			    end
		    end,
		    [], InitTraces),
    {ok,  #ctx {debug = Debug, trace_list = TL}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages.
%% Request can be the following:
%% <ul>
%% <li> trace</li>
%% <li> dump</li>
%% <li> stop</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-type call_request()::
	{trace, 
	 OnOrOff:: on | off, 
	 Filter::list(tuple()), 
	 Level::atom(), 
	 Pid::pid()} |
	dump |
	{debug, TrueOrFalse::boolean()} |
	clear |
	stop.

-spec handle_call(Request::call_request(), 
		  From::{pid(), Tag::term()}, 
		  Ctx::#ctx{}) ->
			 {reply, Reply::term(), Ctx::#ctx{}} |
			 {noreply, Ctx::#ctx{}} |
			 {stop, Reason::atom(), Reply::term(), Ctx::#ctx{}}.


handle_call({trace, on, Filter, Level, Client, File} = _T, _From, 
	    Ctx=#ctx {trace_list = TL, client_list = CL}) 
  when is_list(Filter) ->
    ?dbg("handle_call: trace on ~p.",[_T]),
    case add_trace({Filter, Level, File}, Client, TL) of
	{ok, NewTL} -> 
	    NewCL = monitor_client(Client, CL),
	    {reply, ok, Ctx#ctx {trace_list = NewTL, client_list = NewCL}};
	{Error, TL} ->
	    {reply, Error, Ctx}
    end;
handle_call({trace, off, Filter, Level, Client, File} = _T, _From, 
	    Ctx=#ctx {trace_list = TL, client_list = CL}) 
  when is_list(Filter) ->
    ?dbg("handle_call: trace off ~p.",[_T]),
    case remove_trace({Filter, Level, File}, Client, TL) of
	{ok, NewTL} -> 
	    NewCL = demonitor_client(Client, NewTL, CL),
	    {reply, ok, Ctx#ctx {trace_list = NewTL, client_list = NewCL}};
	{E, TL} ->
	    {reply, E, Ctx}
    end;

handle_call(traces, _From,  Ctx=#ctx {trace_list = TL})  ->
    ?dbg("handle_call: traces.",[]),
    {reply, TL, Ctx};
     
handle_call(clients, _From,  Ctx=#ctx {client_list = CL})  ->
    ?dbg("handle_call: clients.",[]),
    {reply, CL, Ctx};
     
handle_call(i, _From, Ctx=#ctx {trace_list = TL, client_list = CL}) ->
    ?dbg("handle_call: i.",[]),
    PrettyTraceList = {'Trace list:',
	lists:map(
	  fun(#trace_item {client = C, trace = {F, L, B}}) ->
		  lists:flatten(
		    io_lib:format(
		      "Client ~p, filter ~p, level ~p, backend ~p.", 
					      [C, F, L, B]))		  
	  end, TL)},
    PrettyClientList = {'Client list:',
	lists:map(
	  fun(#client_item {pid = P}) ->
		  lists:flatten(io_lib:format("Client ~p.", [P]))
	  end, CL)},
    {reply, {PrettyTraceList, PrettyClientList}, Ctx};

handle_call(dump, _From, Ctx=#ctx {trace_list = TL, client_list = CL}) ->
    lists:foreach(fun(#trace_item {client = C, trace = T, lager_ref = LR}) ->
			  io:format("Client ~p, trace ~p, ref ~w.\n", 
				    [C, T, LR])
		  end, TL),
    lists:foreach(fun(#client_item {pid = P, monitor = M}) ->
			  io:format("Client ~p, monitor ~p.\n", 
				    [P, M])
		  end, CL),
    {reply, ok, Ctx};

handle_call({debug, TrueOrFalse}, _From, Ctx=#ctx {debug = Dbg}) ->
    case set_debug(TrueOrFalse, Dbg) of
	{ok, NewDbg} ->
	    {reply, ok, Ctx#ctx { debug = NewDbg }};
	Error ->
	    {reply, Error, Ctx}
    end;

handle_call(clear, _From, Ctx=#ctx {trace_list = TL, client_list = CL}) ->
    lists:foldl(fun(#trace_item {trace = Trace, client = Client}, Rest) ->
			remove_trace(Trace, Client, TL),
			Rest
		end, [], TL),
    ?dbg("clear: traces removed.",[]),
    lists:foreach(fun( #client_item {monitor = Mon}) ->
			  erlang:demonitor(Mon, [flush])
		  end, CL),
    ?dbg("clear: clients removed.",[]),
    {reply, ok, Ctx#ctx {trace_list = [], client_list = []}};

handle_call(stop, _From, Ctx) ->
    ?dbg("handle_call: stop.",[]),
    {stop, normal, ok, Ctx};

handle_call(_Request, _From, Ctx) ->
    ?dbg("handle_call: unknown request ~p.", [_Request]),
    {reply, {error,bad_call}, Ctx}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages.
%%
%% @end
%%--------------------------------------------------------------------
-type cast_msg()::
	term().

-spec handle_cast(Msg::cast_msg(), Ctx::#ctx{}) -> 
			 {noreply, Ctx::#ctx{}} |
			 {stop, Reason::term(), Ctx::#ctx{}}.

handle_cast(_Msg, Ctx) ->
    ?dbg("handle_cast: unknown msg ~p", [_Msg]),
    {noreply, Ctx}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%% 
%% @end
%%--------------------------------------------------------------------
-type info()::
	{'DOWN', Ref::reference(), process, Pid::pid(), Reason::term()} |
	term().

-spec handle_info(Info::info(), Ctx::#ctx{}) -> 
			 {noreply, Ctx::#ctx{}} |
			 {noreply, Ctx::#ctx{}, Timeout::timeout()} |
			 {stop, Reason::term(), Ctx::#ctx{}}.

handle_info({'DOWN', MonRef, process, _Pid, Reason}, 
	    Ctx=#ctx {trace_list = TL, client_list = CL}) ->
    ?dbg("handle_info: DOWN for process ~p received, reason ~p.", 
	 [_Pid, Reason]),
    %% See if we have this client
    {NewCL, NewTL} =
	case lists:keytake(MonRef, #client_item.monitor, CL) of
	    false ->
		%% Ignore
		?dbg("handle_info: client not found.",[]), 
		{CL, TL};
	    {value, #client_item {pid = Pid}, Rest} ->
		%% Remove all traces for this client
		?dbg("handle_info: client found, removing traces.",[]), 
		{Rest, remove_traces(Pid, TL, [])}
		    
	end,
    {noreply, Ctx#ctx {trace_list = NewTL, client_list = NewCL}};

handle_info(_Info, Ctx) ->
    ?dbg("handle_info: unknown info ~p received.", [_Info]),
    {noreply, Ctx}.
	
%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), Ctx::#ctx{}) -> 
		       no_return().

terminate(_Reason, 
	  _Ctx=#ctx {trace_list = TL, client_list = CL, debug = Dbg}) ->
    ?dbg("terminate: Reason = ~p.",[_Reason]),
    lists:foldl(fun(#trace_item {trace = Trace, client = Client}, Rest) ->
			remove_trace(Trace, Client, TL),
			Rest
		end, [], TL),
    ?dbg("terminate: traces removed.",[]),
    lists:foreach(fun( #client_item {monitor = Mon}) ->
			  erlang:demonitor(Mon, [flush])
		  end, CL),
    ?dbg("terminate: clients removed.",[]),
    stop_debug(Dbg),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process ctx when code is changed.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn::term(), Ctx::#ctx{}, Extra::term()) -> 
			 {ok, NewCtx::#ctx{}}.

code_change(_OldVsn, Ctx, _Extra) ->
    ?dbg("code_change: old version ~p.",[_OldVsn]),
    {ok, Ctx}.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec add_trace({Filter::term(), Level::atom(), File::string() | console},
		Client::pid(), TL::list(tuple())) ->
		       list(tuple()).

add_trace(Trace = {Filter, Level, File}, Client, TL) -> 
    %% See if we already are tracing this.
    ?dbg("add_trace: trace ~p for client ~p",[Trace, Client]),
    case lists:keyfind(Trace, #trace_item.trace, TL) of
	false ->
	    %% Create new trace in lager
	    ?dbg("add_trace: trace ~p not found.",[Trace]),
	    Res = 
		case File of
		    console ->
			lager:trace_console(Filter, Level);
		    _F ->
			lager:trace_file(File, Filter,Level)
		end,
	    case Res of
		{ok, LagerRef} ->
		    ?dbg("add_trace: trace added in lager, ref ~p.",[LagerRef]),
		    {ok, 
		     [#trace_item {trace = Trace,  
				   lager_ref = LagerRef, 
				   client = Client}
		      | TL]};
		{error, _Reason}  = E->
		    ?dbg("add_trace: lager call failed, reason ~p.",[_Reason]),
		    {E, TL}
	    end;
	#trace_item {trace = Trace, client = Client} ->
	    %% Trace already exists, ignore
	    ?dbg("add_trace: trace ~p found.",[Trace]),
	    {ok, TL};
	#trace_item {trace = Trace, lager_ref = LagerRef, client = _Other} ->
	    %% Trace exists in lager, just add a post locally
	    ?dbg("add_trace: trace ~p found for client ~p.",
			[Trace, _Other]),
	    {ok, 
	     [#trace_item {trace = Trace, lager_ref = LagerRef, client = Client}
	      | TL]}
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec remove_trace({Filter::term(), Level::atom(), File::string()},
		Client::pid(), TL::list(tuple())) ->
		       list(tuple()).

remove_trace(Trace, Client, TL) ->		
    %% See if we are tracing this.
    ?dbg("remove_trace: trace ~p for client ~p.",[Trace, Client]),
    case lists:keytake(Trace, #trace_item.trace, TL) of
	false ->
	    %% Ignore ??
	    ?dbg("remove_trace: trace ~p not found in ~p.",[Trace, TL]),
	    {ok, TL};
	{value, 
	 #trace_item {trace = Trace, lager_ref = LagerRef, client = Client}, 
	 Rest} ->
	    %% See if it was the last trace of this
	    case lists:keyfind(Trace, #trace_item.trace, Rest) of
		false ->
		    %% Last trace, remove in lager and locally
		    ?dbg("remove_trace: last trace ~p found.",[Trace]),
		    case lager:stop_trace(LagerRef) of
			ok ->
			    {ok, Rest};
			{error, _Reason} = E ->
			    ?dbg("add_trace: lager call failed, reason ~p.",
				 [_Reason]),
			    {E, TL}
		    end;
		#trace_item {} ->
		    %% We still need this trace in lager,
		    %% only remove locally
		    ?dbg("remove_trace: more traces ~p exist.",[Trace]),
		    {ok, Rest}
	    end;
	{value, #trace_item {trace = Trace, client = _Other}, _Rest} ->
	    ?dbg("remove_trace: trace ~p found for client ~p.",
			[Trace, _Other]),
	    {ok, TL}
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec monitor_client(Client::pid(), CL::list(pid())) ->
		       list(pid()).

monitor_client(Client, CL) ->		
    %% See if we already have this client
    case lists:keyfind(Client, #client_item.pid, CL) of
	false ->
	    %% Monitor the client
	    ?dbg("monitor_client: new client pid ~p.",[Client]),
	    Mon = erlang:monitor(process, Client),
	    [#client_item {pid = Client, monitor = Mon} | CL];
	#client_item {pid = Client} ->
	    ?dbg("monitor_client: old client pid ~p.",[Client]),
	    CL
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec demonitor_client(Client::pid(), CL::list(pid()), TL::list(tuple())) ->
		       list(pid()).

demonitor_client(Client, TL, CL) -> 
    %% See if we have this client
    case lists:keytake(Client, #client_item.pid, CL) of
	false ->
	    %% Ignore
	    ?dbg("demonitor_client: client pid ~p not found.",[Client]),
	    CL;
	{value, #client_item {pid = Client, monitor = Mon}, Rest} ->
	    %% See if this client has any traces left
	    case lists:keyfind(Client, #trace_item.client, TL) of
		false ->
		    %% No more traces
		    %% Demonitor the client
		    ?dbg("demonitor_client: last trace for client ~p.",
				[Client]),
		    erlang:demonitor(Mon, [flush]),
		    Rest;
		#trace_item {} ->
		    %% Continue monitoring
		    ?dbg("demonitor_client: more traces for client ~p.",
				[Client]),
		    CL
	    end
    end.
 

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec remove_traces(Client::pid(), TL::list(tuple()), NewTL::list(tuple())) ->
		       list(tuple()).

remove_traces(_Client, [], NewTL) ->
    ?dbg("remove_traces: all traces for client ~p removed.",[_Client]),
    NewTL;
remove_traces(Client, 
	      [#trace_item {trace = Trace, lager_ref = LagerRef, client = Client} 
	       | RestTL], NewTL) ->
    %% See if it was the last trace of this
    case lists:keytake(Trace, #trace_item.trace, RestTL) of
	false ->
	    %% Last trace, remove in lager and locally
	    ?dbg("remove_traces:last trace for client ~p found.",
			[Client]),
	    lager:stop_trace(LagerRef),
	    remove_traces(Client, RestTL, NewTL);
	{value, _TraceItem, _RestOfRest} ->
	    %% We still need this trace in lager,
	    %% only remove locally
	    ?dbg("remove_traces: more traces for client ~p found.",
			[Client]),
	    remove_traces(Client, RestTL, NewTL)
    end;
remove_traces(Client, [Item | RestTL], NewTL) ->
    %% Not this client
    ?dbg("remove_traces: traces for other client found.",[]),
    remove_traces(Client, RestTL, [Item | NewTL]).

	
%% Internal debugging		     
stop_debug(undefined) ->
    undefined;
stop_debug(Dbg) ->
    lager:stop_trace(Dbg),
    undefined.

%% enable/disable module debug 
set_debug(false, Dbg) ->
    NewDbg = stop_debug(Dbg),
    lager:set_loglevel(lager_console_backend, info),
    {ok, NewDbg};
set_debug(true, undefined) ->
    lager:trace_console([{module,?MODULE}], debug);
set_debug(true, Dbg) -> 
    {ok, Dbg}.

