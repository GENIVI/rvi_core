%%
%% Copyright (C) 2015, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%
-module(rvi_routing).

-behaviour(gen_server).

%% API
-export([get_service_routes/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ROUTING_RULES, routing_rules).

-record(route, {
	  service_prefix = ""::string(),
	  proto_link_pairs = []:: list(tuple())
	 }).
			     
-record(st, {
	  routes= []:: list(#route{})
	 }).

%%%===================================================================
%%% API
%%%===================================================================

get_service_routes(Service) ->
    gen_server:call(?SERVER, { rvi_get_service_routes, Service }).



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, St} |
%%                     {ok, St, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([]) ->
    {ok, Routes } = application:get_env(rvi, ?ROUTING_RULES),

    {ok, #st {
	    routes = Routes
	   }}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, St) ->
%%                                   {reply, Reply, St} |
%%                                   {reply, Reply, St, Timeout} |
%%                                   {noreply, St} |
%%                                   {noreply, St, Timeout} |
%%                                   {stop, Reason, Reply, St} |
%%                                   {stop, Reason, St}
%% @end
%%--------------------------------------------------------------------
handle_call( { rvi_get_service_routes, Service }, _From, St) ->
    {reply, normalize_routes_(find_routes_(Service, St#st.routes), []), t};

handle_call(_Request, _From, St) ->
    Reply = ok,
    {reply, Reply, St}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, St) -> {noreply, St} |
%%                                  {noreply, St, Timeout} |
%%                                  {stop, Reason, St}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, St) ->
    {noreply, St}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, St) -> {noreply, St} |
%%                                   {noreply, St, Timeout} |
%%                                   {stop, Reason, St}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, St) ->
    {noreply, St}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, St) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _St) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process st when code is changed
%%
%% @spec code_change(OldVsn, St, Extra) -> {ok, NewSt}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


prefix_match_(_, [], Len) ->
    Len;

prefix_match_([], _, Len ) ->
    Len;

prefix_match_([ C1 | T1], [ C2 | T2 ], Len) when C1 =:= C2 ->
    prefix_match_(T1, T2, Len +1);

prefix_match_(_,_, Len) ->
    Len.

find_routes_([], _Service, CurRoutes, CurMatchLen ) ->
    { CurRoutes, CurMatchLen };

find_routes_([ { ServicePrefix, Routes } | T], Service, CurRoutes, CurMatchLen ) ->
    MatchLen = prefix_match_(Service, ServicePrefix, 0),

    %% Do we have a better match than previosly recorded?
    case MatchLen >= CurMatchLen of
	true -> 
	    %% Continue with the new routes and matching len installed
	    find_routes_(T, Service, Routes, MatchLen);
	
	false ->
	    %% Continue with the old routes and matching len installed
	    find_routes_(T, Service, CurRoutes, CurMatchLen)
    end.
    
find_routes_(Routes, Service) ->
    case find_routes_(Routes, Service, undefined, 0) of
	{ undefined, 0 } ->
	    not_found;

	{ Routes, _MatchLen } ->
	    Routes
    end.


normalize_routes_({ServicePrefix, []}, Acc) ->
    { ServicePrefix, lists:reverse(Acc) };

normalize_routes_({ServicePrefix, [ {{ Pr, PrOp }, { DL, DLOp }} | Rem ]}, Acc) ->
    normalize_routes_({ServicePrefix, Rem}, [ { {Pr, PrOp}, { DL, DLOp } } | Acc]);  

normalize_routes_({ServicePrefix, [ { Pr, { DL, DLOp }} | Rem ]}, Acc) ->
    normalize_routes_({ServicePrefix, Rem}, [ { {Pr, []}, { DL, DLOp } } | Acc]);  

normalize_routes_({ServicePrefix, [ {{ Pr, PrOp}, DL } | Rem ]}, Acc) ->
    normalize_routes_({ServicePrefix, Rem}, [ { {Pr, PrOp}, { DL, [] } } | Acc]);  

normalize_routes_({ServicePrefix, [ {{ Pr, PrOp }, DL} | Rem ]}, Acc) ->
    normalize_routes_({ServicePrefix, Rem}, [ { {Pr, PrOp}, { DL, [] } } | Acc]).

