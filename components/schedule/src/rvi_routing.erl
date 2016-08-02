%%
%% Copyright (C) 2015, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%
-module(rvi_routing).

-behaviour(gen_server).


-include_lib("lager/include/log.hrl").

%% API
-export([get_service_routes/1]).
-export([get_service_protocols/2]).
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

%%
%% Retrieve all protocols matching the service / data link pair
get_service_protocols(Service, DataLink) ->
    gen_server:call(?SERVER, { rvi_get_protocols, Service, DataLink }).


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
    {ok, Routes } = application:get_env(rvi_core, ?ROUTING_RULES),

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
    {reply, find_routes( St#st.routes, Service), St};

handle_call( { rvi_get_protocols, Service, DataLink }, _From, St) ->
    {reply, find_protocols( St#st.routes, Service, DataLink), St};

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


%% We ran out of prefix chars.
%%  Service is a prefix match
prefix_match_(_Service, [], Len) ->
    Len;

%% We ran out of service chars.
%%  Service is shorter than prefix no match.
prefix_match_([], _Prefix, _Len ) ->
    -1;

%%
prefix_match_([ ServiceH | ServiceT], [ PrefixH | PrefixT ], Len)
  when ServiceH =:= PrefixH ->

    prefix_match_(ServiceT, PrefixT, Len + 1);

%% Mismatch between the the service and candidate. No match
prefix_match_(_Service, _Prefix, _Len) ->
    -1.

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
    end;

find_routes_(Rt, _Svc, CurRoutes, CurMatchLen) ->
    ?warning("rvi_routing(): Incorrect route entry: ~p", [Rt]),
    { CurRoutes, CurMatchLen }.

find_routes(Routes, Service) ->
    ?debug("find_routes(~p, ~p)", [Routes, Service]),
    case find_routes_(Routes, Service, undefined, 0) of
	{ undefined, 0 } ->
	    ?debug("rvi_routing(): ~p -> unknown", [ Service]),
	    []; %% No routes found

	{ MatchRoutes, _MatchLen } ->
	    ?debug("rvi_routing(): ~p -> ~p", [ Service, MatchRoutes ]),
	    normalize_routes_(MatchRoutes, [])
    end.


%% { Protocol,            DataLink }          -> { {Protocol, [] },   {DataLink, [] } },
%% { Protocol,            { DataLink, DOpts } -> { {Protocol, [] },   {DataLink, DOpts } },
%% { { Protocol, POpts }, DataLink }          -> { {Protocol, POpts}, {DataLink, [] }},
%% { { Protocol, POpts }, { DataLink, DOpts } -> { {Protocol, POpts}, {DataLink, DOpts }},
normalize_routes_([], Acc) ->
     lists:reverse(Acc);

normalize_routes_([ {{ Pr, PrOp }, { DL, DLOp }} | Rem ], Acc) ->
    normalize_routes_( Rem, [ {{Pr, PrOp}, { DL, DLOp } } | Acc]);

normalize_routes_([ { Pr, { DL, DLOp }} | Rem ], Acc) ->
    normalize_routes_(Rem, [ { {Pr, []}, { DL, DLOp } } | Acc]);

normalize_routes_([ {{ Pr, PrOp}, DL } | Rem ], Acc) ->
    normalize_routes_(Rem, [ { {Pr, PrOp}, { DL, [] } } | Acc]);

normalize_routes_([ {Pr, DL} | Rem ], Acc) ->
    normalize_routes_(Rem, [ { {Pr, []}, { DL, [] } } | Acc]);
normalize_routes_([H|T], Acc) ->
    ?error("Unrecognized routing rule: ~p", [H]),
    normalize_routes_(T, Acc);
normalize_routes_(Other, Acc) ->
    ?error("Unrecognized routing entry (expected list): ~p", [Other]),
    lists:reverse(Acc).



find_protocols_(_DataLink, [], Acc ) ->
    lists:reverse(Acc);


%% Matching data link. This is an allowed protocol
find_protocols_(DataLink, [ {{ Pr, PrOp }, { DL, DLOp }}  | T],
		Acc) when DataLink =:= DL ->

    find_protocols_(DataLink, T, [ { Pr, PrOp, DLOp } | Acc ]);


%% No match
find_protocols_(DataLink, [ {{ _Pr, _PrOp }, { _DL, _DLOp }}  | T], Acc) ->
    find_protocols_(DataLink, T,  Acc ).


find_protocols(AllRoutes, Service, DataLink) ->
    ?debug("find_protocols(~p, ~p)", [AllRoutes, Service]),
    SvcRoutes = find_routes(AllRoutes, Service),
    Res = find_protocols_(DataLink, SvcRoutes, []),
    ?debug("find_protocols(~p:~p): -> ~p", [ DataLink, Service, Res]),
    Res.
