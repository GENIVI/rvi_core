%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%
%% Copyright (C) 2016, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(rvi_services).
-behaviour(gen_server).

-export([start_link/0]).
-export([register_services/0]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include_lib("lager/include/log.hrl").
-include_lib("exo/include/exo_url.hrl").

-record(st, {cs}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_services() ->
    gen_server:call(?MODULE, register_services).

init([]) ->
    {ok, #st{cs = rvi_common:get_component_specification()}}.


handle_call(register_services, _From, #st{cs = CS} = St) ->
    Svcs = configured_services(),
    ?debug("configured_services() -> ~p", [Svcs]),
    _ = [register_service(S, CS) || S <- Svcs],
    {reply, ok, St};
handle_call(Other, _, St) ->
    ?debug("unknown call (~p)", [Other]),
    {reply, {error, badarg}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

configured_services() ->
    setup:get_env(rvi_core, services, []).

register_service({SvcName, Module, URL}, CompSpec) ->
    Port = url_port(URL),
    {ok,_} = rvi_services_svc_sup:start_service(Port, Module),
    service_edge_rpc:register_service(CompSpec, SvcName, URL).

url_port("http" ++ _ = URI) ->
    #url{port = Port} = exo_url:parse(URI),
    Port;
url_port("HTTP" ++ _ = URI) ->
    #url{port = Port} = exo_url:parse(URI),
    Port.
