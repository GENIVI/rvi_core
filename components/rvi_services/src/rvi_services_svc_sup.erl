%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%
%% Copyright (C) 2016, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(rvi_services_svc_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_service/2]).
-export([init/1]).

-include_lib("lager/include/log.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_service(Port, Module) ->
    ?debug("start_service(~p, ~p)", [Port, Module]),
    supervisor:start_child(
      ?MODULE, [Port, [{request_handler,
                        {exoport_exo_http, handle_body, [Module]}}
                      ]]).

init([]) ->
    {ok, { #{strategy  => simple_one_for_one,
             intensity => 5,
             period    => 10},
           [#{id => id,
              start => {exo_http_server, start_link, []},
              restart => permanent,
              shutdown => 5000,
              type => worker,
              modules => [exo_http_server]}
           ] }}.
