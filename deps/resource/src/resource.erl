%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Resource event
%%% @end
%%% Created :  2 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(resource).

-export([notify_when_destroyed/2]).
-on_load(init/0).

init() ->
    Nif = filename:join(code:priv_dir(resource), "resource_nif"),
    erlang:load_nif(Nif, 0).


notify_when_destroyed(_Pid,_Message) ->
    erlang:error(nif_not_loaded).

    
