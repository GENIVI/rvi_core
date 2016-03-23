%% -*- mode: erlang; indent-tabs-mode: nil; -*-
%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%
%% Copyright (C) 2016 Ulf Wiger. All rights reserved.
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%%---- END COPYRIGHT ---------------------------------------------------------

-ifndef(event).

-define(costly_event(E),
        case erlang:trace_info({MODULE,event,3}, traced) of
            {_, false} -> ok;
            _          -> event(?LINE, E, none)
        end).

-define(costly_event(E, S),
        case erlang:trace_info({MODULE,event,3}, traced) of
            {_, false} -> ok;
            _          -> event(?LINE, E, S)
        end).

-define(event(E), event(?LINE, E, none)).
-define(event(E, S), event(?LINE, E, S)).

-endif.  % ?event()
