%% -*- mode: erlang; indent-tabs-mode: nil; -*-
%%=============================================================================
%%
%% Copyright (C) 2015, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/

-define(CONNECT_TIMEOUT, 5000).
-define(CALL_TIMEOUT, 5000).

-define(TYPE_REQUEST, 0).
-define(TYPE_RESPONSE, 1).
-define(TYPE_NOTIFY, 2).

-define(RPC_PORT, 8000).
