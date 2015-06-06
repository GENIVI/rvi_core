%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2006 - 2014, Rogvall Invest AB, <tony@rogvall.se>
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
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% File    : bt_app.erl
%%% Author  : Tony Rogvall <tony@pbook.synap.se>
%%% Description : 
%%% Created :  2 Feb 2006 by Tony Rogvall <tony@pbook.synap.se>

-module(bt_app).

-behaviour(application).
-export([start/2,stop/1]).

%% start
start(_Type, _StartArgs) ->
    bt_sup:start_link().

%% stop FIXME
stop(_State) ->
  ok.
