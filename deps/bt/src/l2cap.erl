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
%%% File    : l2cap.erl
%%% Author  : Tony Rogvall <tony@pbook.synap.se>
%%% Description : L2CAP wrapper
%%% Created :  19 Jul 2006 by Tony Rogvall <tony@pbook.synap.se>

-module(l2cap).

-export([open/2, close/1, send/2, listen/1, accept/1, accept/2]).

open(Address, Psm) ->
    bt_drv:l2cap_open(Address, Psm).

close(L2CAP) ->
    bt_drv:l2cap_close(L2CAP).

send(L2CAP, Data) ->
    bt_drv:l2cap_send(L2CAP, Data).

listen(Psm) ->
    bt_drv:l2cap_listen(Psm).

accept(ListenRef) ->
    bt_drv:l2cap_accept(ListenRef).

accept(ListenRef,Timeout) when Timeout==infinity; 
			       is_integer(Timeout),Timeout>0 ->
    bt_drv:l2cap_accept(ListenRef,Timeout).

