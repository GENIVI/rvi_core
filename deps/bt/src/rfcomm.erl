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
%%% File    : rfcomm.erl
%%% Author  : Tony Rogvall <tony@pbook.synap.se>
%%% Description : RFCOMM wrapper
%%% Created :  2 Feb 2006 by Tony Rogvall <tony@pbook.synap.se>

-module(rfcomm).

-export([open/2, 
	 close/1, 
	 send/2, 
	 listen/1, 
	 accept/1, 
	 accept/2, 
	 accept/3]).

open(Address, Channel) ->
    bt_drv:rfcomm_open(Address, Channel).

close(RFComm) ->
    bt_drv:rfcomm_close(RFComm).

send(RFComm, Data) ->
    bt_drv:rfcomm_send(RFComm, Data).

listen(Channel) ->
    bt_drv:rfcomm_listen(Channel).

accept(ListenRef) ->
    accept(ListenRef, infinity, self() ).

accept(ListenRef, Timeout) ->
    accept(ListenRef, Timeout, self()).

accept(ListenRef,Timeout, CtlPid) when Timeout==infinity; 
				       is_integer(Timeout),Timeout>0 ->
    bt_drv:rfcomm_accept(ListenRef,Timeout, CtlPid).

