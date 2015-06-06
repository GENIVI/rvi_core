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
%%% File    : obex_dm_server.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : OBEX SYNCML Server
%%% Created : 29 May 2006 by Tony Rogvall <tony@PBook.local>

-module(obex_sync_server).

-export([start/0, start/1]).
-export([init/3,
	 terminate/1,
	 handle_connect/2,
	 handle_disconnect/2,
	 handle_get/2,
	 handle_put/3,
	 handle_abort/2,
	 handle_setpath/3,
	 handle_command/2]).
-export([sdp_info/1]).

-include("../include/sdp.hrl").

-record(s,
	{
	  sref,
	  opts
	 }).

-ifdef(debug).
-define(dbg(Fmt,As), io:format("~s:~w:" Fmt "\n", [?FILE,?LINE | As])).
-else.
-define(dbg(Fmt,As), ok).
-endif.


sdp_info(Channel) ->
    Base1 = 16#0100,
    [{?ATTR_ServiceRecordHandle, {uint32, 65000}},   %% Should not be needed ?
     {?ATTR_ServiceClassIDList, 
      {sequence,[{uuid,?UUID_SyncMLServer}]}},
     {?ATTR_ProtocolDescriptorList,
      {sequence,[{sequence,[{uuid,?UUID_L2CAP}]},
		 {sequence,[{uuid,?UUID_RFCOMM},{uint8,Channel}]},
		 {sequence,[{uuid,?UUID_OBEX}]}]}},
     {?ATTR_BrowseGroupList,
      {sequence,[{uuid,?UUID_PublicBrowseGroup}]}},
     {?ATTR_LanguageBaseAttributeIDList,
      {sequence, [{uint16, ?LANGUAGE($e,$n)}, 
		  {uint16, ?ENCODING_UTF8}, 
		  {uint16, Base1}]}},
     {?ATTR_ServiceName+Base1, {text,"SyncMLServer"}}].


start() ->
    start(14).

start(Channel) ->
    %% NOTE: The Channel number is just a hint, the
    %%  number may be any in the range 1-30
    obex:server(Channel, [{transport,rfcomm},
			  {sdp, sdp_info(Channel)},
			  {target,{"SYNCML-SYNC",?MODULE}}]).

%%
%% Below is NOT server functions
%%  but the connection side of the server
%%


init(SrvRef, SrvOpts, _Mtu) ->
    ?dbg("init: ~p, opts=~p, peer_mtu=~p", [SrvRef, SrvOpts, _Mtu]),
    #s { sref = SrvRef, opts = SrvOpts}.

terminate(_S) ->
    ?dbg("termiate", []),
    ok.

handle_connect(Target,S) ->
    ?dbg("handle_connect: ~p", [Target]),
    ID = 1235,  %% FIXME
    {reply, {ok,[{connectionID,ID},{who,Target}]}, S}.

handle_disconnect(_Hs, S) ->
    ?dbg("handle_disconnect: ~p", [_Hs]),
    {reply, {ok,success}, S}.

handle_get(_Hs, S) ->
    ?dbg("handle_get: ~p", [_Hs]),
    {reply, {ok,[],<<"Hello">>}, S}.

handle_put(_Hs,<<>>,S) ->
    ?dbg("handle_put: ~p data=~p", [_Hs,<<>>]),
    {reply, {ok,success}, S};
handle_put(_Hs,_Data,S) ->
    ?dbg("handle_put: ~p data=~p", [_Hs,_Data]),
    {reply, continue, S}.

handle_abort(_Hs, S) ->
    ?dbg("handle_abort: ~p", [_Hs]),
    {reply, {ok,success}, S}.

handle_setpath(_Hs, _Args, S) ->
    ?dbg("handle_setpath: ~p args=~p", [_Hs,_Args]),
    {reply, {error,not_allowed}, S}.

handle_command(_Hs, S) ->
    ?dbg("handle_command: ~p", [_Hs]),
    {reply, {error,not_allowed}, S}.






    
    
    
    


     

