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
%%% File    : obex_generic.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Generic OBEX server
%%% Created : 31 May 2006 by Tony Rogvall <tony@PBook.local>

-module(obex_generic).

-export([init/3,
	 terminate/1,
	 handle_connect/2,
	 handle_disconnect/2,
	 handle_get/2,
	 handle_put/3,
	 handle_abort/2,
	 handle_setpath/3,
	 handle_command/2]).

-include_lib("kernel/include/file.hrl").

-record(s,
	{
	  path,   %% current path
	  id,     %% connection id
	  sref,   %% server reference
	  fd,     %% current file
	  opts,   %% server options
	  mtu     %% peer mtu
	 }).

-ifdef(debug).
-define(dbg(Fmt,As), io:format("~s:~w:" Fmt "\n", [?FILE,?LINE | As])).
-else.
-define(dbg(Fmt,As), ok).
-endif.

%%
%% init(SrvRef, SrvOpts) -> S
%%
init(SrvRef, SrvOpts, PeerMtu) ->
    ?dbg("init: ~p, opts=~p, peer_mtu=~p", [SrvRef, SrvOpts, PeerMtu]),
    {ok,Path} = file:get_cwd(),
    #s { path=Path, sref = SrvRef, opts = SrvOpts, mtu=PeerMtu }.

%%
%% Terminate
%%
terminate(S) ->
    ?dbg("termiate", []),
    if S#s.fd == undefined ->
	    ok;
       true ->
	    file:close(S#s.fd)
    end.
%%
%% handle_connect(Target,S) -> 
%%   {reply, {ok,ConnectHeaders}, S'}
%% | {reply, {error,Reason}, S'}
%%
handle_connect(Target,S) ->
    ?dbg("handle_connect: ~p", [Target]),
    if Target == undefined ->
	    {reply, {ok,[]}, S};
       true ->
	    ID = 1234,
	    {reply, {ok,[{connectionID,ID}, {who,Target}]}, 
	     S#s { id = ID }}
    end.

%%
%% handle_disconnect(Headers,S) ->
%%   {reply, {ok,success}, S'}
%% | {reply, {error,Reason}, S'}
%%
handle_disconnect(_Hs, S) ->
    ?dbg("handle_disconnect: ~p", [_Hs]),
    {reply, {ok,success}, S}.
    
%%
%% handle_get(Headers, S) -> 
%%
%%     {reply, {ok,Headers,Data}, S'}
%%   | {reply, {continue,Headers,Data}, S'}
%%   | {reply, {error,Reason}, S'}
%%
%% handle_get is called the first time after all request headers
%%  has been read.
%% After that handle_get will be until bodyEnd is returned
%% 
%%
handle_get(Hs, S) ->
    ?dbg("handle_get: ~p", [Hs]),
    if S#s.fd == undefined ->
	    get_init(Hs, S);
       true ->
	    get_continue(Hs, S)
    end.

%%
%%  handle_put(Headers, Data, S) ->
%%       {reply, continue, S'}
%%    |  {reply, {error,Reason}, S'}
%%    |  {reply, {ok,success}, S'}
%%
%% handle_put will be called after all request headers 
%%   (not including body nor bodyEnd) 
%% Data found among inital request headers are put in the Data field
%%
%% The last call will have Headers=[] and Data=<<>>
%%
handle_put(Hs, Data, S) ->
    ?dbg("handle_put: ~p data=~p", [Hs,Data]),
    if S#s.fd == undefined ->
	    put_init(Hs, Data, S);
       true ->
	    put_continue(Hs,Data,S)
    end.

%%
%%  handle_abort(Headers, S) ->
%%     {reply, {ok,success}, S}
%%   | {reply, {error,Reason}, S}
%%
handle_abort(_Hs, S) ->
    ?dbg("handle_abort: ~p", [_Hs]),
    if S#s.fd == undefined ->
	    {reply, {ok,success}, S};
       true ->
	    file:close(S#s.fd),
	    {reply, {ok,success}, S#s { fd = undefined} }
    end.

%%
%%  handle_setpath(Header, Args, S) ->
%%     {reply, {ok,success}, S}
%%   | {reply, {error,Reason}, S}
%%
handle_setpath(Hs, _Args, S) ->
    ?dbg("handle_setpath: ~p args=~p", [Hs,_Args]),
    %% FIXME: use Args to create path and do ..
    case lists:keysearch(name, 1, Hs) of
	false ->
	    {reply, {error, conflict}, S};
	{value,{_,Path}} ->
	    Path1 = filename:join(S#s.path, Path),
	    {reply, {ok, success}, S#s { path=Path1}}
    end.

%%
%% handle_command(Headers, S) ->
%%   {reply, {ok,sucess}, S'}
%% | {reply, {error,Reason}, S'}
%%
%% 
%%
handle_command(_Hs, S) ->
    ?dbg("handle_command: ~p", [_Hs]),
    {reply, {ok,success}, S}.

%%
%% Return first headers
%% Normally return things like:
%%       [{name, FileName},
%%        {type,MimeType},
%%        {length,Size},
%%        {time,IsoTimeStamp},
%%        {body,Data}, {bodyEnd,Data}]
%% 
get_init(Hs, S) ->
    case lists:keysearch(name, 1, Hs) of
	false ->
	    {reply, {error, conflict}, S};
	{value,{_,File}} ->
	    Path = filename:join(S#s.path, File),
	    case get_info(Path) of
		{ok,Info} ->
		    case file:open(Path, [read,binary]) of
			{ok,Fd} ->
			    Hs = [{name,File}|Info],
			    RdSz = data_mtu(Hs, S#s.mtu),
			    ?dbg("obex_generic: data_mtu: ~w\n", [RdSz]),
			    case file:read(Fd, RdSz) of
				eof ->
				    file:close(Fd),
				    {reply,{ok,Hs,<<>>},S};
				{ok,Data} ->
				    {reply,{continue,Hs,Data},S#s{fd=Fd}}
			    end;
			_Error ->
			    {reply,{error,bad_request}, S}
		    end;
		Error ->
		    {reply, Error, S}
	    end
    end.

%%
%% Continue 
%%
get_continue(_Hs, S) ->
    RdSz = data_mtu([], S#s.mtu),
    ?dbg("obex_generic: data_mtu: ~w\n", [RdSz]),
    case file:read(S#s.fd, RdSz) of
	eof ->
	    file:close(S#s.fd),
	    {reply,{ok,[],<<>>}, S#s { fd = undefined }};
	{ok,Data} ->
	    {reply,{continue,[],Data}, S}
    end.

%%
%% Get info headers
%%
get_info(File) ->
    case file:read_file_info(File) of
	{error,enoent} ->
	    {error,not_found};
	{error,eaccess} ->
	    {error, unauthorized};
	{ok,I} ->
	    if I#file_info.type =/= regular ->
		    {error, not_allowed};
	       I#file_info.access == none; I#file_info.access == write ->
		    {error, unauthorized};
	       true ->
		    {ok,[{length,I#file_info.size},
			 {time,obex:time_iso(I#file_info.mtime)}]}
	    end
    end.

%%
%% Calculate usable data MTU
%%
data_mtu(Hs, Mtu) ->
    Sz = obex:size_headers(Hs),
    %% remove 3 byte for data header and 3 byte for response header
    (Mtu - (Sz rem Mtu))-6.
    
%%
%% Put init: process initial put request
%%
put_init(Hs, Data, S) ->
    case lists:keysearch(name, 1, Hs) of
	false ->
	    {reply, {error, conflict}, S};
	{value, {_,File}} ->
	    Path = filename:join(S#s.path, File),
	    case file:open(Path, [write,binary]) of
		{ok,Fd} ->
		    put_write(Fd, Data, S);
		_Error ->
		    %% FIXME: check reason
		    {reply, {error,unauthorized}, S}
	    end
    end.

put_continue(_Hs, <<>>, S) ->
    file:close(S#s.fd),
    {reply, {ok,success}, S#s {fd = undefined}};
put_continue(_Hs, Data, S) ->
    put_write(S#s.fd, Data, S).
    
put_write(Fd, <<>>, S) ->
    {reply, continue, S#s { fd=Fd }};
put_write(Fd, Data, S) ->
    file:write(Fd, Data), %% FIXME: handle error
    {reply, continue, S#s { fd=Fd }}.


    
    

	


    


