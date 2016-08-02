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
%%%    Hyper text transport protocol
%%% @end
%%% Created : 16 Dec 2011 by Tony Rogvall <tony@rogvall.se>

-module(exo_http).

-include("../include/exo_http.hrl").

%% simple client interface
-export([wget/1, wget/2, wget/3, wget/4]).
-export([wpost/2, wpost/3, wpost/4, wpost/5, wpost_body/2]).
-export([wxget/3, wxget/4, wxget/5, wxget/6]).
-export([woptions/1, woptions/2, woptions/3, woptions/4]).

-export([wtrace/1, wtrace/2, wtrace/3, wtrace/4]).
-export([open/1, open/2, close/3]).
-export([request/2, request/3, request/4, request/5]).
-export([send/2, send/3, send/4, send/7,
	 send_body/2, send_chunk/2, send_chunk_end/2]).

%% message interface
-export([recv_response/1, recv_response/2,
	 recv_request/1, recv_request/2,
	 recv_body/2,
	 recv_body/3,
	 recv_body_eof/1,
	 recv_body_data/2,
	 recv_body_chunks/1,
	 recv_body_chunks/2,
	 recv_headers/2,
	 recv_headers/3
	]).

%% parse interface
-export([convert_uri/1]).
-export([tokens/1]).

-export([set_chdr/3,
	 set_shdr/3]).

%% format interface
-export([format_response/1, format_response/3,
	 format_request/1, format_request/2, format_request/4,
	 format_query/1,
	 format_headers/1,
	 format_hdr/1,
	 fmt_chdr/1,
	 fmt_shdr/1,
	 make_request/4,
	 make_response/4,
	 auth_basic_encode/2,
	 url_encode/1,
	 make_headers/2
	]).

-import(lists, [map/2, reverse/1]).

-ifdef(debug).
-define(dbg(F, A), io:format((F), (A))).
-else.
-define(dbg(F, A), ok).
-endif.


%%
%% Perform a HTTP/1.1 GET
%%
wget(Url) ->
    wget(Url,{1,1}, [], infinity).

wget(Url, Hs) ->
    wget(Url, {1,1}, Hs, infinity).

wget(Url, Version, Hs) ->
    wget(Url, Version, Hs, infinity).

wget(Url, Version, Hs, Timeout) ->
    Req = make_request('GET',Url,Version,Hs),
    request(Req,[],Timeout).

%% Proxy version
wxget(Proxy,Port,Url) ->
    wxget(Proxy,Port,Url,{1,1},[],infinity).

wxget(Proxy,Port,Url, Hs) ->
    wxget(Proxy,Port,Url, {1,1}, Hs,infinity).

wxget(Proxy,Port,Url, Version, Hs) ->
    wxget(Proxy,Port,Url, Version, Hs,infinity).

wxget(Proxy,Port,Url, Version, Hs,Timeout) ->
    Req = make_request('GET',Url,Version,Hs),
    xrequest(Proxy,Port,Req,[],Timeout).

%%
%% HTTP/1.1 OPTIONS
%%
woptions(Url) ->
    woptions(Url,{1,1},[],infinity).
woptions(Url, Hs) ->
    woptions(Url,{1,1},Hs,infinity).

woptions(Url, Version, Hs) ->
    woptions(Url, Version, Hs,infinity).

woptions(Url, Version, Hs, Timeout) ->
    Req = make_request('OPTIONS',Url,Version,Hs),
    request(Req,[],Timeout).

%%
%% HTTP/1.1 TRACE
%%
wtrace(Url) ->
    wtrace(Url,{1,1},[],infinity).

wtrace(Url, Hs) ->
    wtrace(Url,{1,1},Hs,infinity).

wtrace(Url, Version, Hs) ->
    wtrace(Url, Version, Hs, infinity).

wtrace(Url, Version, Hs,Timeout) ->
    Req = make_request('TRACE',Url,Version,Hs),
    request(Req,[],Timeout).

%%
%% HTTP/1.1 POST
%% 1.  Content-type: application/x-www-form-urlencoded
%%       - Data = [{key,value}] => key=valye&...
%%       - Data = [{file,Name,FileName} | {binary,Name,<<bin>>} | <<bin>>
%%
%% 2.
%%     Content-type: multipart/form-data; boundary=XYZ
%%
%%     Content-type: multipart/<form>
%%
%%        - Data = [{file,ContentType,DispositionName,FileName}  |
%%                  {data,ContentType,DispositionName,<<bin>>} |
%%                  <<bin>>]
%%
%%
wpost(Url,Data) ->
    wpost(Url,{1,1},[],Data).

wpost(Url,Hs,Data) ->
    wpost(Url,{1,1},Hs,Data,infinity).

wpost(Url,Version,Hs,Data) ->
    wpost(Url,Version,Hs,Data,infinity).

wpost(Url,Version,Hs,Data,Timeout) ->
    Req = make_request('POST',Url,Version,Hs),
    {ok,Req1,Body} = wpost_body(Req, Data),
    request(Req1, Body, Timeout).

wpost_body(Req, Data) ->
    Headers = Req#http_request.headers,
    case Headers#http_chdr.content_type of
	undefined ->
	    wpost_form_body(Req, Data);
	"application/x-www-form-urlencoded" ->
	    wpost_form_body(Req, Data);
	"multipart/"++_ ->
	    wpost_multi_body(Req, Data);
	_ ->
	    wpost_plain_body(Req, Data)
    end.


wpost_form_body(Req, Data) ->
    {ok,Req,format_query(Data)}.

wpost_multi_body(Req, Data) ->
    H = Req#http_request.headers,
    Ct0 = H#http_chdr.content_type,
    {Boundary,Req1} =
	case string:str(Ct0, "boundary=") of
	    0 ->
		{MS,S,US} = now(),
		Bnd = integer_to_list((1000000*(1000000*MS+S) + US)),
		Ct1 = H#http_chdr.content_type ++
		    "; boundary=\""++Bnd ++"\"",
		H1 = set_chdr('Content-Type', Ct1, H),
		{Bnd, Req#http_request { headers = H1 }};
	    I ->
		Str = string:sub_string(Ct0, I, length(Ct0)),
		["boundary", QBnd | _] = string:tokens(Str, " ;="),
		{unquote(QBnd), Req}
	end,
    {ok,Req1,multi_data(Data, Boundary)}.


unquote([$" | Str]) ->
    case reverse(Str) of
	[$" | RStr] -> reverse(RStr);
	_ -> Str
    end;
unquote(Str) -> Str.


wpost_plain_body(Req, Data) ->
    Body = case Data of
	       Bin when is_binary(Bin) ->
		   Bin;
	       [{file,_,FileName}] ->
		   {ok,Bin} = file:read_file(FileName),
		   Bin;
	       [{file,_,_,FileName}] ->
		   {ok,Bin} = file:read_file(FileName),
		   Bin;
	       [{data,_,Bin}] ->
		   Bin;
	       [{data,_,_,Bin}] ->
		   Bin;
	       List when is_list(List) ->
		   iolist_to_binary(List)
	   end,
    {ok,Req,Body}.


multi_data(Data, Boundary) ->
    iolist_to_binary(
      [
     lists:map(
       fun(Bin) when is_binary(Bin) ->
	       [
		"--",Boundary,?CRNL,
		"Content-Type: text/plain",?CRNL,
		"Content-Transfer-Encoding: 8bit",?CRNL,
		?CRNL,
		Bin,
		?CRNL
	       ];
	  ({file,ContentType,FileName}) ->
	       {ok,Bin} = file:read_file(FileName),
	       [
		"--",Boundary,?CRNL,
		"Content-Type: ",ContentType,?CRNL,
		"Content-Transfer-Encoding: 8bit",?CRNL,
		?CRNL,
		Bin,
		?CRNL
	       ];
	  ({file,ContentType,DispositionName,FileName}) ->
	       {ok,Bin} = file:read_file(FileName),
	       [
		"--",Boundary,?CRNL,
		"Content-Type: ",ContentType,?CRNL,
		"Content-Disposition: filename=\"",DispositionName,"\"",?CRNL,
		"Content-Transfer-Encoding: 8bit",?CRNL,
		?CRNL,
		Bin,
		?CRNL
	       ];
	  ({data,ContentType,DispositionName,Bin}) ->
	       [
		"--",Boundary,?CRNL,
		"Content-Type: ",ContentType,?CRNL,
		"Content-Disposition: filename=\"",DispositionName,"\"",?CRNL,
		"Content-Transfer-Encoding: 8bit",?CRNL,
		?CRNL,
		Bin,
		?CRNL
	       ];
	  ({data,[{_,_}|_] = Hdrs,Bin}) ->
	       [
		"--",Boundary,?CRNL,
		[format_field(K, V) || {K,V} <- Hdrs],
		?CRNL,
		Bin,
		?CRNL
	       ];
	  ({data,ContentType,Bin}) ->
	       [
		"--",Boundary,?CRNL,
		"Content-Type: ",ContentType,?CRNL,
		"Content-Transfer-Encoding: 8bit",?CRNL,
		?CRNL,
		Bin,
		?CRNL
	       ]
       end, Data),
       "--",Boundary,"--",?CRNL]).

request(Req, Body) ->
    request(Req, Body,infinity).

request(Req, Body,Timeout) ->
    case open(Req,Timeout) of
	{ok, S} ->
	    case request(S, Req, Body, false, Timeout) of
		{ok,Resp,RespBody} ->
		    close(S,Req,Resp),
		    {ok,Resp,RespBody};
		Error ->
		    exo_socket:close(S),
		    Error
	    end;
	Error ->
	    Error
    end.


xrequest(Proxy,Port,Req,Body,Timeout) ->
    Proto = case Req#http_request.uri of
		#url { scheme = http } -> [tcp,http];
		#url { scheme = https } -> [tcp,ssl,http];
		_ -> [tcp,http]
	    end,
    case exo_socket_cache:open(Proto,Req#http_request.version,
			       Proxy,Port,Timeout) of
	{ok,S} ->
	    exo_socket:setopts(S, [{mode,binary},{packet,http}]),
	    case request(S, Req, Body, true, Timeout) of
		{ok,Resp,RespBody} ->
		    close(S,Req,Resp),
		    {ok,Resp,RespBody};
		_Error ->
		    exo_socket:close(S)
	    end;
	Error ->
	    Error
    end.

request(S, Req, Body, Proxy) ->
    request(S, Req, Body, Proxy, infinity).

request(S, Req, Body, Proxy, Timeout) ->
    case send(S, Req, Body, Proxy) of
	ok ->
	    %% FIXME: take care of POST 100-continue
	    case recv_response(S, Timeout) of
		{ok, Resp} ->
		    ?dbg("response: ~p\n", [Resp]),
		    case recv_body(S, Resp, Timeout) of
			{ok,RespBody} ->
			    {ok,Resp,RespBody};
			Error ->
			    ?dbg("body: ~p\n", [Error]),
			    Error
		    end;
		Error ->
		    ?dbg("response: ~p\n", [Error]),
		    Error
	    end;
	Error -> Error
    end.

open(Request) ->
    open(Request,infinity).

open(Request,Timeout) ->
    URI = Request#http_request.uri,
    Url = if is_record(URI, url) -> URI;
	     is_list(URI) -> exo_url:parse(URI, sloppy)
	  end,
    Scheme = if Url#url.scheme == undefined -> http;
		true -> Url#url.scheme
	     end,
    Port = if Url#url.port == undefined ->
		   case Scheme of
		       http      -> 80;
		       https     -> 443;
		       ftp       -> 21
		   end;
	      true ->
		   Url#url.port
	   end,
    Proto = case Scheme of
		https -> [tcp,ssl,http];
		_ -> [tcp,http]
	    end,
    case exo_socket_cache:open(Proto,Request#http_request.version,
			       Url#url.host,Port,Timeout) of
	{ok,S} ->
	    exo_socket:setopts(S, [{mode,binary},{packet,http}]),
	    {ok,S};
	Error ->
	    Error
    end.

close(S, Req, Resp) ->
    case do_close(Req,Resp) of
	true ->
	    ?dbg("real close\n",[]),
	    exo_socket:close(S);
	false ->
	    ?dbg("session close\n",[]),
	    exo_socket_cache:close(S)
    end.

do_close(Req, Res) ->
    _ReqH = Req#http_request.headers,
    ResH = Res#http_response.headers,
    case tokens(ResH#http_shdr.connection) of
	["close"] -> true;
	["keep-alive"] ->
	    %% Check {1,0} and keep-alive requested
	    false;
	_ ->
	    case Req#http_request.version of
		{1,1} -> false;
		_ -> true
	    end
    end.

%%
%% Send the HTTP request on a open connection
%%
send(Socket, Request) ->
    send(Socket, Request, false).

send(Socket, Request, Proxy) ->
    send(Socket, Request, [], Proxy).

send(Socket, Request, Body, Proxy) ->
    send(Socket,
	 Request#http_request.method,
	 Request#http_request.uri,
	 Request#http_request.version,
	 Request#http_request.headers,
	 Body, Proxy).

send(Socket, Method, URI, Version, H, Body, Proxy) ->
    Url = if is_record(URI, url) -> URI;
	     is_list(URI) -> exo_url:parse(URI, sloppy)
	  end,
    %% FIXME: why is not port used?
    _Port = if Url#url.port == undefined ->
		    case Url#url.scheme of
			undefined -> 80;
			http      -> 80;
			https     -> 443;
			ftp       -> 21
		    end;
	       true ->
		    Url#url.port
	    end,
    H1 =
	if H#http_chdr.host == undefined ->
		H#http_chdr { host = Url#url.host };
	   true ->
		H
	end,
    H2 = if is_binary(Body), size(Body) > 0,
	    H1#http_chdr.content_length == undefined ->
		 H1#http_chdr { content_length = size(Body) };
	    is_list(Body), Body =/= [],
	    H1#http_chdr.content_length == undefined ->
		 H1#http_chdr { content_length = lists:flatlength(Body) };
	    true ->
		 H1
	 end,
    H3 = if Version == {1,0},
	    H1#http_chdr.connection == undefined ->
		 H2#http_chdr { connection = "keep-alive" };
	    true ->
		 H2
	 end,
    Request = [format_request(Method,Url,Version,Proxy),?CRNL,
	       format_hdr(H3),?CRNL, Body],
    ?dbg("> ~p\n", [Request]),
    exo_socket:send(Socket, Request).

%%
%% Send "extra" body data not sent in the original send
%%
send_body(Socket, Body) ->
    exo_socket:send(Socket, Body).

%%
%% Send chunks
%%
send_chunk(Socket, Chunk) when is_binary(Chunk) ->
    Sz = size(Chunk),
    if Sz > 0 ->
	    ChunkSize = erlang:integer_to_list(Sz,16),
	    ChunkExt = "",
	    exo_socket:send(Socket, [ChunkSize,ChunkExt,?CRNL,Chunk,?CRNL]);
       Sz == 0 ->
	    ok
    end.

send_chunk_end(Socket, _Trailer) ->
    ChunkSize = "0",
    ChunkExt = "",
    exo_socket:send(Socket, [ChunkSize, ChunkExt, ?CRNL,
			     %% Trailer is put here
			     ?CRNL]).

%%
%% Receive a http/https request
%%
recv_request(S) ->
    recv_request(S, infinity).

recv_request(S, Timeout) ->
    case exo_socket:recv(S, 0, Timeout) of
	{ok, {http_request, Method, Uri, Version}} ->
	    CUri = convert_uri(Uri),
	    recv_headers(S, #http_request { method = Method,
					    uri    = CUri,
					    version = Version });
	{ok, Data} ->
	    io:format("Request data: ~p\n", [Data]),
	    {error, sync_error };
	{error, {http_error, ?CRNL}} -> recv_request(S);
	{error, {http_error, ?NL}} -> recv_request(S);
	Error ->
	    Error
    end.

%%
%% Receive a http/https response
%%
recv_response(S) ->
    recv_response(S, infinity).

recv_response(S,Timeout) ->
    case exo_socket:recv(S, 0, Timeout) of
	{ok, {http_response, Version, Status, Phrase}} ->
	    recv_headers(S, #http_response { version = Version,
					      status = Status,
					      phrase = Phrase },Timeout);
	{ok, _} ->
	    {error, sync_error };
	{error, {http_error, ?CRNL}} -> recv_response(S,Timeout);
	{error, {http_error, ?NL}} -> recv_response(S,Timeout);
	Error ->
	    Error
    end.

%%
%% Receive a body for a request or a response
%%
recv_body(S, R) ->
    recv_body(S, R, infinity).

recv_body(S, Request, Timeout) when is_record(Request, http_request) ->
    Method = Request#http_request.method,
    if Method == 'POST' ->
	    H = Request#http_request.headers,
	    case Request#http_request.version of
		{0,9} ->
		    recv_body_eof(S, Timeout);
		{1,0} ->
		    case H#http_chdr.content_length of
			undefined -> recv_body_eof(S,Timeout);
			Len -> recv_body_data(S, list_to_integer(Len),Timeout)
		    end;
		{1,1} ->
		    case H#http_chdr.content_length of
			undefined ->
			    case H#http_chdr.transfer_encoding of
				undefined -> recv_body_eof(S,Timeout);
				"chunked" -> recv_body_chunks(S,Timeout)
			    end;
			Len -> recv_body_data(S, list_to_integer(Len),Timeout)
		    end
	    end;
       %% FIXME: handle GET/XXX with body
       true ->
	    {ok, <<>>}
    end;
recv_body(S, Response,Timeout) when is_record(Response, http_response) ->
    %% version 0.9  => read until eof
    %% version 1.0  => read either Content-Length or until eof
    %% version 1.1  => read Content-Length or Chunked or eof
    H = Response#http_response.headers,
    case Response#http_response.version of
	{0,9} ->
	    recv_body_eof(S,Timeout);
	{1,0} ->
	    case H#http_shdr.content_length of
		undefined -> recv_body_eof(S,Timeout);
		Len -> recv_body_data(S, list_to_integer(Len),Timeout)
	    end;
	{1,1} ->
	    case H#http_shdr.content_length of
		undefined ->
		    case H#http_shdr.transfer_encoding of
			undefined -> recv_body_eof(S,Timeout);
			"chunked" -> recv_body_chunks(S,Timeout)
		    end;
		Len -> recv_body_data(S, list_to_integer(Len),Timeout)
	    end
    end.

recv_body_eof(Socket) ->
    recv_body_eof(Socket,infinity).

recv_body_eof(Socket,Timeout) ->
    ?dbg("RECV_BODY_EOF: tmo=~w\n", [Timeout]),
    exo_socket:setopts(Socket, [{packet,raw},{mode,binary}]),
    recv_body_eof1(Socket, [], Timeout).

recv_body_eof1(Socket, Acc,Timeout) ->
    case exo_socket:recv(Socket, 0, Timeout) of
	{ok, Bin} ->
	    recv_body_eof1(Socket, [Bin|Acc],Timeout);
	{error, closed} ->
	    {ok, iolist_to_binary(reverse(Acc))};
	Error ->
	    Error
    end.

recv_body_data(Socket, Len) ->
    recv_body_data(Socket, Len, infinity).

recv_body_data(_Socket, 0, _Timeout) ->
    ?dbg("RECV_BODY_DATA: len=0, tmo=~w\n", [_Timeout]),
    {ok, <<>>};
recv_body_data(Socket, Len, Timeout) ->
    ?dbg("RECV_BODY_DATA: len=~p, tmo=~w\n", [Len,Timeout]),
    exo_socket:setopts(Socket, [{packet,raw},{mode,binary}]),
    case exo_socket:recv(Socket, Len, Timeout) of
	{ok, Bin} ->
	    exo_socket:setopts(Socket, [{packet,http}]),
	    {ok,Bin};
	Error ->
	    Error
    end.


recv_body_chunks(Socket) ->
    recv_body_chunks(Socket, infinity).

recv_body_chunks(Socket, Timeout) ->
    exo_socket:setopts(Socket, [{packet,line},{mode,list}]),
    ?dbg("RECV_BODY_CHUNKS: tmo=~w\n", [Timeout]),
    recv_body_chunk(Socket, [], Timeout).

recv_body_chunk(S, Acc, Timeout) ->
    case exo_socket:recv(S, 0, Timeout) of
	{ok,Line} ->
	    ?dbg("CHUNK-Line: ~p\n", [Line]),
	    {ChunkSize,_Ext} = chunk_size(Line),
	    ?dbg("CHUNK: ~w\n", [ChunkSize]),
	    if ChunkSize == 0 ->
		    exo_socket:setopts(S, [{packet,httph}]),
		    case recv_chunk_trailer(S, [], Timeout) of
			{ok,_TR} ->
			    ?dbg("CHUNK TRAILER: ~p\n", [_TR]),
			    exo_socket:setopts(S, [{packet,http},
						   {mode,binary}]),
			    {ok, iolist_to_binary(reverse(Acc))};
			Error ->
			    Error
		    end;
	       ChunkSize > 0 ->
		    exo_socket:setopts(S, [{packet,raw},{mode,binary}]),
		    case exo_socket:recv(S, ChunkSize, Timeout) of
			{ok,Bin} ->
			    exo_socket:setopts(S, [{packet,line},{mode,list}]),
			    case exo_socket:recv(S, 0, Timeout) of
				{ok, ?NL} ->
				    recv_body_chunk(S,[Bin|Acc],Timeout);
				{ok, ?CRNL} ->
				    recv_body_chunk(S,[Bin|Acc],Timeout);
				{ok, _Data} ->
				    ?dbg("out of sync ~p\n", [_Data]),
				    {error, sync_error};
				Error ->
				    Error
			    end;
			Error ->
			    Error
		    end
	    end;
	Error ->
	    Error
    end.


recv_chunk_trailer(S, Acc, Timeout) ->
    case exo_socket:recv(S, 0, Timeout) of
	{ok,{http_header,_,K,_,V}} ->
	    recv_chunk_trailer(S,[{K,V}|Acc],Timeout);
	{ok,http_eoh} ->
	    {ok, reverse(Acc)};
	Error ->
	    Error
    end.

recv_headers(S, R) ->
    recv_headers(S, R, infinity).

recv_headers(S, R, Timeout) ->
    if is_record(R, http_request) ->
	    recv_hc(S, R, #http_chdr { },Timeout);
       is_record(R, http_response) ->
	    recv_hs(S, R, #http_shdr { },Timeout)
    end.


recv_hc(S, R, H, Timeout) ->
    case exo_socket:recv(S, 0, Timeout) of
	{ok, Hdr} ->
	    case Hdr of
		http_eoh ->
		    ?dbg("EOH <\n", []),
		    Other = reverse(H#http_chdr.other),
		    H1 = H#http_chdr { other = Other },
		    R1 = R#http_request { headers = H1 },
		    ?dbg("< ~s~s\n", [format_request(R1,true),
				      format_headers(fmt_chdr(H1))]),
		    {ok, R1};
		{http_header,_,K,_,V} ->
		    ?dbg("HEADER < ~p ~p\n", [K, V]),
		    recv_hc(S,R,set_chdr(K,V,H), Timeout);
		Got ->
		    {error, Got}
	    end;
	{error, {http_error, ?CRNL}} ->
	    ?dbg("ERROR CRNL <\n", []),
	    recv_hc(S, R, H,Timeout);
	{error, {http_error, ?NL}} ->
	    ?dbg("ERROR NL <\n", []),
	    recv_hc(S, R, H,Timeout);
	Error -> Error
    end.

recv_hs(S, R, H, Timeout) ->
    case exo_socket:recv(S, 0, Timeout) of
	{ok, Hdr} ->
	    case Hdr of
		http_eoh ->
		    ?dbg("EOH <\n", []),
		    Other = reverse(H#http_shdr.other),
		    H1 = H#http_shdr { other = Other },
		    R1 = R#http_response { headers = H1 },
		    ?dbg("< ~s~s\n", [format_response(R1),
				      format_hdr(H1)]),
		    {ok, R1};
		{http_header,_,K,_,V} ->
		    ?dbg("HEADER < ~p ~p\n", [K, V]),
		    recv_hs(S,R,set_shdr(K,V,H),Timeout);
		Got ->
		    {error, Got}
	    end;
	{error, {http_error, ?CRNL}} ->
	    ?dbg("ERROR CRNL <\n", []),
	    recv_hs(S, R, H,Timeout);
	{error, {http_error, ?NL}} ->
	    ?dbg("ERROR NL <\n", []),
	    recv_hs(S, R, H, Timeout);
	Error -> Error
    end.


make_request(Method, Url, Version, Hs) ->
    U = exo_url:parse(Url, sloppy),
    #http_request { method = Method,
		    uri = U,
		    version = Version,
		    headers = mk_chdr(Hs) }.

make_response(Version, Status, Phrase, Hs) ->
    #http_response { version = Version,
		     status = Status,
		     phrase = Phrase,
		     headers = mk_shdr(Hs)}.

%%
%% Format http_request
%%
format_request(R) ->
    format_request(R, false).

format_request(R, Proxy) ->
    format_request(R#http_request.method,
		   R#http_request.uri,
		   R#http_request.version,
		   Proxy).

format_request(Method, Url, Version, Proxy) ->
    [if is_atom(Method) -> atom_to_list(Method);
	is_list(Method) -> Method
     end,
     " ",
     if is_record(Url, url) ->
	     if Proxy == true ->
		     exo_url:format(Url);
		true ->
		     exo_url:format_path(Url)
	     end;
	is_list(Url) -> Url
     end,
     case Version of
	 {0,9} ->  "";
	 {1,0} ->  " HTTP/1.0";
	 {1,1}  -> " HTTP/1.1"
     end].

format_response(R) ->
    format_response(R#http_response.version,
		    R#http_response.status,
		    R#http_response.phrase).

format_response({0,9}, _Status, _Phrase) -> "";
format_response(Version, Status, Phrase) ->
    [case Version of
	{1,0} ->  "HTTP/1.0";
	{1,1}  -> "HTTP/1.1"
     end,
     " ", integer_to_list(Status),
     case Phrase of
	 "" -> "";
	 _ -> [$\s|Phrase]
     end
    ].

format_query([Item]) ->
    case Item of
	{Key,Value} ->
	    [url_encode(to_list(Key)),"=",url_encode(to_list(Value))];
	Key ->
	    url_encode(to_list(Key))
    end;
format_query([Item|Vs]) ->
    case Item of
	{Key,Value} ->
	    [url_encode(to_list(Key)),"=",url_encode(to_list(Value)),"&" |
	     format_query(Vs)];
	Key ->
	    [url_encode(to_list(Key)), "&" |
	     format_query(Vs)]
    end;
format_query([]) ->
    [].

%%
%% Encode basic authorization
%%
auth_basic_encode(User,undefined) ->
    base64:encode_to_string(to_list(User)++":");
auth_basic_encode(User,Pass) ->
    base64:encode_to_string(to_list(User)++":"++to_list(Pass)).

make_headers(undefined, _Pass) -> [];
make_headers(User, Pass) ->
    [{"Authorization", "Basic "++auth_basic_encode(User, Pass)}].

%%
%% Url encode a string
%%
url_encode([C|T]) ->
    if C >= $a, C =< $z ->  [C|url_encode(T)];
       C >= $A, C =< $Z ->  [C|url_encode(T)];
       C >= $0, C =< $9 ->  [C|url_encode(T)];
       C == $\s         ->  [$+|url_encode(T)];
       C == $_; C == $.; C == $-; C == $/; C == $: -> % FIXME: more..
	    [C|url_encode(T)];
       true ->
	    case erlang:integer_to_list(C, 16) of
		[C1]   -> [$%,$0,C1 | url_encode(T)];
		[C1,C2] ->[$%,C1,C2 | url_encode(T)]
	    end
    end;
url_encode([]) ->
    [].

to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.

convert_uri({abs_path, Path}) ->
    #url{ path = Path };
convert_uri({absoluteURI, Scheme, Host, Port, Path}) ->
    exo_url:parse_path(#url{ scheme = Scheme,host = Host, port = Port}, Path);
convert_uri({scheme, Scheme, Request}) ->
    #url{ scheme = Scheme, path = Request }.

format_field(Key,Value) ->
    K = if is_atom(Key) -> atom_to_list(Key);
	   is_list(Key) -> Key;
	   is_binary(Key) -> Key
	end,
    V = if is_integer(Value) -> integer_to_list(Value);
	   is_atom(Value) -> atom_to_list(Value);
	   is_list(Value) -> Value;
	   is_binary(Value) -> Value
	end,
    [K,": ",V,"\r\n"].

format_headers([{Key,Value}|Hs]) ->
    [format_field(Key,Value) | format_headers(Hs)];
format_headers([]) ->
    [].


mk_shdr(Hs) ->
    mk_shdr(Hs, #http_shdr { }).

mk_shdr([{K,V}|Hs], H) ->
    mk_shdr(Hs, set_shdr(K,V,H));
mk_shdr([], H) ->
    H.

set_shdr(K,V,H) ->
    case K of
	'Connection'        -> H#http_shdr { connection = V };
	'Transfer-Encoding' -> H#http_shdr { transfer_encoding = V };
	'Location'          -> H#http_shdr { location = V };
	'Set-Cookie'        -> H#http_shdr { set_cookie = V };
	'Content-Length'    -> H#http_shdr { content_length = V };
	'Content-Type'      -> H#http_shdr { content_type = V };
	_ ->
	    Hs = [{K,V} | H#http_shdr.other],
	    H#http_shdr { other = Hs }
    end.

mk_chdr(Hs) ->
    mk_chdr(Hs, #http_chdr { }).

mk_chdr([{K,V}|Hs], H) ->
    mk_chdr(Hs, set_chdr(K,V,H));
mk_chdr([], H) ->
    H.

set_chdr(K,V,H) ->
    case K of
	'Host'   -> H#http_chdr { host = V };
	'Connection' -> H#http_chdr { connection = V };
	'Transfer-Encoding' -> H#http_chdr { transfer_encoding = V };
	'Accept' -> H#http_chdr { accept = V };
	'If-Modified-Since' -> H#http_chdr { if_modified_since = V };
	'If-Match' -> H#http_chdr { if_match = V };
	'If-None-Match' -> H#http_chdr { if_none_match = V };
	'If-Range' -> H#http_chdr { if_range = V };
	'If-Unmodified-Since' -> H#http_chdr { if_unmodified_since = V };
	'Range' -> H#http_chdr { range = V };
	'Referer' -> H#http_chdr { referer = V };
	'User-Agent' -> H#http_chdr { user_agent = V };
	'Accept-Ranges' -> H#http_chdr { accept_ranges = V };
	'Cookie' ->
	    V1 = [V | H#http_chdr.cookie],
	    H#http_chdr { cookie = V1 };
	'Keep-Alive' -> H#http_chdr { keep_alive = V };
        'Content-Length' -> H#http_chdr { content_length = V };
        'Content-Type' -> H#http_chdr { content_type = V };
        'Authorization' -> H#http_chdr { authorization = V };
	_ ->
	    Hs = [{K,V} | H#http_chdr.other],
	    H#http_chdr { other = Hs }
    end.

format_hdr(H) when is_record(H, http_chdr) ->
    fcons('Host', H#http_chdr.host,
    fcons('Connection', H#http_chdr.connection,
    fcons('Transfer-Encoding', H#http_chdr.transfer_encoding,
    fcons('Accept', H#http_chdr.accept,
    fcons('If-Modified-Since', H#http_chdr.if_modified_since,
    fcons('If-Match', H#http_chdr.if_match,
    fcons('If-None-Match', H#http_chdr.if_none_match,
    fcons('If-Range', H#http_chdr.if_range,
    fcons('If-Unmodified-Since', H#http_chdr.if_unmodified_since,
    fcons('Range', H#http_chdr.range,
    fcons('Referer', H#http_chdr.referer,
    fcons('User-Agent', H#http_chdr.user_agent,
    fcons('Accept-Ranges', H#http_chdr.accept_ranges,
    fcons_list('Cookie', H#http_chdr.cookie,
    fcons('Keep-Alive', H#http_chdr.keep_alive,
    fcons('Content-Length', H#http_chdr.content_length,
    fcons('Content-Type', H#http_chdr.content_type,
    fcons('Authorization', H#http_chdr.authorization,
	  format_headers(H#http_chdr.other)))))))))))))))))));
format_hdr(H) when is_record(H, http_shdr) ->
    fcons('Connection', H#http_shdr.connection,
    fcons('Transfer-Encoding', H#http_shdr.transfer_encoding,
    fcons('Location', H#http_shdr.location,
    fcons('Set-Cookie', H#http_shdr.set_cookie,
    fcons('Content-Length', H#http_shdr.content_length,
    fcons('Content-Type', H#http_shdr.content_type,
	  format_headers(H#http_shdr.other))))))).


%%
%% Convert the http_chdr (client header) structure into a
%% key value list suitable for formatting.
%% returns [ {Key,Value} ]
%% Looks a bit strange, but is done this way to avoid creation
%% of garabge.
fmt_chdr(H) ->
    hcons('Host', H#http_chdr.host,
    hcons('Connection', H#http_chdr.connection,
    hcons('Transfer-Encoding', H#http_chdr.transfer_encoding,
    hcons('Accept', H#http_chdr.accept,
    hcons('If-Modified-Since', H#http_chdr.if_modified_since,
    hcons('If-Match', H#http_chdr.if_match,
    hcons('If-None-Match', H#http_chdr.if_none_match,
    hcons('If-Range', H#http_chdr.if_range,
    hcons('If-Unmodified-Since', H#http_chdr.if_unmodified_since,
    hcons('Range', H#http_chdr.range,
    hcons('Referer', H#http_chdr.referer,
    hcons('User-Agent', H#http_chdr.user_agent,
    hcons('Accept-Ranges', H#http_chdr.accept_ranges,
    hcons_list('Cookie', H#http_chdr.cookie,
    hcons('Keep-Alive', H#http_chdr.keep_alive,
    hcons('Content-Length', H#http_chdr.content_length,
    hcons('Content-Type', H#http_chdr.content_type,
    hcons('Authorization', H#http_chdr.authorization,
	  H#http_chdr.other)))))))))))))))))).

%% Convert the http_shdr (server header) structure into a
%% key value list suitable for formatting.
fmt_shdr(H) ->
    hcons('Connection', H#http_shdr.connection,
    hcons('Transfer-Encoding', H#http_shdr.transfer_encoding,
    hcons('Location', H#http_shdr.location,
    hcons('Set-Cookie', H#http_shdr.set_cookie,
    hcons('Content-Length', H#http_shdr.content_length,
    hcons('Content-Type', H#http_shdr.content_type,
	  H#http_shdr.other)))))).

hcons(_Key, undefined, Hs) -> Hs;
hcons(Key, Val, Hs) ->
    [{Key,Val} | Hs].

hcons_list(Key, [V|Vs], Hs) ->
    [{Key,V} | hcons_list(Key,Vs,Hs)];
hcons_list(_Key, [], Hs) ->
    Hs.

fcons(_Key, undefined, Hs) -> Hs;
fcons(Key, Val, Hs) ->
    [format_field(Key,Val) | Hs].

fcons_list(Key, [V|Vs], Hs) ->
    [format_field(Key,V) | fcons_list(Key,Vs,Hs)];
fcons_list(_Key, [], Hs) ->
    Hs.

%%
%% Parse chunk-size [ chunk-extension ] CRLF
%% return {chunk-size, chunk-extension}
%%
chunk_size(Line) ->
    chunk_size(Line, 0).

chunk_size([H|Hs], N) ->
    if
	H >= $0, H =< $9 ->
	    chunk_size(Hs, (N bsl 4)+(H-$0));
	H >= $a, H =< $f ->
	    chunk_size(Hs, (N bsl 4)+((H-$a)+10));
	H >= $A, H =< $F ->
	    chunk_size(Hs, (N bsl 4)+((H-$A)+10));
	H == $\r -> {N, ""};
	H == $\n -> {N, ""};
	H == $\s -> {N, Hs};
	H == $;  -> {N, [H|Hs]}
    end;
chunk_size([], N) ->
    {N, ""}.

tokens(undefined) ->
    [];
tokens(Line) ->
    string:tokens(string:to_lower(Line), ";").
