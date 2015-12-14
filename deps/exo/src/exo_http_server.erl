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
%%% @author Marina Westman Lönne <malotte@malotte.net>
%%% @copyright (C) 2012, Feuerlabs, Inc. All rights reserved.
%%% @doc
%%%   Simple exo_http_server
%%% @end
%%% Created : 2010 by Tony Rogvall <tony@rogvall.se>

-module(exo_http_server).

-behaviour(exo_socket_server).

%% exo_socket_server callbacks
-export([init/2,
	 data/3,
	 close/2,
	 error/3]).

-export([control/4]).

-include("log.hrl").
-include("exo_socket.hrl").
-include("exo_http.hrl").

-record(state,
	{
	  request,
	  response,
	  access = [],
	  request_handler
	}).

%% Configurable start
-export([start/2,
	 start_link/2,
	 response/5, response/6]).

%% For testing
-export([test/0]).

%%-----------------------------------------------------------------------------
%% @doc
%%  Starts a socket server on port Port with server options ServerOpts
%% that are sent to the server when a connection is established,
%% i.e init is called.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec start(Port::integer(),
	    ServerOptions::list({Option::atom(), Value::term()})) ->
		   {ok, ChildPid::pid()} |
		   {error, Reason::term()}.

start(Port, ServerOptions) ->
    ?debug("exo_http_server: start: port ~p, server options ~p",
	   [Port, ServerOptions]),
    Dir = code:priv_dir(exo),
    exo_socket_server:start(Port, [tcp,probe_ssl,http],
			    [{active,once},{reuseaddr,true},
			     {verify, verify_none},
			     {keyfile, filename:join(Dir, "host.key")},
			     {certfile, filename:join(Dir, "host.cert")},
			     {upgrade_timeout, 5000},
			     {accept_timeout, 5000}],
			    ?MODULE, ServerOptions).

%%-----------------------------------------------------------------------------
%% @doc
%%  Starts and links a socket server on port Port with server options ServerOpts
%% that are sent to the server when a connection is established,
%% i.e init is called.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(Port::integer(),
		 ServerOptions::list({Option::atom(), Value::term()})) ->
			{ok, ChildPid::pid()} |
			{error, Reason::term()}.

start_link(Port, ServerOptions) ->
    ?debug("exo_http_server: start: port ~p, server options ~p",
	   [Port, ServerOptions]),
    Dir = code:priv_dir(exo),
    exo_socket_server:start_link(Port, [tcp,probe_ssl,http],
				 [{active,once},{reuseaddr,true},
				  {verify, verify_none},
				  {keyfile, filename:join(Dir, "host.key")},
				  {certfile, filename:join(Dir, "host.cert")}],
				 ?MODULE, ServerOptions).

%%-----------------------------------------------------------------------------
%% @doc
%%  Init function called when a connection is established.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec init(Socket::#exo_socket{},
	   ServerOptions::list({Option::atom(), Value::term()})) ->
		  {ok, State::#state{}}.

init(Socket, Options) ->
    {ok,{_IP,_Port}} = exo_socket:peername(Socket),
    ?debug("exo_http_server: connection from: ~p : ~p,\n options ~p",
	   [_IP, _Port, Options]),
    Access = proplists:get_value(access, Options, []),
    Module = proplists:get_value(request_handler, Options, undefined),
    {ok, #state{ access = Access, request_handler = Module}}.


%% To avoid a compiler warning. Should we actually support something here?
control(_Socket, _Request, _From, State) ->
    {ignore, State}.

%%-----------------------------------------------------------------------------
%% @doc
%%  Data function called when data is received.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec data(Socket::#exo_socket{},
	   Data::term(),
	   State::#state{}) ->
		  {ok, NewState::#state{}} |
		  {stop, {error, Reason::term()}, NewState::#state{}}.

data(Socket, Data, State) ->
    ?debug("exo_http_server:~w: data = ~w\n", [self(),Data]),
    case Data of
	{http_request, Method, Uri, Version} ->
	    CUri = exo_http:convert_uri(Uri),
	    Req  = #http_request { method=Method,uri=CUri,version=Version},
	    case exo_http:recv_headers(Socket, Req) of
		{ok, Req1} ->
		    handle_request(Socket, Req1, State);
		Error ->
		    {stop, Error, State}
	    end;
	{http_error, ?CRNL} ->
	    {ok, State};
	{http_error, ?NL} ->
	    {ok, State};
	_ when is_list(Data); is_binary(Data) ->
	    ?debug("exo_http_server: request data: ~p\n", [Data]),
	    {stop, {error,sync_error}, State};
	Error ->
	    {stop, Error, State}
    end.

%%-----------------------------------------------------------------------------
%% @doc
%%  Close function called when a connection is closed.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec close(Socket::#exo_socket{},
	    State::#state{}) ->
		   {ok, NewState::#state{}}.

close(_Socket, State) ->
    ?debug("exo_http_server: close\n", []),
    {ok,State}.

%%-----------------------------------------------------------------------------
%% @doc
%%  Error function called when an error is detected.
%%  Stops the server.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec error(Socket::#exo_socket{},
	    Error::term(),
	    State::#state{}) ->
		   {stop, {error, Reason::term()}, NewState::#state{}}.

error(_Socket,Error,State) ->
    ?debug("exo_http_serber: error = ~p\n", [Error]),
    {stop, Error, State}.


handle_request(Socket, R, State) ->
    ?debug("exo_http_server: request = ~s\n",
	 [[exo_http:format_request(R),?CRNL,
	   exo_http:format_hdr(R#http_request.headers),
	   ?CRNL]]),
    case exo_http:recv_body(Socket, R) of
	{ok, Body} ->
	    handle_body(Socket, R, Body, State);
	{error, closed} ->
	    {stop, normal,State};
	Error ->
	    {stop, Error, State}
    end.

handle_body(Socket, Request, Body,
	    State=#state {request_handler = RH}) when is_tuple(RH) ->
    {M, F, As} = request_handler(RH, Socket, Request, Body),
    ?debug("exo_http_server: calling ~p with -BODY:\n~s\n-END-BODY\n",
	   [RH, Body]),
    case apply(M, F, As) of
	ok -> {ok, State};
	stop -> {stop, normal, State};
	{error, Error} ->  {stop, Error, State}
    end;
handle_body(Socket, Request, Body, State) ->
    Url = Request#http_request.uri,
    ?debug("exo_http_server: -BODY:\n~s\n-END-BODY\n", [Body]),
    if Request#http_request.method == 'GET',
       Url#url.path == "/quit" ->
	    response(Socket, "close", 200, "OK", "QUIT"),
	    exo_socket:shutdown(Socket, write),
	    {stop, normal, State};
       Url#url.path == "/test" ->
	    response(Socket, undefined, 200, "OK", "OK"),
	    {ok, State};
       true ->
	    response(Socket, undefined, 404, "Not Found",
		     "Object not found"),
	    {ok, State}
    end.

%% @private
request_handler({Module, Function}, Socket, Request, Body) ->
    {Module, Function, [Socket, Request, Body]};
request_handler({Module, Function, XArgs}, Socket, Request, Body) ->
    {Module, Function, [Socket, Request, Body | XArgs]}.


%%-----------------------------------------------------------------------------
%% @doc
%%  Support function for sending a response.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec response(Socket::#exo_socket{},
	      Connection::string() | undefined,
	      Status::integer(),
	      Phrase::string(),
	      Status::string()) ->
				ok |
				{error, Reason::term()}.

response(S, Connection, Status, Phrase, String) ->
    response(S, Connection, Status, Phrase, String, []).

response(S, Connection, Status, Phrase, Body, Opts) ->
    ContentType = opt(content_type, Opts, "text/plain"),
    H = #http_shdr { connection = Connection,
		     content_length = content_length(Body),
		     content_type = ContentType },
    R = #http_response { version = {1, 1},
			 status = Status,
			 phrase = Phrase,
			 headers = H },
    Response = [exo_http:format_response(R),
		?CRNL,
		exo_http:format_hdr(H),
		?CRNL,
		Body],
    ?debug("exo_http_server: response:\n~s\n", [iolist_to_binary(Response)]),
    exo_socket:send(S, Response).

content_length(B) when is_binary(B) ->
    byte_size(B);
content_length(L) when is_list(L) ->
    iolist_size(L).


opt(K, L, Def) ->
    case lists:keyfind(K, 1, L) of
	{_, V} -> V;
	false  -> Def
    end.


%% @private
test() ->
    Dir = code:priv_dir(exo),
    exo_socket_server:start(9000, [tcp,probe_ssl,http],
			    [{active,once},{reuseaddr,true},
			     {verify, verify_none},
			     {keyfile, filename:join(Dir, "host.key")},
			     {certfile, filename:join(Dir, "host.cert")}],
			    ?MODULE, []).
