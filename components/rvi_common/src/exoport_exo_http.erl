%%
%% Copyright (C) 2014, Feuerlabs
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%
-module(exoport_exo_http).
-export([instance/3,
	 handle_body/4]).

-include_lib("exo/include/exo_http.hrl").
-include_lib("lager/include/log.hrl").


instance(SupMod, AppMod, Opts) ->
    ?debug("exoport_exo_http:instance(): SupModule(~p), Opts(~p)",
	   [ SupMod, Opts]),
    Port = opt(port, Opts, 8800),

    Child = {exo_http, {exo_http_server, start_link,
			[Port, [{request_handler,
				 {?MODULE, handle_body, [AppMod]}}]]},
	     permanent, 5000, worker, [exo_http_server]},
    case supervisor:start_child(SupMod, Child) of
	{ok, _} ->  ok;
	{ok, _, _} -> ok;

	Err -> Err
    end.


handle_body(Socket, Request, Body, AppMod) when Request#http_request.method == 'POST' ->
    ensure_ready(fun handle_post/4, Socket, Request, Body, AppMod);

handle_body(Socket, _Request, _Body, _AppMod) ->
    exo_http_server:response(Socket, undefined, 404, "Not Found",
			     "Object not found. Try using POST method.").

handle_post(Socket, Request, Body, AppMod) ->
    case Request#http_request.headers of
	#http_chdr{content_type = "application/json" ++ _ = T}
	  when T=="application/json";
	       T=="application/json-rpc";
	       T=="application/jsonrequest" ->
	    handle_post_json(Socket, Request, Body, AppMod);
	#http_chdr{content_type = "multipart/" ++ _} ->
	    handle_post_multipart(Socket, Request, Body, AppMod)
    end.

handle_post_json(Socket, _Request, Body, AppMod) ->
    try decode_json(Body) of
	{call, Id, Method, Args} ->
	    try handle_rpc(AppMod, Method, Args) of
		{ok, Reply} ->
		    success_response(Socket, Id, Reply);
		ok ->
		    ok;
		{error, Error} ->
		    error_response(Socket, Id, Error)
	    catch
		error:Reason ->
		    ?debug("~p:handle_rpc(~p, ~p, ~p) ERROR: ~p~n~p",
			   [?MODULE, AppMod, Method, Args, Reason,
			    erlang:get_stacktrace()]),
		    error_response(Socket, Id, internal_error)
	    end;
	{notification, Method, Args} ->
	    handle_notification(AppMod, Method, Args),
	    exo_http_server:response(Socket, undefined, 200, "OK", "");
	{error, _} ->
	    error_response(Socket, parse_error)
    catch
	error:_ ->
	    exo_http_server:response(Socket, undefined, 501,
				     "Internal Error",
				     "Internal Error")
    end.

handle_post_multipart(Socket, Request, Body, _AppMod) ->
    case (Request#http_request.headers)#http_chdr.content_type of
	"multipart/" ++ _ = Type ->
	    {match, [B]} =
		re:run(Type, "boundary\\s*=\\s*(.+)$", [{capture,[1],binary}]),
	    BoundaryRe = <<"--",B/binary,"\\s*\\r\\n|",
			   "\\r\\n--",B/binary,"\\s*\\r\\n|",
			   "\\r\\n--",B/binary,"--\\s*\\r\\n">>,
	    Parts = [decode_part(P)
		     || P <- re:split(Body, BoundaryRe, [{return,binary}]),
			  P =/= <<>>],
	    io:fwrite("Parts = ~p~n", [Parts]),
	    error_response(Socket, internal_error)
    end.

decode_part(P) ->
    io:fwrite("decode_part()~n"
	      "---------------------------~n"
	      "~s~n"
	      "---------------------------~n", [P]),
    decode_part(P, #http_chdr{}).

decode_part(P, Acc) ->
    case erlang:decode_packet(httph, P, []) of
	{ok, {http_header,_,K,_,V}, Rest} ->
	    decode_part(Rest, exo_http:set_chdr(K,V,Acc));
	{ok, http_eoh, Body} ->
	    {Acc, Body};
	{ok, {http_error, _}, _} ->
	    {Acc, P}
    end.

%% Validated RPC
handle_rpc(Mod, Method, Args) ->
    ?debug("exoport_exo_http_server:handle_rpc(): Mod:       ~p", [Mod]),
    ?debug("exoport_exo_http_server:handle_rpc(): Method:    ~p", [Method]),
    ?debug("exoport_exo_http_server:handle_rpc(): Args:      ~p", [Args]),

    try Mod:handle_rpc(Method, Args) of
	{ok, Result} ->
	    ?debug("exoport_exo_http_server:handle_rpc(ok): Result:   ~p", [Result]),
	    {ok, Result};

	{error, Reason} ->
	    {error, Reason};

	Wut ->
	    ?warning("exoport_exo_http_server:handle_rpc(ok): UNKNOWN:   ~p", [Wut]),
	    {error, Wut}

    catch
	error:Crash ->
            ?error("rpc_callback() CRASHED: Reason:   ~p", [Crash]),
            ?error("post_request() CRASHED: Stack:    ~p", [erlang:get_stacktrace()]),
	    {error, {internal_error, Crash}}
    end.

handle_notification(Mod, Method, Args) ->
    ?debug("exoport_exo_http_server:handle_notification(): Mod:       ~p", [Mod]),
    ?debug("exoport_exo_http_server:handle_notification(): Method:    ~p", [Method]),
    ?debug("exoport_exo_http_server:handle_notification(): Args:      ~p", [Args]),

    try Mod:handle_notification(Method, Args) of
	_ -> ok
    catch
	error:Crash ->
            ?error("rpc_callback() CRASHED: Reason:   ~p", [Crash]),
            ?error("post_request() CRASHED: Stack:    ~p", [erlang:get_stacktrace()]),
	    {error, {internal_error, Crash}}
    end.

success_response(Socket, Id, Reply) ->
    JSON = [{<<"jsonrpc">>, <<"2.0">>},
	    {<<"id">>, Id},
	    {<<"result">>, Reply}],
    exo_http_server:response(Socket, undefined, 200, "OK",
			     jsx:encode(JSON),
			     [{content_type, "application/json"}]).

error_response(Socket, Error) ->
    %% No Id available
    JSON = [{<<"jsonrpc">>, <<"2.0">>},
	    {<<"error">>, [{<<"code">>, json_error_code(Error)},
			   {<<"message">>, json_error_msg(Error)}]}],
    Body = jsx:encode(JSON),
    exo_http_server:response(Socket, undefined, 200, "OK", Body,
			     [{content_type, "application/json"}]).

error_response(Socket, Id, Error) ->
    JSON = [{<<"jsonrpc">>, <<"2.0">>},
	    {<<"id">>, Id},
	    {<<"error">>, [{<<"code">>, json_error_code(Error)},
			   {<<"message">>, json_error_msg(Error)}]}],
    Body = jsx:encode(JSON),
    exo_http_server:response(Socket, undefined, 200, "OK", Body,
			     [{content_type, "application/json"}]).

decode_json(Body) ->
    try jsx:decode(Body) of
	[T|_] = Elems when is_tuple(T) ->
	    case [opt(K,Elems,undefined) ||
		     K <- [<<"jsonrpc">>, <<"id">>,
			   <<"method">>, <<"params">>]] of
		[<<"2.0">>,undefined,Method,Params]
		  when Method =/= undefined,
		       Params =/= undefined ->
		    {notification, Method, Params};
		[<<"2.0">>,Id,Method,Params]
		  when Id=/=undefined,
		       Method=/=undefined,
		       Params =/= undefined ->
		    {call, Id, Method, Params};
		_ ->
		    {error, invalid}
	    end
    catch
	error:_ ->
	    {error, parse_error}
    end.

json_error_code(parse_error     )  -> -32700;
json_error_code(invalid_request )  -> -32600;
json_error_code(method_not_found)  -> -32601;
json_error_code(invalid_params  )  -> -32602;
json_error_code(internal_error  )  -> -32603;
json_error_code(_) -> -32603. % internal error


json_error_msg(-32700) -> <<"parse error">>;
json_error_msg(-32600) -> <<"invalid request">>;
json_error_msg(-32601) -> <<"method not found">>;
json_error_msg(-32602) -> <<"invalid params">>;
json_error_msg(-32603) -> <<"internal error">>;
json_error_msg(Code) when Code >= -32099, Code =< -32000 -> <<"server error">>;
json_error_msg(_) -> <<"json error">>.

opt(K, L, Def) ->
    case lists:keyfind(K, 1, L) of
	{_, V} -> V;
	false  -> Def
    end.


ensure_ready(F, Socket, Request, Body, AppMod) ->
    try rvi_server:ensure_ready(_Timeout = 10000),
	 F(Socket, Request, Body, AppMod)
    catch
	error:timeout ->
	    ?error("~p timeout waiting for rvi_core", [?MODULE]),
	    exo_http_server:response(Socket, undefined, 501,
				     "Internal Error",
				     "Internal Error")
    end.
