%%
%% Copyright (C) 2014, Feuerlabs 
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%
-module(exoport_exo_http).
-export([instance/3,
	 handle_body/4,
%%	 json_rpc/1,
%%	 json_rpc/2,
	 data_to_json/3]).

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
	{ok, _} -> ok;
	{ok, _, _} -> ok;
	Err -> Err
    end.


handle_body(Socket, Request, Body, AppMod) when Request#http_request.method == 'POST' ->
    Url = Request#http_request.uri,
    ?debug("exoport_exo_http: handle_body(): Url:       ~p", [Url]),
    ?debug("exoport_exo_http: handle_body(): Socket:    ~p", [Socket]),
    ?debug("exoport_exo_http: handle_body(): AppMod:    ~p", [AppMod]),
    ?debug("exoport_exo_http: handle_body(): Request:   ~p", [Request]),
    ?debug("exoport_exo_http: handle_body(): Body:      ~p", [Body]),

    try decode_json(Body) of
	{call, Id, Method, Args} ->
	    case handle_rpc(AppMod, Method, Args) of
		{ok, Reply} ->
		    success_response(Socket, Id, Reply);
		ok ->
		    ok;
		{error, Error} ->
		    error_response(Socket, Id, Error)
	    end;
	{notification, _Method, _Args} ->
	    %% FIXME: Notification.
	    exo_http_server:response(Socket, undefined, 200, "OK", "");
	{error, _} ->
	    error_response(Socket, parse_error)
    catch
	error:_ ->
	    exo_http_server:response(Socket, undefined, 501,
				     "Internal Error",
				     "Internal Error")
    end;

handle_body(Socket, _Request, _Body, _AppMod) ->
    exo_http_server:response(Socket, undefined, 404, "Not Found",
			     "Object not found. Try using POST method.").


%% Validated RPC
handle_rpc(Mod, Method, Args) ->
    ?debug("exo_http_server:handle_rpc(): Mod:       ~p", [Mod]),
    ?debug("exo_http_server:handle_rpc(): Method:    ~p", [Method]),
    ?debug("exo_http_server:handle_rpc(): Args:      ~p", [Args]),

    try Mod:handle_rpc(Method, Args) of
	{ok, Result} ->
	    ?debug("exo_http_server:handle_rpc(ok): Result:   ~p", [Result]),
	    {ok, Result};
	
	{error, Reason} ->
	    {error, Reason}
    catch
	error:Crash ->
            ?error("rpc_callback() CRASHED: Reason:   ~p", [Crash]),
            ?error("post_request() CRASHED: Stack:    ~p", [erlang:get_stacktrace()]),
	    {error, {internal_error, Crash}}
    end.

success_response(Socket, Id, Reply) ->
    JSON = {struct, [{"jsonrpc", "2.0"},
		     {"id", Id},
		     {"result", {struct,  Reply}}]},
    exo_http_server:response(Socket, undefined, 200, "OK",
			     exo_json:encode(JSON),
			     [{content_type, "application/json"}]).

error_response(Socket, Error) ->
    %% No Id available
    JSON = {struct, [{"jsonrpc", "2.0"},
		     {"error", {struct,
				[{"code", json_error_code(Error)},
				 {"message", json_error_msg(Error)}]}}]},
    Body = list_to_binary(exo_json:encode(JSON)),
    exo_http_server:response(Socket, undefined, 200, "OK", Body,
			     [{content_type, "application/json"}]).

error_response(Socket, Id, Error) ->
    JSON = {struct, [{"jsonrpc", "2.0"},
		     {"id", Id},
		     {"error", {struct,
				[{"code", json_error_code(Error)},
				 {"message", json_error_msg(Error)}]}}]},
    Body = list_to_binary(exo_json:encode(JSON)),
    exo_http_server:response(Socket, undefined, 200, "OK", Body,
			     [{content_type, "application/json"}]).

decode_json(Body) ->
    try exo_json:decode_string(binary_to_list(Body)) of
	{ok, {struct,Elems}} ->
	    case [opt(K,Elems,undefined) || K <- ["jsonrpc","id",
						  "method", "params"]] of
		["2.0",undefined,Method,Params]
		  when Method =/= undefined,
		       Params =/= undefined ->
		    {notification, Method, Params};
		["2.0",Id,Method,Params]
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


json_error_msg(-32700) -> "parse error";
json_error_msg(-32600) -> "invalid request";
json_error_msg(-32601) -> "method not found";
json_error_msg(-32602) -> "invalid params";
json_error_msg(-32603) -> "internal error";
json_error_msg(Code) when Code >= -32099, Code =< -32000 -> "server error";
json_error_msg(_) -> "json error".

opt(K, L, Def) ->
    case lists:keyfind(K, 1, L) of
	{_, V} -> V;
	false  -> Def
    end.


%%
data_to_json(Elems, Env, Data) ->
    ?debug("data_to_json(~p, ~p, ~p)~n", [Elems, Env, Data]),
    case find_leaf(<<"rpc-status-string">>, Elems) of
        false ->
            yang_json:data_to_json(Elems, Env, Data);
        _Leaf ->
            case keyfind(<<"rpc-status-string">>, Data) of
                false ->
                    case keyfind(<<"rpc-status">>, Data) of
                        false ->
                            yang_json:data_to_json(Elems, Env, Data);
                        Status ->
                            case enum_descr(find_leaf(<<"rpc-status">>, Elems),
                                            to_binary(element(2, Status))) of
                                false ->
                                    yang_json:data_to_json(Elems, Env, Data);
                                Descr ->
                                    yang_json:data_to_json(
                                      Elems, Env,
                                      [{<<"rpc-status-string">>, Descr}|Data])
                            end
                    end;
                _ ->
                    yang_json:data_to_json(Elems, Env, Data)
            end
    end.

enum_descr(false, _) -> false;
enum_descr({leaf, _, _, I}, V) ->
    case lists:keyfind(type, 1, I) of
        {_, _, <<"enumeration">>, I1} ->
            enum_descr_(I1, V);
        _ ->
            false
    end.

%% Assume rpc-status can be either the numeric value or the description.
enum_descr_([{enum,_,V,I}|_], V) ->
    case lists:keyfind(description,1,I) of
        {_, _, Descr, _} -> Descr;
        false -> V
    end;
enum_descr_([{enum,_,D,I}|T], V) ->
    case lists:keyfind(value, 1, I) of
        {_, _, V, _} ->
            case lists:keyfind(description,1,I) of
                {_, _, Descr, _} -> Descr;
                false -> D
            end;
        _ ->
            enum_descr_(T, V)
    end;
enum_descr_([_|T], V) ->
    enum_descr_(T, V);
enum_descr_([], _) ->
    false.



find_leaf(K, [{leaf,_,K,_} = L|_]) -> L;
find_leaf(K, [_|T]) -> find_leaf(K, T);
find_leaf(_, []) -> false.

keyfind(A, [H|T]) when is_tuple(H) ->
    K = element(1, H),
    case comp(A,K) of
        true ->
            H;
        false ->
            keyfind(A, T)
    end;
keyfind(_, []) ->
    false.

comp(A, A) -> true;
comp(A, B) when is_binary(A), is_list(B) ->
    binary_to_list(A) == B;
comp(A, B) when is_binary(A), is_atom(B) ->
    A == atom_to_binary(B, latin1);
comp(_, _) ->
    false.

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L).
