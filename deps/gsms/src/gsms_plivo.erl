-module(gsms_plivo).
-behaviour(gsms_session).

-export([start_link/2,  % called from gsms_if_sup.erl
	 new/1]).

-export([mandatory_options/0,
	 init/1,
	 handle_send/3,
	 get_signal_strength/1
	]).
	 %% handle_call/3,
	 %% subscribe/2]).

-export([decode_body/2,
	 signature/3,
	 uuid/0,
	 http_date/0,
	 get_x_plivo_sig/1]).

%% HTTP callback
-export([handle_body/4]).

-export([trace/0]).
-export([test_new/0, test_new/2, simtest/1]).

-record(st, {account,
	     auth_id,
	     auth_token,
	     src,
	     recv_uri,
	     recv_port,
	     recv_pid,
	     send_uri}).

-record(server, {parent,
		 uri,
		 auth_token}).

-include_lib("exo/include/exo_http.hrl").
-include_lib("exo/src/exo_socket.hrl").
-include("gsms.hrl").
-include("log.hrl").
-define(mandatory, '$mandatory').

start_link(_Id, Opts) ->
    {ok, new(Opts)}.

new(Opts) ->
    gsms_session:new(?MODULE, Opts).

mandatory_options() ->
    [auth_id, auth_token, src_number, recv_port, recv_uri].

init(Opts) ->
    [Acct, ID, Token, Src, Port, URI, SendURI, Attrs] =
	[gsms_lib:get_opt(K, Opts)
	 || K <- [acct, auth_id, auth_token,
		  src_number, recv_port, recv_uri,
		  {send_uri, "http://api.plivo.com"},
		  {attributes, []}]],
    {ok, RPid} = spawn_http_listener(Port, URI, Token),
    {ok, Src, Attrs, #st{account = Acct,
			 auth_id = ID,
			 auth_token = Token,
			 src = no_plus(Src),
			 recv_uri = URI,
			 recv_port = Port,
			 recv_pid = RPid,
			 send_uri = SendURI}}.

handle_send(Opts, Body, St) ->
    try
	Dest = gsms_lib:get_opt({addr, ?mandatory}, Opts),
	Res = plivo_send_SMS(Dest, Body, St),
	?debug("send: Res = ~p~n", [Res]),
	{ok, Res, St}
    catch
	error:E ->
	    {error, E}
    end.

get_signal_strength(St) ->
    %% For now, simply return maximum strength (see gsms/src/README)
    {ok, 30, St}.

plivo_send_SMS(Dest, Msg, #st{auth_id = AuthID,
			      auth_token = AuthTok,
			      src = Src,
			      send_uri = SendURI,
			      recv_uri = RecvURI}) ->
    URI = lists:flatten([SendURI, "/v1/Account/", AuthID, "/Message/"]),
    JSON = {struct, [{"src", Src},
		     {"dst", no_plus(Dest)},
		     {"text", Msg},
		     {"url", RecvURI},
		     {"log", true}]},
    Req = binary_to_list(iolist_to_binary(exo_json:encode(JSON))),
    Hdrs = [{'Content-Type', "application/json"}
	    | exo_http:make_headers(AuthID, AuthTok)],
    send_result(exo_http:wpost(URI, {1,1}, Hdrs, Req, 1000)).

send_result({ok, #http_response{status = Status} = R, Body}) ->
    if Status >= 200, Status =< 299 ->
	    {ok, get_uuid(R, Body)};
       true ->
	    {error, {Status, R#http_response.phrase}}
    end;
send_result({error, _} = E) ->
    E.

spawn_http_listener(Port, URI, Token) ->
    Srv = #server{parent = self(),
		  uri = URI,
		  auth_token = Token},
    exo_http_server:start_link(Port, [{request_handler,
				       {?MODULE, handle_body, [Srv]}}]).

handle_body(Socket, Request, Body, #server{auth_token = Tok, uri = URI}) ->
    ?debug("handle_body(_, ~p, ~p, _)~n", [Request, Body]),
    try decode_body(Request, Body) of
	Result ->
	    case validate_request(URI, Request, Result, Tok) of
		true ->
		    ?debug("handle_body() -> ~p~n", [Result]),
		    case parse_result(Result) of
			ok ->
			    response(Socket, ok);
			error ->
			    response(Socket, error)
		    end;
		false ->
		    response(Socket, auth)
	    end
    catch
	_:_ ->
	    response(Socket, error)
    end.

validate_request(URI, Request, Result, Tok) ->
    Sig = get_x_plivo_sig(Request),
    check_signature(Request, URI, Result, Sig, Tok).

check_signature(#http_request{uri = #url{path = Path}},
		URI, Result, Sig, Tok) ->
    URL = uri_join(URI, Path),
    Sig == signature(URL, Result, Tok).

uri_join(URI, Path) ->
    strip_trailing_slash(URI) ++ strip_trailing_slash(Path).

no_plus([$+|Num]) -> Num;
no_plus(Num     ) -> Num.

add_plus([$+|_] = Num) -> Num;
add_plus(Num         ) -> "+" ++ Num.

signature(URL, Result, Tok) ->
    Str = lists:foldl(
	    fun({K, A}, S) when is_atom(A) ->
		    S ++ K ++ atom_to_list(A);
	       ({K, F}, S) when is_float(F) ->
		    S ++ K ++ io_lib_format:fwrite_g(F);
	       ({K, I}, S) when is_integer(I) ->
		    S ++ K ++ integer_to_list(I);
	       ({K, V}, S) ->
		    S ++ K ++ V
	    end, strip_trailing_slash(URL), lists:sort(params(Result))),
    base64:encode_to_string(crypto:hmac(sha, Tok, Str)).

http_date() ->
    httpd_util:rfc1123_date().

strip_trailing_slash(S) ->
    case lists:reverse(S) of
	"/" ++ Rest ->
	    lists:reverse(Rest);
	_ ->
	    S
    end.

params({struct, Params}) ->
    [params(P) || P <- Params];
params({K, {array, A}}) ->
    {K, [params(P) || P <- A]};
params(Params) ->
    Params.

decode_body(R, Body) ->
    case get_content_type(R) of
	"application/x-www-form-urlencoded" ->
	    decode_www_form_urlencoded(Body);
	"application/json" ->
	    decode_json(Body)
    end.

get_content_type(#http_request{headers = #http_chdr{content_type = T}}) -> T;
get_content_type(#http_response{headers = #http_shdr{content_type = T}}) -> T.

decode_www_form_urlencoded(Body) ->
    lists:map(
      fun(L) ->
	      [K,V] = re:split(L, "=", [{return,list}]),
	      {unescape(K), unescape(V)}
      end, re:split(Body, "&", [{return,list}])).

unescape([$%,A,B|T]) ->
    [list_to_integer([A,B], 16) | unescape(T)];
unescape([$+|T]) ->
    [$\s|unescape(T)];
unescape([H|T]) ->
    [H|unescape(T)];
unescape([]) ->
    [].

decode_json(Body) ->
    exo_json:decode_string(to_string(Body)).

to_string(B) when is_binary(B) ->
    binary_to_list(B);
to_string(S) when is_list(S) ->
    S.

uuid() ->
    %% For now, convert to list (TODO: shouldn't be necessary)
    binary_to_list(uuid_()).

uuid_() ->
    %% https://en.wikipedia.org/wiki/Universally_unique_identifier
    N = 4, M = 2, % version 4 - random bytes
    <<A:48, _:4, B:12, _:2, C:62>> = crypto:rand_bytes(16),
    UBin = <<A:48, N:4, B:12, M:2, C:62>>,
    <<A1:8/binary, B1:4/binary, C1:4/binary, D1:4/binary, E1:12/binary>> =
	<< <<(hex(X)):8>> || <<X:4>> <= UBin >>,
    <<A1:8/binary, "-",
      B1:4/binary, "-",
      C1:4/binary, "-",
      D1:4/binary, "-",
      E1:12/binary>>.

hex(X) when X >= 0, X =< 9 ->
    $0 + X;
hex(X) when X >= 10, X =< 15 ->
    $a + X - 10.

get_uuid(R, Body) ->
    case decode_body(R, Body) of
	{ok, Decoded} ->
	    case lists:keyfind("message_uuid", 1, params(Decoded)) of
		false ->
		    io:fwrite("Cannot find message_uuid~n", []),
		    uuid();
		{_, UUID} ->
		    UUID
	    end;
	_ ->
	    io:fwrite("Couldn't decode body~n", []),
	    uuid()
    end.

get_x_plivo_sig(#http_response{headers = H}) ->
    find_x_sig(other_hdrs(H));
get_x_plivo_sig(#http_request{headers = H}) ->
    find_x_sig(other_hdrs(H)).

other_hdrs(#http_chdr{other = Hdrs}) -> Hdrs;
other_hdrs(#http_shdr{other = Hdrs}) -> Hdrs.

find_x_sig(Hdrs) ->
    case lists:keyfind("X-Plivo-Signature", 1, Hdrs) of
	false    -> false;
	{_, Sig} -> Sig
    end.

response(Socket, Reply) ->
    {Code, Msg} = case Reply of
		      ok    -> {200, "OK"};
		      error -> {404, "Not found"};
		      auth  -> {401, "Authorization failed"}
		  end,
    exo_http_server:response(Socket, undefined, Code, Msg, "").

%% From https://www.plivo.com/docs/api/application/ :
%% ------------------------------------------------------------
%% The following parameters will be sent to the Message URL.
%%
%% Fromstring        The source number of the incoming message.
%%    This will be the number of the person sending a message to a Plivo number.
%% Tostring          The number to which the message was sent.
%%    This will the your Plivo number on which the message has been received.
%% Typestring        Type of the message. This will always be sms
%% Textstring        The content of the message.
%% MessageUUIDstring A unique ID for the message.
%%    Your message can be uniquely identified on Plivo by this ID.
%% ------------------------------------------------------------
parse_result(Result) ->
    case Result of
	{struct, Elems} ->
	    parse_result_(Elems);
	[{_,_}|_] = Elems ->
	    parse_result_(Elems);
	_ ->
	    error
    end.

parse_result_(Elems) ->
    case lists:keyfind("Status", 1, Elems) of
	{_, "delivered"} ->
	    {_, _UUID} = lists:keyfind("MessageUUID", 1, Elems),
	    %% gsms_router:notify(UUID, ok);
	    ok;
	{_, _} ->
	    ok;  % ignore for now
	false ->
	    case [From, To, _Type, Text, _UUID] = _Res =
		[proplists:get_value(K, Elems, "")
		 || K <- ["From", "To", "Type", "Text", "MessageUUID"]] of
		_ when From =/= undefined ->
		    ?debug("Res = ~p~n", [_Res]),
		    gsms_router:input_from(To, #gsms_deliver_pdu{
						  addr = addr(From),
						  ud = Text}),
		    ok;
		_ ->
		    ok  % ignore for now
	    end
    end.

addr(A) ->
    #gsms_addr{type = international,
	       addr = add_plus(A)}.

%%
test_new() ->
    test_new("111", 9111).

test_new(Src, Port) ->
    application:ensure_all_started(gsms),
    new([{acct, "Acct"},
	 {auth_id,"myacct"},{auth_token,"myauth"},
	 {src_number, Src},
	 {recv_port, Port},
	 {send_uri, "https://localhost:9100"},
	 {recv_uri,"https://localhost"}]).

simtest(1) ->
    application:ensure_all_started(gsms),
    R = new([{acct, acct1},
	     {auth_id, "acct1"},
	     {auth_token, "auth1"},
	     {src_number, "111"},
	     {recv_port, 9200},
	     {send_uri, "http://localhost:9100"},
	     {recv_uri, "http://localhost:9200"}]),
    trace(),
    R;
simtest(2) ->
    application:ensure_all_started(gsms),
    R = new([{acct, acct2},
	     {auth_id, "acct2"},
	     {auth_token, "auth2"},
	     {src_number, "222"},
	     {recv_port, 9300},
	     {send_uri, "http://localhost:9100"},
	     {recv_uri, "http://localhost:9300"}]),
    trace(),
    R.

trace() ->
    dbg:tracer(),
    dbg:tpl(?MODULE, x),
    dbg:tp(exo_http, x),
    dbg:p(all, [c]).
