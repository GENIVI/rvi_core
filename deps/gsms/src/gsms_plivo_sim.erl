-module(gsms_plivo_sim).
-behaviour(gen_server).

-export([start_link/1]).

-export([send_message/3]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).
-compile(export_all).

-include_lib("exo/include/exo_http.hrl").
-include("log.hrl").

-define(DEFAULT_PORT, 9100).

%% TODO: A bunch of duplication in the records. Should be cleaned up.
%% TODO: Should be enough with one HTTP server instance serving all accts.
-record(service, {acct,
		  type,
		  uri,
		  conn_opts = [],
		  numbers = [],
		  auth_token,
		  auth_string,
		  pid}).
-record(st, {services = [],
	     server,
	     opts,
	     notify = []}).

-record(server, {parent}).

-define(mandatory, '$mandatory').

test() ->
    application:ensure_all_started(gsms),
    start_link([{services, [{plivo, [{type, plivo_sim},
				     {port, 9100},
				     {uri, "http://localhost:9100"},
				     {account, "myacct"},
				     {auth, "myauth"}
				    ]}
			   ]}
	       ]).

simtest() ->
    application:ensure_all_started(gsms),
    start_link([{port, 9100},
		{services,
		 [{s1, [{type, plivo_sim},
			{numbers, ["111"]},
			{uri, "http://localhost:9200"},
			{account, "acct1"},
			{auth, "auth1"}]},
		  {s2, [{type, plivo_sim},
			{numbers, ["222"]},
			{uri, "http://localhost:9300"},
			{account, "acct2"},
			{auth, "auth2"}]}]}]).

start_link(Opts) ->
    case lists:keyfind(reg_name, 1, Opts) of
	false ->
	    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []);
	{_, Name} when is_tuple(Name) ->
	    gen_server:start_link(Name, ?MODULE, Opts, [])
    end.

send_message(Opts, Body) ->
    call(?MODULE, {send_message, Opts, Body}).

send_message(Server, Opts, Body) ->
    call(Server, {send_message, Opts, Body}).

init(Opts) ->
    {ok, Pid} = start_server(Opts),
    S0 = #st{server = Pid, opts = Opts},
    S = case lists:keyfind(services, 1, Opts) of
	    false -> S0#st{server = Pid};
	    {_, Svcs} ->
		lists:foldl(
		  fun({Svc, SvcOpts}, Sx) ->
			  {_, Sx1} = do_add_service(Svc, SvcOpts, Sx),
			  Sx1
		  end, S0, Svcs)
	end,
    {ok, S}.

handle_call({send_message, Opts, Body}, _From, S) ->
    %% We should really queue the message for delivery, then send a notification
    case message_params(Opts, Body, S) of
	{UUID, URI, Token, Params} ->
	    S1 = maybe_notify(Opts, UUID, S),
	    self() ! {message_sent, UUID, Params},
	    try Res = do_send_message(URI, UUID, Token, Params),
		 {reply, Res, S1}
	    catch
		error:Reason ->
		    {reply, {error, Reason}, S}
	    end;
	false ->
	    {reply, {error, not_found}, S}
    end;
handle_call({add_service, Svc, Opts}, _From, S) ->
    {Reply, S1} = do_add_service(Svc, Opts, S),
    {reply, Reply, S1};
handle_call({authorize,AuthStr}, _, #st{services = Svcs} = S) ->
    {reply, lists:keyfind(AuthStr, #service.auth_string, Svcs), S};
handle_call(_Req, _From, S) ->
    {reply, {error, badarg}, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({Evt, UUID, _Params} = Msg, #st{notify = Nfy} = S)
  when Evt == message_sent; Evt == message_delivered ->
    Found = [N || {E, ID, _} = N <- Nfy,
		  E == Evt andalso ID == UUID],
    [notify(Msg, Num, S) || {_, _, Num} <- Found],
    {noreply, S#st{notify = Nfy -- Found}};
handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

call(Server, Req) ->
    gen_server:call(Server, Req).

ask_authorize(Pid, AuthStr) ->
    call(Pid, {authorize, AuthStr}).

auth_service([#service{conn_opts = Opts}|T], Acct, Token) ->
    case lists:member({account,Acct}, Opts) of
	true ->
	    lists:member({auth, Token}, Opts);
	false ->
	    auth_service(T, Acct, Token)
    end;
auth_service([], _, _) ->
    false.

maybe_notify(Opts, UUID, #st{notify = Nfy} = S) ->
    case lists:keyfind(notify, 1, Opts) of
	false ->
	    S;
	{_, Number, Tags} ->
	    S#st{notify = [{Tag, UUID, Number} || Tag <- Tags] ++ Nfy}
    end.

notify({Event, UUID, Params0}, Number, S) ->
    case find_service(Number, S) of
	#service{uri = URI, auth_token = Token} ->
	    Params =
		[{"Status", status(Event)},
		 {"ParentMessageUUID", UUID},
		 {"PartInfo", "1 of 1"} | Params0],
	    do_send_message(URI, UUID, Token, Params);
	false ->
	    ignore
    end.

status(message_sent     ) -> "sent";
status(message_delivered) -> "delivered".


message_params(Opts, Body, #st{} = S) ->
    [From, To, UUID] = [gsms_lib:get_opt(K, Opts, Def)
			|| {K, Def} <- [{from, ?mandatory},
					{to, ?mandatory},
					{uuid, fun gsms_plivo:uuid/0}]],
    case find_service(To, S) of
	#service{uri = URI, auth_token = Token} ->
	    Params = [
		      {"To", no_plus(To)},
		      {"From", no_plus(From)},
		      {"TotalRate", 0.0},
		      {"Units", 1},
		      {"Text", Body},
		      {"TotalAmount", 0.0},
		      {"Type", "sms"},
		      {"MessageUUID", UUID}
		     ],
	    {UUID, URI, Token, Params};
	_ ->
	    false
    end.

find_service(Number, #st{services = Svcs}) ->
    case [Svc1 || #service{numbers = Ns} = Svc1 <- Svcs,
		  lists:member(Number, Ns)] of
	[#service{} = Svc|_] ->
	    Svc;
	_ ->
	    false
    end.

no_plus([$+|Num]) -> Num;
no_plus(Num     ) -> Num.

do_send_message(URI, UUID, Token, Params) ->
    Sig = gsms_plivo:signature(URI, Params, Token),
    Hs = headers(Sig),
    Result = exo_http:wpost(URI, Hs, Params),
    io:fwrite("wpost result = ~p~n", [Result]),
    self() ! {message_delivered, UUID, Params},
    {ok, UUID}.

headers(Sig) ->
    [{'Content-Type', "application/x-www-form-urlencoded"},
     {'Accept-Encoding', "gzip, deflate"},
     {"X-Plivo-Signature", Sig}].

do_add_service(_Svc, Opts, S) ->
    [Type, ConnOpts, Numbers, Acct, URI, Auth] =
	[gsms_lib:get_opt(K, Opts, Def)
	 || {K, Def} <- [{type, plivo_sim},
			 {connection, []},
			 {numbers, []},
			 {account, ?mandatory},
			 {uri, ?mandatory},
			 {auth, ?mandatory}]],
    AuthStr = exo_http:auth_basic_encode(Acct, Auth),
    SvcRec = #service{type = Type,
		      conn_opts = ConnOpts,
		      numbers = [no_plus(N) || N <- Numbers],
		      acct = Acct,
		      uri = URI,
		      auth_token = Auth,
		      auth_string = AuthStr},
    {ok, S#st{services = [SvcRec | S#st.services]}}.

start_server(Opts) ->
    Port = gsms_lib:get_opt(port, Opts, ?DEFAULT_PORT),
    Srv = #server{parent = self()},
    exo_http_server:start_link(Port, [{request_handler,
				       {?MODULE, handle_body, [Srv]}}]).

handle_body(Socket, Request, Body, #server{parent = P}) ->
    ?debug("Path = ~p~nBody = ~p~n",
	   [(Request#http_request.uri)#url.path, Body]),
    case check_auth(Request, P) of
	false ->
	    response(Socket, authentication_failed, "");
	#service{acct = Acct, auth_token = AuthTok, uri = URI} ->
	    handle_body_(Socket, Request, Body, Acct, AuthTok, URI, P)
    end.

handle_body_(Socket, Request, Body, Acct, AuthTok, URI, P) ->
    case valid_request(Request, Acct) of
	false ->
	    response(Socket, authentication_failed, "");
	"Message" ->
	    ?debug("handle_body(_, ~p, ~p, _)~n", [Request, Body]),
	    try exo_json:decode_string(binary_to_list(Body)) of
		{ok, {struct, Result}} ->
		    ?debug("Decoded = ~p~n", [Result]),
		    {_, Src} = lists:keyfind("src", 1, Result),
		    {_, Dest} = lists:keyfind("dst", 1, Result),
		    {_, Text} = lists:keyfind("text", 1, Result),
		    gsms_router:input_from(Src, Text),
		    UUID = gsms_plivo:uuid(),
		    API_id = gsms_plivo:uuid(),
		    Struct = {struct,
			      [{"api_id", API_id},
			       {"message","message(s) queued"},
			       {"message_uuid", UUID},
			       {"api_id", API_id}]},
		    JSON = to_json(Struct),
		    send_message(P, [{from, Src},
				     {to, Dest},
				     {uuid, UUID},
				     {notify, Src, [message_sent,
						    message_delivered]}],
				 Text),
		    response(Socket, ok, JSON,
			     response_headers(URI, Struct, AuthTok))
	    catch
		_:_ ->
		    response(Socket, server_error, "")
	    end
    end.

check_auth(Request, P) ->
    case get_basic_auth(Request) of
	false -> false;
	AuthStr ->
	    case ask_authorize(P, AuthStr) of
		false -> false;
		Auth  -> Auth
	    end
    end.

to_json(Struct) ->
    exo_json:encode(Struct).

valid_request(#http_request{uri = #url{path = Path}}, Acct) ->
    case filename:split(Path) of
	["/", "v1","Account",Acct,"Message"] ->
	    "Message";
	_Split ->
	    io:fwrite("unrecognized: ~p~n", [_Split]),
	    false
    end.

get_basic_auth(#http_request{headers = #http_chdr{
					  authorization = "Basic " ++ Auth}}) ->
    Auth;
get_basic_auth(_) ->
    false.


response(Socket, Res, Body) ->
    response(Socket, Res, Body, [{"Date", gsms_plivo:http_date()}]).

response(Socket, Res, Body, Hdrs) ->
    Opts = [{content_type, "application/json"} | Hdrs],
    {Code, _} = lists:keyfind(Res, 2, responses()),
    exo_http_server:response(Socket, undefined, Code,
			     atom_to_list(Res), Body, Opts).

response_headers(URI, Params, Token) ->
    [{"Date", gsms_plivo:http_date()},
     {"X-Plivo-Signature", gsms_plivo:signature(URI, Params, Token)}].


responses() ->
    [{200, ok},
     {201, resource_created},
     {202, resource_changed},
     {204, resource_deleted},
     {400, parameter_missing},  % ... or invalid
     {401, authentication_failed},
     {404, resource_not_found},
     {405, method_not_allowed},
     {500, server_error}].

args(send_message) ->
    [{"src"   , string, mandatory},
     {"dst"   , string, mandatory},
     {"text"  , string, mandatory},
     {"type"  , string, optional, "sms"},
     {"url"   , string, optional, ""},
     {"method", string, optional, "POST"},
     {"log"   , boolean, optional, true}].
