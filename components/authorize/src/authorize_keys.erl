-module(authorize_keys).
-behaviour(gen_server).

-export([get_key_pair/0,
	 get_key_pair_from_pem/2,
	 get_pub_key/1,
	 authorize_jwt/0,
	 provisioning_key/0,
	 signed_public_key/2,
	 save_keys/2,
	 save_cert/4]).
-export([get_certificates/0,
	 get_certificates/1]).
-export([validate_message/2,
	 validate_service_call/2]).
-export([filter_by_service/2,
	 find_cert_by_service/1]).
-export([public_key_to_json/1,
	 json_to_public_key/1]).

-export([self_signed_public_key/0]).  % just temporary
-export([pp_key/1,
	 abbrev_bin/1,
	 abbrev_payload/1,
	 abbrev_jwt/1]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include_lib("lager/include/log.hrl").
-include_lib("public_key/include/public_key.hrl").

-record(st, {provisioning_key,
	     cert_dir,
	     authorize_jwt}).

-record(cert, {id,
	       register = [],
	       invoke = [],
	       validity = [],
	       jwt,
	       cert}).

-record(key, {id,
	      key}).

-define(CERTS, authorize_certs).
-define(KEYS,  authorize_keys).

public_key_to_json(#'RSAPublicKey'{modulus = N, publicExponent = E}) ->
    [
     {<<"kty">>, <<"RSA">>},
     {<<"alg">>, <<"RS256">>},
     {<<"use">>, <<"sig">>},
     {<<"kid">>, <<"1">>},
     {<<"e">>, base64url:encode(binary:encode_unsigned(E))},
     {<<"n">>, base64url:encode(binary:encode_unsigned(N))}
    ].

self_signed_public_key() ->
    Key = filename:join([code:priv_dir(rvi), "keys",
			 "self_provisioning_key.pem"]),
    {Priv, _} = get_key_pair_from_pem(openssl, Key),
    MyPub = authorize_rpc:public_key(),
    signed_public_key(MyPub, Priv).

signed_public_key(MyPub, Priv) ->
    JSON = [
	    {<<"keys">>, [public_key_to_json(MyPub)]}
	   ],
    authorize_sig:encode_jwt(JSON, Priv).

json_to_public_key(JSON) ->
    try
	#'RSAPublicKey'{
	   modulus = get_unsigned(["n"], JSON),
	   publicExponent = get_unsigned(["e"], JSON)}
    catch
	throw:undefined ->
	    undefined
    end.

get_unsigned(Path, JSON) ->
    case rvi_common:get_json_element(Path, JSON) of
	{ok, Value} ->
	    binary:decode_unsigned(base64url:decode(Value));
	{error, _} ->
	    throw(undefined)
    end.

get_key_pair() ->
    case get_env(key_pair) of
	undefined ->
	    {undefined, undefined};
	{openssl_pem, Pem} ->
	    get_key_pair_from_pem(openssl, Pem)
    end.

authorize_jwt() ->
    gen_server:call(?MODULE, authorize_jwt).

validate_message(JWT, Conn) ->
    gen_server:call(?MODULE, {validate_message, JWT, Conn}).

validate_service_call(Service, Conn) ->
    gen_server:call(?MODULE, {validate_service_call, Service, Conn}).

get_certificates() ->
    get_certificates(local).

get_certificates(Conn) ->
    gen_server:call(?MODULE, {get_certificates, Conn}).

filter_by_service(Services, Conn) ->
    gen_server:call(?MODULE, {filter_by_service, Services, Conn}).

find_cert_by_service(Service) ->
    gen_server:call(?MODULE, {find_cert_by_service, Service}).

provisioning_key() ->
    gen_server:call(?MODULE, provisioning_key).

save_keys(Keys, Conn) ->
    gen_server:call(?MODULE, {save_keys, Keys, Conn}).

save_cert(Cert, JWT, Conn, LogId) ->
    gen_server:call(?MODULE, {save_cert, Cert, JWT, Conn, LogId}).

%% Gen_server functions

start_link() ->
    create_ets(),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
	{ok, Pid} = Ok ->
	    ets:give_away(?CERTS, Pid, undefined),
	    ets:give_away(?KEYS, Pid, undefined),
	    Ok;
	Other ->
	    Other
    end.

init([]) ->
    ProvisioningKey = get_pub_key(get_env(provisioning_key)),
    ?debug("ProvisioningKey = ~s~n", [pp_key(ProvisioningKey)]),
    CertDir = setup:verify_dir(get_env(cert_dir)),
    {ok, AuthJwt0} = file:read_file(get_env(authorize_jwt)),
    AuthJwt = strip_nl(AuthJwt0),
    ?debug("CertDir = ~p~n", [CertDir]),
    Certs = scan_certs(CertDir, ProvisioningKey),
    ?debug("scan_certs found ~p certificates~n", [length(Certs)]),
    [ets:insert(?CERTS, {{local, C#cert.id}, C}) || C <- Certs],
    {ok, #st{provisioning_key = ProvisioningKey,
	     cert_dir = CertDir,
	     authorize_jwt = AuthJwt}}.

handle_call(Req, From, S) ->
    try handle_call_(Req, From, S)
    catch
	error:Err ->
	    ?warning("ERROR - authorize_keys:handle_call(~p): ~p~n~p~n",
		     [Req, Err, erlang:get_stacktrace()]),
	    {reply, error, S}
    end.

handle_call_(authorize_jwt, _, S) ->
    {reply, S#st.authorize_jwt, S};
handle_call_(provisioning_key, _, S) ->
    {reply, S#st.provisioning_key, S};
handle_call_({get_certificates, Conn}, _, S) ->
    Certs = certs_by_conn(Conn),
    {reply, Certs, S};
handle_call_({save_keys, Keys, Conn}, _, S) ->
    ?debug("save_keys: Keys=~p, Conn=~p~n", [abbrev_k(Keys), Conn]),
    save_keys_(Keys, Conn),
    {reply, ok, S};
handle_call_({validate_message, JWT, Conn}, _, S) ->
    {reply, validate_message_(JWT, Conn), S};
handle_call_({validate_service_call, Svc, Conn}, _, S) ->
    {reply, validate_service_call_(Svc, Conn), S};
handle_call_({save_cert, Cert, JWT, {IP, Port} = Conn, LogId}, _, S) ->
    case process_cert_struct(Cert, JWT) of
	invalid ->
	    log(LogId, "cert INVALID Conn=~s:~w", [IP, Port]),
	    {reply, {error, invalid}, S};
	#cert{} = C ->
	    ets:insert(?CERTS, {{Conn, C#cert.id}, C}),
	    log(LogId, "cert stored ~s Conn=~s:~w", [abbrev_bin(C#cert.id), IP, Port]),
	    {reply, ok, S}
    end;
handle_call_({filter_by_service, Services, Conn} =R, _From, State) ->
    ?debug("authorize_keys:handle_call(~p,...)~n", [R]),
    Filtered = filter_by_service_(Services, Conn),
    ?debug("Filtered = ~p~n", [Filtered]),
    {reply, Filtered, State};
handle_call_({find_cert_by_service, Service} = R, _From, State) ->
    ?debug("authorize_keys:handle_call(~p,...)~n", [R]),
    Res = find_cert_by_service_(Service),
    ?debug("Res = ~p~n", [case Res of {ok,{A,B}} -> {ok,{A,abbrev_bin(B)}}; _ -> Res end]),
    {reply, Res, State};
handle_call_(_, _, S) ->
    {reply, error, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% Local functions

certs_by_conn(Conn) ->
    ?debug("certs_by_conn(~p)~n", [Conn]),
    UTC = rvi_common:utc_timestamp(),
    Certs = ets:select(?CERTS, [{ {{Conn,'_'}, #cert{jwt = '$1',
						     validity = '$2',
						     _='_'}},
				  [], [{{'$1', '$2'}}] }]),
    ?debug("rough selection: ~p~n", [[{abbrev_bin(C),I} || {C,I} <- Certs]]),
    [C || {C,V} <- Certs, check_validity(V, UTC)].

cert_recs_by_conn(Conn) ->
    ?debug("cert_recs_by_conn(~p)~n", [Conn]),
    UTC = rvi_common:utc_timestamp(),
    Certs = ets:select(?CERTS, [{ {{Conn,'_'}, '$1'},
				  [], ['$1'] }]),
    ?debug("rough selection: ~p~n", [[abbrev_bin(C#cert.id) || C <- Certs]]),
    [C || C <- Certs, check_validity(C#cert.validity, UTC)].

filter_by_service_(Services, Conn) ->
    ?debug("Filter: certs = ~p", [ets:tab2list(?CERTS)]),
    Invoke = ets:select(?CERTS, [{ {{Conn,'_'}, #cert{invoke = '$1',
						     _ = '_'}},
				  [], ['$1'] }]),
    ?debug("Services by conn (~p) -> ~p~n", [Conn, Invoke]),
    filter_svcs_(Services, Invoke).

filter_svcs_([S|Svcs], Invoke) ->
    case lists:any(fun(Ds) ->
			   lists:any(
			     fun(D) ->
				     match_svc(D, S)
			     end, Ds)
		   end, Invoke) of
	true ->
	    [S|filter_svcs_(Svcs, Invoke)];
	false ->
	    filter_svcs_(Svcs, Invoke)
    end;
filter_svcs_([], _) ->
    [].

find_cert_by_service_(Service) ->
    SvcParts = split_path(strip_prot(Service)),
    LocalCerts = ets:select(?CERTS, [{ {{local,'_'}, '$1'}, [], ['$1'] }]),
    ?debug("find_cert_by_service(~p~nLocalCerts = ~p~n",
	   [Service, [{Id,Reg,Inv} || #cert{id = Id,
					    invoke = Inv,
					    register = Reg} <- LocalCerts]]),
    case lists:foldl(
	   fun(#cert{register = Register} = C, {Max, _} = Acc) ->
		   case match_length(Register, SvcParts) of
		       L when L > Max ->
			   {L, C};
		       _ ->
			   Acc
		   end
	   end, {0, none}, LocalCerts) of
	{0, none} ->
	    {error, not_found};
	{_, #cert{id = Id, jwt = JWT}} ->
	    {ok, {Id, JWT}}
    end.

match_length(Invoke, Svc) ->
    R = lists:foldl(fun(D, Max) ->
			    DParts = split_path(strip_prot(D)),
			    erlang:max(match_length_(DParts, Svc), Max)
		    end, 0, Invoke),
    ?debug("match_length(~p,~p) -> ~p~n", [Invoke, Svc, R]),
    R.

match_length_(D, Svc) ->
    match_length_(D, Svc, 0).

match_length_([H|T], [H|T1], L) ->
    match_length_(T, T1, L+1);
match_length_(["+"|T], [_|T1], L) ->
    match_length_(T, T1, L+1);
match_length_([H|_], [H1|_], _) when H =/= H1 ->
    0;
match_length_([_|_], [], _) ->
    0;
match_length_([], _, L) ->
    L.

match_svc(D, S) ->
    A = split_path(strip_prot(D)),
    B = split_path(strip_prot(S)),
    ?debug("match_svc_(~p, ~p)~n", [A, B]),
    match_svc_(A, B).

strip_prot(P) ->
    case re:split(P, ":", [{return,list}]) of
	[_] -> P;
	[_,Rest] -> Rest
    end.

split_path(P) ->
    re:split(P, "/", [{return, list}]).

match_svc_([H|T], [H|T1]) ->
    match_svc_(T, T1);
match_svc_(["+"|T], [_|T1]) ->
    match_svc_(T, T1);
match_svc_([], _) ->
    true;
match_svc_(_, _) ->
    false.

get_env(K) ->
    Res = case application:get_env(rvi_core, K) of
	      {ok, V} -> V;
	      _       -> undefined
	  end,
    lager:debug("get_env(~p) -> ~p", [K, Res]),
    Res.

get_key_pair_from_pem(openssl, Pem) ->
    case file:read_file(Pem) of
	{ok, Bin} ->
	    case public_key:pem_decode(Bin) of
		[Entry] ->
		    case public_key:pem_entry_decode(Entry) of
			#'RSAPrivateKey'{modulus = Mod,
					 publicExponent = PE} = Priv ->
			    Pub = #'RSAPublicKey'{modulus = Mod,
						  publicExponent = PE},
			    {Priv, Pub};
			_ ->
			    ?debug("Unknown PEM entry (~p)~n", [Pem]),
			    {undefined, undefined}
		    end;
		_ ->
		    ?debug("Unsupported PEM file (~p)~n", [Pem]),
		    {undefined, undefined}
	    end;
	Error ->
	    ?debug("Cannot read PEM file (~p): ~p~n", [Pem, Error]),
	    {undefined, undefined}
    end.

get_pub_key({openssl_pem, File}) ->
    get_openssl_pub_key(File);
get_pub_key(File) ->
    get_openssl_pub_key(File).

get_openssl_pub_key(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    Entries = public_key:pem_decode(Bin),
	    case public_key:pem_entry_decode(hd(Entries)) of
		#'RSAPublicKey'{} = Pub ->
		    Pub;
		#'RSAPrivateKey'{modulus = N,
				 publicExponent = E}->
		    #'RSAPublicKey'{modulus = N,
				    publicExponent = E}
	    end;
	Error ->
	    ?warning("Cannot read pub key ~p (~p)~n", [File, Error]),
	    undefined
    end.

create_ets() ->
    create_ets(?CERTS, #cert.id),
    create_ets(?KEYS, #key.id).

create_ets(Tab, KeyPos) ->
    case ets:info(Tab, name) of
	undefined ->
	    ets:new(Tab, [ordered_set, public, named_table,
			  {keypos, KeyPos},
			  {heir, self(), undefined}]);
	_ ->
	    true
    end.

scan_certs(Dir, Key) ->
    UTC = rvi_common:utc_timestamp(),
    case file:list_dir(Dir) of
	{ok, Fs} ->
	    lists:foldl(
	      fun(F, Acc) ->
		      process_cert(filename:join(Dir, F), Key, UTC, Acc)
	      end, [], Fs);
	Error ->
	    ?warning("Cannot read certs (~p): ~p~n", [Dir, Error]),
	    ok
    end.

process_cert(F, Key, UTC, Acc) ->
    case file:read_file(F) of
	{ok, Bin} ->
	    try authorize_sig:decode_jwt(strip_nl(Bin), Key) of
		{_, Cert} ->
		    ?info("Unpacked Cert ~p:~n~p~n", [F, abbrev_payload(Cert)]),
		    case process_cert_struct(Cert, Bin, UTC) of
			invalid ->
			    Acc;
			#cert{} = C ->
			    [C|Acc]
		    end;
		invalid ->
		    ?warning("Invalid cert: ~p~n", [F]),
		    Acc
	    catch
		error:Exception ->
		    ?warning("Cert validation failure (~p): ~p~n",
			     [F, Exception]),
		    Acc
	    end;
	Error ->
	    ?warning("Cannot read cert ~p: ~p~n", [F, Error]),
	    Acc
    end.

strip_nl(Bin) ->
    case re:split(Bin,"\\s+$",[{return,binary},trim]) of
	[Trimmed] -> Trimmed;
	_ -> Bin
    end.

process_cert_struct(Cert, Bin) ->
    process_cert_struct(Cert, Bin, rvi_common:utc_timestamp()).

process_cert_struct(Cert, Bin, UTC) ->
    try process_cert_struct_(Cert, Bin, UTC)
    catch
	error:Err ->
	    ?warning("Failure processing Cert ~p~n~p",
		     [Cert, {Err, erlang:get_stacktrace()}]),
	    invalid
    end.

process_cert_struct_(Cert, Bin, UTC) ->
    ID = cert_id(Cert),
    {ok, Register} = rvi_common:get_json_element(
		      [{'OR', ["sources", "register"]}], Cert),
    {ok, Invoke} = rvi_common:get_json_element(
		    [{'OR', ["destinations", "invoke"]}], Cert),
    {ok, Start} = rvi_common:get_json_element(
		    ["validity", "start"], Cert),
    {ok, Stop}  = rvi_common:get_json_element(
		    ["validity", "stop"], Cert),
    ?debug("Start = ~p; Stop = ~p~n", [Start, Stop]),
    Validity = {Start, Stop},
    case check_validity(Start, Stop, UTC) of
	true ->
	    #cert{id = ID,
		  register = Register,
		  invoke = Invoke,
		  validity = Validity,
		  jwt = Bin,
		  cert = Cert};
	false ->
	    %% Cert outdated
	    ?warning("Outdated cert: Validity = ~p; UTC = ~p~n",
		     [Validity, UTC]),
	    invalid
    end.

cert_id(Cert) ->
    case rvi_common:get_json_element(["id"], Cert) of
	{ok, Id} ->
	    Id;
	{error, undefined} ->
	    ?warning("Cert has no ID: ~p~n", [Cert]),
	    erlang:now()
    end.

check_validity({Start, Stop}, UTC) ->
    check_validity(Start, Stop, UTC).

check_validity(Start, Stop, UTC) ->
    (UTC > Start) andalso (UTC < Stop).

save_keys_(Keys, Conn) ->
    lists:foreach(
      fun(K) ->
	      save_key(K, Conn)
      end, Keys).

save_key(K, Conn) ->
    case json_to_public_key(K) of
	undefined ->
	    ?warning("Unknown key type: ~p~n", [K]),
	    skip;
	#'RSAPublicKey'{} = PubKey ->
	    KeyID =
	    case rvi_common:get_json_element(["kid"], K) of
		{ok, ID} -> {Conn, ID};
		_        -> {Conn, make_ref()}
	    end,
	    ?debug("Saving key ~p, PubKey = ~p~n", [KeyID, pp_key(PubKey)]),
	    ets:insert(?KEYS, #key{id = KeyID, key = PubKey})
    end.

keys_by_conn(Conn) ->
    ?debug("keys_by_conn(~p); all keys: ~p",
	   [Conn, ets:select(?KEYS, [{ #key{id = '$1', _='_'},
				       [], ['$1'] }])]),
    ets:select(?KEYS, [{ #key{id = {Conn,'$1'},
			      key = '$2', _='_'}, [], [{{'$1', '$2'}}] }]).

validate_message_(JWT, Conn) ->
    ?debug("validate_message_(~p, ~p) -> ~p~n", [JWT, Conn, keys_by_conn(Conn)]),
    [_|_] = Keys = keys_by_conn(Conn),
    validate_message_1(Keys, JWT).

validate_message_1([{_,K}|T], JWT) ->
    case authorize_sig:decode_jwt(JWT, K) of
	invalid ->
	    validate_message_1(T, JWT);
	{_, Msg} ->
	    Msg
    end;
validate_message_1([], _) ->
    error(invalid).

validate_service_call_(Svc, Conn) ->
    case lists:filter(fun(C) -> can_invoke(Svc, C) end, cert_recs_by_conn(Conn)) of
	[] ->
	    invalid;
	[#cert{id = ID}|_] ->
	    {ok, ID}
    end.

can_invoke(Svc, #cert{invoke = In}) ->
    lists:any(fun(I) -> match_svc(I, Svc) end, In).

pp_key(#'RSAPrivateKey'{modulus = Mod, publicExponent = Pub}) ->
    P = integer_to_binary(Pub),
    M = integer_to_binary(Mod),
    <<"#{'RSAPrivateKey'{modulus = ", (abbrev_bin(M))/binary,
      ", publicExponent = ", P/binary, ", _ = ...}">>;
pp_key(#'RSAPublicKey'{modulus = Mod, publicExponent = Pub}) ->
    P = integer_to_binary(Pub),
    M = integer_to_binary(Mod),
    <<"#{'RSAPublicKey'{modulus = ", (abbrev_bin(M))/binary,
      ", publicExponent = ", P/binary, ", _ = ...}">>.

abbrev_bin(B) ->
    abbrev_bin(B, 20, 6).

abbrev_bin(B, Len, Part) ->
    try case byte_size(B) of
	    Sz when Sz > Len ->
		Part1 = erlang:binary_part(B, {0,Part}),
		Part2 = erlang:binary_part(B, {Sz,-Part}),
		<<Part1/binary, "...", Part2/binary>>;
	    _ ->
		B
	end
    catch error:_ -> B end.

abbrev_payload(Payload) ->
    try lists:map(fun abbrev_pl/1, Payload)
    catch error:_ -> Payload end.

abbrev_jwt({Hdr, Body} = X) ->
    try {Hdr, abbrev_payload(Body)}
    catch error:_ -> X end.

abbrev_pl({<<"keys">> = K, Ks}) ->
    {K, [abbrev_k(Ky) || Ky <- Ks]};
abbrev_pl({<<"certs">> = C, Cs}) ->
    {C, [abbrev_bin(Cert) || Cert <- Cs]};
abbrev_pl({<<"certificate">> = K, C}) ->
    {K, abbrev_bin(C)};
abbrev_pl({<<"sign">> = K, S}) when is_binary(S) ->
    {K, abbrev_bin(S)};
abbrev_pl(B) when is_binary(B) ->
    abbrev_bin(B, 40, 10);
abbrev_pl(L) when is_list(L) ->
    abbrev_payload(L);
abbrev_pl(X) ->
    X.

abbrev_k(K) ->
    try lists:map(fun abbrev_elem/1, K)
    catch error:_ -> K end.

abbrev_elem({<<"n">>, Bin}) ->
    {<<"n">>, authorize_keys:abbrev_bin(Bin)};
abbrev_elem(X) ->
    X.

log([ID], Fmt, Args) ->
    rvi_log:log(ID, <<"authorize">>, rvi_log:format(Fmt, Args));
log(_, _, _) ->
    ok.
