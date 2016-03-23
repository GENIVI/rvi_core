-module(authorize_keys).
-behaviour(gen_server).

-export([get_key_pair/0,
	 get_device_key/0,
	 get_key_pair_from_pem/2,
	 get_pub_key/1,
	 provisioning_key/0,
	 signed_public_key/2,
	 %% save_keys/2,
	 save_cred/5]).
-export([get_credentials/0,
	 get_credentials/1]).
-export([validate_message/2,
	 validate_service_call/2]).
-export([filter_by_service/2,
	 find_cred_by_service/1]).
-export([public_key_to_json/1,
	 json_to_public_key/1]).

-export([cache_authorizations/1,
	 remove_cached_authorizations/1,
	 remove_cached_authorizations_for_conn/1,
	 update_authorization_cache/2]).

-export([remove_connection/1]).

-export([self_signed_public_key/0]).  % just temporary
-export([strip_nl/1]).
-export([pp_key/1,
	 abbrev/1,
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
	     dev_cert,
	     cred_dir}).

-record(cred, {id,
	       right_to_receive = [],
	       right_to_invoke = [],
	       validity = [],
	       device_cert,
	       jwt,
	       cred}).

-define(CREDS, authorize_creds).
-define(CACHE, authorize_cache).

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

get_device_key() ->
    case get_env(device_key) of
	undefined ->
	    {undefined, undefined};
	[_|_] = File ->
	    get_key_pair_from_pem(openssl, File);
	{openssl_pem, Pem} ->
	    get_key_pair_from_pem(openssl, Pem)
    end.

validate_message(JWT, Conn) ->
    gen_server:call(?MODULE, {validate_message, JWT, Conn}).

validate_service_call(Service, Conn0) ->
    Conn = normalize_conn(Conn0),
    case ets:lookup(?CACHE, {Service, Conn}) of
	[{_, Res}] ->
	    ?debug("cached validation (~p): ~p", [{Service, Conn}, Res]),
	    Res;
	[] ->
	    ?debug("no cached validation (~p)", [{Service, Conn}]),
	    gen_server:call(?MODULE, {validate_service_call, Service, Conn})
    end.

get_credentials() ->
    get_credentials(local).

get_credentials(Conn) ->
    gen_server:call(?MODULE, {get_credentials, Conn}).

filter_by_service(Services, Conn) ->
    gen_server:call(?MODULE, {filter_by_service, Services, Conn}).

find_cred_by_service(Service) ->
    gen_server:call(?MODULE, {find_cred_by_service, Service}).

provisioning_key() ->
    gen_server:call(?MODULE, provisioning_key).

save_cred(Cred, JWT, Conn, PeerCert, LogId) ->
    gen_server:call(?MODULE, {save_cred, Cred, JWT, Conn, PeerCert, LogId}).

cache_authorizations(Svcs) ->
    gen_server:cast(?MODULE, {cache_authorizations, Svcs}).

remove_cached_authorizations(Svcs) ->
    gen_server:cast(?MODULE, {remove_cached_authorizations, Svcs}).

remove_cached_authorizations_for_conn(Conn) ->
    remove_cached_authorizations_for_conn_(normalize_conn(Conn)).

update_authorization_cache(Conn, CS) ->
    gen_server:cast(?MODULE, {update_authorization_cache, Conn, CS}).

remove_connection(Conn) ->
    gen_server:cast(?MODULE, {remove_connection, Conn}).

%% Gen_server functions

start_link() ->
    create_ets(),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
	{ok, Pid} = Ok ->
	    ets:give_away(?CREDS, Pid, undefined),
	    %% ets:give_away(?KEYS, Pid, undefined),
	    Ok;
	Other ->
	    Other
    end.

init([]) ->
    ProvisioningKey = get_pub_key_from_cert(get_env(root_cert)),
    ?debug("ProvisioningKey = ~s~n", [pp_key(ProvisioningKey)]),
    {ok, DevCertBin} = file:read_file(get_env(device_cert)),
    DevCert = strip_pem_begin_end(DevCertBin),
    ?debug("Own DevCert = ~w", [abbrev_bin(DevCert)]),
    CredDir = setup:verify_dir(get_env(cred_dir)),
    ?debug("CredDir = ~p~n", [CredDir]),
    Creds = scan_creds(CredDir, ProvisioningKey, DevCert),
    ?debug("scan_creds found ~p credentials~n", [length(Creds)]),
    [ets:insert(?CREDS, {{local, C#cred.id}, C}) || C <- Creds],
    ?debug("CREDS = ~p", [ets:tab2list(?CREDS)]),
    {ok, #st{provisioning_key = ProvisioningKey,
	     dev_cert = DevCert,
	     cred_dir = CredDir}}.

handle_call(Req, From, S) ->
    try handle_call_(Req, From, S)
    catch
	error:Err ->
	    ?warning("ERROR - authorize_keys:handle_call(~p): ~p~n~p~n",
		     [Req, Err, erlang:get_stacktrace()]),
	    {reply, error, S}
    end.

handle_call_(provisioning_key, _, S) ->
    {reply, S#st.provisioning_key, S};
handle_call_({get_credentials, Conn}, _, S) ->
    Creds = creds_by_conn(normalize_conn(Conn)),
    {reply, Creds, S};
handle_call_({validate_service_call, Svc, Conn}, _, S) ->
    {reply, validate_service_call_(Svc, Conn), S};
handle_call_({save_cred, Cred, JWT, {IP, Port} = Conn0, PeerCert, LogId}, _, S) ->
    Conn = normalize_conn(Conn0),
    ?debug("save_cred: ~p (Conn=~p, PeerCert=~p)", [Cred, Conn, abbrev(PeerCert)]),
    case process_cred_struct(Cred, JWT, PeerCert) of
	invalid ->
	    log(LogId, warning, "cred INVALID Conn=~s:~w", [IP, Port]),
	    {reply, {error, invalid}, S};
	#cred{} = C ->
	    ets:insert(?CREDS, {{Conn, C#cred.id}, C}),
	    log(LogId, result, "cred stored ~s Conn=~p", [abbrev_bin(C#cred.id), Conn]),
	    {reply, ok, S}
    end;
handle_call_({filter_by_service, Services, Conn0} =R, _From, State) ->
    ?debug("authorize_keys:handle_call(~p,...)~n", [R]),
    Conn = normalize_conn(Conn0),
    Filtered = filter_by_service_(Services, Conn),
    ?debug("Filtered = ~p~n", [Filtered]),
    {reply, Filtered, State};
handle_call_({find_cred_by_service, Service} = R, _From, State) ->
    ?debug("authorize_keys:handle_call(~p,...)~n", [R]),
    Res = find_cred_by_service_(Service),
    ?debug("Res = ~p~n", [case Res of {ok,{A,B}} -> {ok,{A,abbrev_bin(B)}}; _ -> Res end]),
    {reply, Res, State};
handle_call_(_, _, S) ->
    {reply, error, S}.

handle_cast({cache_authorizations, Svcs}, S) ->
    cache_authorizations_(Svcs),
    {noreply, S};
handle_cast({remove_cached_authorizations, Svcs}, S) ->
    remove_cached_authorizations_(Svcs),
    {noreply, S};
handle_cast({update_authorization_cache, Conn0, CS}, S) ->
    Conn = normalize_conn(Conn0),
    update_authorization_cache_(Conn, CS),
    {noreply, S};
handle_cast({remove_connection, Conn0}, S) ->
    Conn = normalize_conn(Conn0),
    ets:select_delete(?CACHE, [{ {{'_', Conn}, '_'}, [], [true] }]),
    ets:select_delete(?CREDS, [{ {{Conn, '_'}, '_'}, [], [true] }]),
    {noreply, S};
handle_cast(_, S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% Local functions

creds_by_conn(Conn) ->
    ?debug("creds_by_conn(~p)~n", [Conn]),
    UTC = rvi_common:utc_timestamp(),
    ?debug("all creds = ~p", [abbrev(ets:tab2list(?CREDS))]),
    Creds = ets:select(?CREDS, [{ {{Conn,'_'}, #cred{jwt = '$1',
						     validity = '$2',
						     _='_'}},
				  [], [{{'$1', '$2'}}] }]),
    ?debug("rough selection: ~p~n", [[{abbrev_bin(C),I} || {C,I} <- Creds]]),
    [C || {C,V} <- Creds, check_validity(V, UTC)].

cred_recs_by_conn(Conn0) ->
    Conn = normalize_conn(Conn0),
    ?debug("cred_recs_by_conn(~p)~nAll = ~p", [Conn, abbrev(ets:tab2list(?CREDS))]),
    UTC = rvi_common:utc_timestamp(),
    Creds = ets:select(?CREDS, [{ {{Conn,'_'}, '$1'},
				  [], ['$1'] }]),
    ?debug("rough selection: ~p~n", [[abbrev_bin(C#cred.id) || C <- Creds]]),
    [C || C <- Creds, check_validity(C#cred.validity, UTC)].

normalize_conn(local) ->
    local;
normalize_conn({IP, Port} = Conn) when is_binary(IP), is_binary(Port) ->
    Conn;
normalize_conn({IP, Port}) ->
    {to_bin(IP), to_bin(Port)}.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> iolist_to_binary(L);
to_bin(I) when is_integer(I) -> integer_to_binary(I).

filter_by_service_(Services, Conn0) ->
    Conn = normalize_conn(Conn0),
    ?debug("Filter: creds = ~p", [[{K,abbrev_payload(V)} || {K,V} <- ets:tab2list(?CREDS)]]),
    Invoke = ets:select(?CREDS, [{ {{Conn,'_'}, #cred{right_to_invoke = '$1',
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

find_cred_by_service_(Service) ->
    SvcParts = split_path(strip_prot(Service)),
    LocalCreds = ets:select(?CREDS, [{ {{local,'_'}, '$1'}, [], ['$1'] }]),
    ?debug("find_creds_by_service(~p~nLocalCreds = ~p~n",
	   [Service, [{Id,Rcv,Inv} || #cred{id = Id,
					    right_to_invoke = Inv,
					    right_to_receive = Rcv} <- LocalCreds]]),
    case lists:foldl(
	   fun(#cred{right_to_receive = Receive} = C, {Max, _} = Acc) ->
		   case match_length(Receive, SvcParts) of
		       L when L > Max ->
			   {L, C};
		       _ ->
			   Acc
		   end
	   end, {0, none}, LocalCreds) of
	{0, none} ->
	    {error, not_found};
	{_, #cred{id = Id, jwt = JWT}} ->
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
match_length_([[]], _, L) ->
    L;
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
match_svc_([[]], _) ->
    true;
match_svc_([], _) ->
    true;
match_svc_(_, _) ->
    false.

get_env(K) ->
    Res = case setup:get_env(rvi_core, K) of
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
	    ?warning("Cannot read pub key ~p (~p). CWD: ~p~n", [File, Error, file:get_cwd()]),
	    undefined
    end.

get_pub_key_from_cert(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    get_pub_key_from_cert_rec(
	      public_key:pem_entry_decode(hd(public_key:pem_decode(Bin))));
	Error ->
	    error({cannot_read_cert, [Error, {file, File}]})
    end.

get_pub_key_from_cert_rec(#'Certificate'{
			     tbsCertificate =
				 #'TBSCertificate'{
				    subjectPublicKeyInfo =
					#'SubjectPublicKeyInfo'{
					   algorithm =
					       #'AlgorithmIdentifier'{
						  algorithm = Algo},
					   subjectPublicKey = Key}}}) ->
    'RSAPublicKey' = KeyAlg = pubkey_cert_records:supportedPublicKeyAlgorithms(Algo),
    public_key:der_decode(KeyAlg, Key).




create_ets() ->
    create_ets(?CREDS, 1),
    %% create_ets(?KEYS, #key.id),
    create_ets(?CACHE, 1).

create_ets(Tab, KeyPos) ->
    case ets:info(Tab, name) of
	undefined ->
	    ets:new(Tab, [ordered_set, public, named_table,
			  {keypos, KeyPos},
			  {heir, self(), undefined}]);
	_ ->
	    true
    end.

scan_creds(Dir, Key, Cert) ->
    case filelib:is_dir(Dir) of
	true ->
	    UTC = rvi_common:utc_timestamp(),
	    filelib:fold_files(
	      Dir,
	      _FileRegexp = "\\.jwt$",
	      _Recursive = false,
	      fun(F, Acc) ->
		      process_cred(F, Key, Cert, UTC, Acc)
	      end,
	      []);
	false ->
	    ?warning("Cannot read creads: ~p not a directory", [Dir]),
	    []
    end.

process_cred(F, Key, Cert, UTC, Acc) ->
    case file:read_file(F) of
	{ok, Bin} ->
	    try authorize_sig:decode_jwt(strip_nl(Bin), Key) of
		{_, Cred} ->
		    ?info("Unpacked Cred ~p:~n~p~n", [F, abbrev_payload(Cred)]),
		    case process_cred_struct(Cred, Bin, UTC, Cert) of
			invalid ->
			    Acc;
			#cred{} = C ->
			    [C|Acc]
		    end;
		invalid ->
		    ?warning("Invalid cred: ~p~n", [F]),
		    Acc
	    catch
		error:Exception ->
		    ?warning("Cred validation failure (~p): ~p~n",
			     [F, Exception]),
		    Acc
	    end;
	Error ->
	    ?warning("Cannot read cred ~p: ~p~n", [F, Error]),
	    Acc
    end.

strip_nl(Bin) ->
    case re:split(Bin,"\\s+$",[{return,binary},trim]) of
	[Trimmed] -> Trimmed;
	_ -> Bin
    end.

strip_pem_begin_end(<<"-----BEGIN CERTIFICATE-----\n", Bin/binary>>) ->
    re:replace(hd(re:split(Bin, <<"-----END CERTIFICATE-----">>, [])),
	       <<"\\v">>, <<>>, [global,{return,binary}]);
strip_pem_begin_end(Bin) ->
    Bin.


process_cred_struct(Cred, Bin, Cert) ->
    process_cred_struct(Cred, Bin, rvi_common:utc_timestamp(), to_pem(Cert)).

to_pem(Cert) when is_binary(Cert) ->
    %% The Peer cert is assumed to be DER encoded.
    %%  The cert in the cred is PEM-encoded
    PEMCert = public_key:pem_encode([{'Certificate', Cert, not_encrypted}]),
    strip_pem_begin_end(PEMCert);
to_pem(Other) ->
    Other.


process_cred_struct(Cred, Bin, UTC, Cert) ->
    try process_cred_struct_(Cred, Bin, UTC, Cert)
    catch
	error:Err ->
	    ?warning("Failure processing Cred ~p~n~p",
		     [Cred, {Err, erlang:get_stacktrace()}]),
	    invalid
    end.

process_cred_struct_(Cred, Bin, UTC, DevCert) ->
    ID = cred_id(Cred),
    {ok, Receive} = rvi_common:get_json_element(
		      [{'OR', ["right_to_receive", "right_to_register",
			       "sources", "register"]}], Cred),
    {ok, Invoke} = rvi_common:get_json_element(
		    [{'OR', ["right_to_invoke", "destinations", "invoke"]}], Cred),
    {ok, Start} = rvi_common:get_json_element(
		    ["validity", "start"], Cred),
    {ok, Stop}  = rvi_common:get_json_element(
		    ["validity", "stop"], Cred),
    {ok, Cert} = rvi_common:get_json_element(["device_cert"], Cred),
    case DevCert == undefined orelse Cert == DevCert of
	false ->
	    ?warning("Wrong device_cert in cred~n~p~n~p", [Cert, DevCert]),
	    invalid;
	true ->
	    ok
    end,
    ?debug("Start = ~p; Stop = ~p~n", [Start, Stop]),
    Validity = {Start, Stop},
    case check_validity(Start, Stop, UTC) of
	true ->
	    #cred{id = ID,
		  right_to_receive = Receive,
		  right_to_invoke = Invoke,
		  validity = Validity,
		  jwt = Bin,
		  cred = Cred};
	false ->
	    %% Cred outdated
	    ?warning("Outdated cred: Validity = ~p; UTC = ~p~n",
		     [Validity, UTC]),
	    invalid
    end.

cred_id(Cred) ->
    case rvi_common:get_json_element(["id"], Cred) of
	{ok, Id} ->
	    Id;
	{error, undefined} ->
	    ?warning("Cred has no ID: ~p~n", [Cred]),
	    erlang:unique_integer([positive,monotonic])
    end.

check_validity({Start, Stop}, UTC) ->
    check_validity(Start, Stop, UTC).

check_validity(Start, Stop, UTC) ->
    (UTC > Start) andalso (UTC < Stop).

validate_service_call_(Svc, Conn) ->
    Res =
	case lists:filter(fun(C) ->
				  can_invoke(Svc, C)
			  end, cred_recs_by_conn(Conn)) of
	    [] ->
		invalid;
	    [#cred{id = ID}|_] ->
		{ok, ID}
	end,
    ets:insert(?CACHE, {{Svc, Conn}, Res}),
    Res.

cache_authorizations_(Svcs) ->
    CacheEntries = ets:foldl(
		     fun(CEntry, Acc) ->
			     lists:foldr(
			       fun(Svc, Acc1) ->
				       cache_authorization_entry(
					 CEntry, Svc, Acc1)
			       end, Acc, Svcs)
		     end, [], ?CREDS),
    ets:insert(?CACHE, CacheEntries),
    ?debug("auth cache: ~p", [ets:tab2list(?CACHE)]),
    ok.

cache_authorization_entry(Entry, Svc, Acc) ->
    ?debug("cache_authorization_entry(~p, ~p)", [Entry, Svc]),
    case {Entry, Acc} of
	{{{Conn, _}, _C}, [{{Svc, Conn}, {ok,_}}|_]} ->
	    Acc;
	{{{Conn, ID}, C}, Acc} ->
	    case can_invoke(Svc, C) of
		true  ->
		    case Acc of
			[{{Svc, Conn}, invalid}|Rest] ->
			    [{{Svc, Conn}, {ok, ID}}|Rest];
			_ ->
			    [{{Svc, Conn}, {ok, ID}}|Acc]
		    end;
		false ->
		    case Acc of
			[{{Svc, Conn}, invalid}|_] ->
			    Acc;
			_ ->
			    [{{Svc, Conn}, invalid}|Acc]
		    end
	    end
    end.

remove_cached_authorizations_(Svc) ->
    ets:select_delete(?CACHE, [{ {{Svc,'_'},'_'}, [], [true] }]),
    ok.

update_authorization_cache_(Conn, CS) ->
    remove_cached_authorizations_for_conn_(Conn),
    [ok, Svcs] = service_discovery_rpc:get_all_services(CS),
    ?debug("update authorization cache for ~p; Svs = ~p", [Conn, Svcs]),
    lists:foreach(
      fun(Svc) ->
	      validate_service_call_(Svc, Conn)
      end, Svcs),
    ?debug("auth cache: ~p", [ets:tab2list(?CACHE)]).

remove_cached_authorizations_for_conn_(Conn) ->
    ets:select_delete(?CACHE, [{ {{'_', Conn}, '_'}, [], [true] }]),
    ok.

can_invoke(Svc, #cred{right_to_invoke = In}) ->
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

abbrev(B) when is_binary(B) ->
    abbrev_bin(B);
abbrev(X) ->
    abbrev_payload(X).

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

abbrev_payload(Payload) when is_list(Payload) ->
    try lists:map(fun abbrev_pl/1, Payload)
    catch error:_ -> Payload end;
abbrev_payload(PL) ->
    try abbrev_pl(PL)
    catch error:_ -> PL end.

abbrev_jwt({Hdr, Body} = X) ->
    try {Hdr, abbrev_payload(Body)}
    catch error:_ -> X end;
abbrev_jwt(X) ->
    X.

abbrev_pl({K, #cred{} = C}) ->
    {abbrev_pl(K), abbrev_pl(C)};
abbrev_pl(#cred{} = Payload) ->
    list_to_tuple(lists:map(fun(B) when is_binary(B) -> abbrev_bin(B);
			       ([{_,_}|_]=L) -> abbrev_payload(L);
			       (A) -> A
			    end, tuple_to_list(Payload)));
abbrev_pl(#'RSAPublicKey'{} = Key) ->
    pp_key(Key);
abbrev_pl(#'RSAPrivateKey'{} = Key) ->
    pp_key(Key);
abbrev_pl(#'OTPCertificate'{} = Cert) ->
    abbrev_deep_tuple(Cert);
abbrev_pl(#'OTPTBSCertificate'{} = Cert) ->
    abbrev_deep_tuple(Cert);
abbrev_pl({<<"keys">> = K, Ks}) ->
    {K, [abbrev_k(Ky) || Ky <- Ks]};
abbrev_pl({<<"creds">> = C, Cs}) ->
    {C, [abbrev_bin(Cred) || Cred <- Cs]};
abbrev_pl({<<"certificate">> = K, C}) ->
    {K, abbrev_bin(C)};
abbrev_pl({<<"device_cert">> = K, C}) ->
    {K, abbrev_bin(C)};
abbrev_pl({<<"credential">> = K, C}) ->
    {K, abbrev_bin(C)};
abbrev_pl({<<"sign">> = K, S}) when is_binary(S) ->
    {K, abbrev_bin(S)};
abbrev_pl({K, B}) when is_atom(K), is_binary(B), byte_size(B) > 50 ->
    {K, abbrev_bin(B)};
abbrev_pl({K,[{_,_}|_]=L}) ->
    {K, abbrev_pl(L)};
abbrev_pl(B) when is_binary(B) ->
    abbrev_bin(B, 40, 10);
abbrev_pl(L) when is_list(L) ->
    abbrev_payload(L);
abbrev_pl(T) when is_tuple(T), tuple_size(T) > 2 ->
    abbrev_deep_tuple(T);
abbrev_pl(X) ->
    X.

abbrev_deep_tuple(T) when is_tuple(T) ->
    list_to_tuple([abbrev_pl(E) || E <- tuple_to_list(T)]).

abbrev_k(K) ->
    try lists:map(fun abbrev_elem/1, K)
    catch error:_ -> K end.

abbrev_elem({<<"n">>, Bin}) ->
    {<<"n">>, authorize_keys:abbrev_bin(Bin)};
abbrev_elem(X) ->
    X.

log([ID], L, Fmt, Args) ->
    rvi_log:log(ID, L, <<"authorize">>, rvi_log:format(Fmt, Args));
log(_, _, _, _) ->
    ok.
