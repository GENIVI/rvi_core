-module(authorize_keys).
-behaviour(gen_server).

-export([get_key_pair/0,
	 get_key_pair_from_pem/2,
	 get_pub_key/1,
	 authorize_jwt/0,
	 provisioning_key/0,
	 signed_public_key/2,
	 save_keys/2]).
-export([get_certificates/0,
	 get_certificates/1]).
-export([public_key_to_json/1,
	 json_to_public_key/1]).

-export([self_signed_public_key/0]).  % just temporary

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
	       sources = [],
	       destinations = [],
	       validity = [],
	       jwt,
	       cert}).

-record(key, {id,
	      key}).

-define(CERTS, authorize_certs).
-define(KEYS,  authorize_keys).

public_key_to_json(#'RSAPublicKey'{modulus = N, publicExponent = E}) ->
    {struct, [{"kty", "RSA"},
	      {"alg", "RS256"},
	      {"use", "sig"},
	      {"kid", "1"},
	      {"e", base64url:encode(binary:encode_unsigned(E))},
	      {"n", base64url:encode(binary:encode_unsigned(N))}
	     ]}.

self_signed_public_key() ->
    Key = filename:join([code:priv_dir(rvi), "keys",
			 "self_provisioning_key.pem"]),
    {Priv, _} = get_key_pair_from_pem(openssl, Key),
    MyPub = authorize_rpc:public_key(),
    signed_public_key(MyPub, Priv).

signed_public_key(MyPub, Priv) ->
    JSON = {struct, [
		     {"keys", {array, [public_key_to_json(MyPub)]}}
		    ]},
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

get_certificates() ->
    get_certificates(local).

get_certificates(Conn) ->
    gen_server:call(?MODULE, {get_certificates, Conn}).

provisioning_key() ->
    gen_server:call(?MODULE, provisioning_key).

save_keys(Keys, Conn) ->
    gen_server:call(?MODULE, {save_keys, Keys, Conn}).

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
    ?debug("ProvisioningKey = ~p~n", [ProvisioningKey]),
    CertDir = setup:verify_dir(get_env(cert_dir)),
    {ok, AuthJwt} = file:read_file(get_env(authorize_jwt)),
    ?debug("CertDir = ~p~n", [CertDir]),
    Certs = scan_certs(CertDir, ProvisioningKey),
    [ets:insert(?CERTS, {{local, C#cert.id}, C}) || C <- Certs],
    {ok, #st{provisioning_key = ProvisioningKey,
	     cert_dir = CertDir,
	     authorize_jwt = AuthJwt}}.

handle_call(authorize_jwt, _, S) ->
    {reply, S#st.authorize_jwt, S};
handle_call(provisioning_key, _, S) ->
    {reply, S#st.provisioning_key, S};
handle_call({get_certificates, Conn}, _, S) ->
    Certs = certs_by_conn(Conn),
    {reply, Certs, S};
handle_call({save_keys, Keys, Conn}, _, S) ->
    ?debug("save_keys: Keys=~p, Conn=~p~n", [Keys, Conn]),
    {reply, ok, S};
handle_call(_, _, S) ->
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
    UTC = rvi_common:utc_timestamp(),
    Certs = ets:select(?CERTS, [{ {{Conn,'_'}, #cert{jwt = '$1',
						     validity = '$2',
						     _='_'}},
				  [], [{{'$1', '$2'}}] }]),
    [C || {C,V} <- Certs, check_validity(V, UTC)].

get_env(K) ->
    case application:get_env(rvi, K) of
	{ok, V} ->
	    V;
	_ ->
	    undefined
    end.

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

get_pub_key(File) ->
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
	    ?warning("Cannot read pub key ~s (~p)~n", [File, Error]),
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
	    try authorize_sig:decode_jwt(Bin, Key) of
		{_, Cert} ->
		    ID = cert_id(Cert),
		    ?info("Cert ~p loaded (~p)~n", [ID, F]),
		    {ok, Sources} = rvi_common:get_json_element(
				      ["sources"], Cert),
		    {ok, Dests} = rvi_common:get_json_element(
				    ["destinations"], Cert),
		    {ok, Validity} = rvi_common:get_json_element(
				       ["validity"], Cert),
		    {Start, Stop} = parse_validity(Validity),
		    ?debug("Start = ~p; Stop = ~p~n", [Start, Stop]),
		    case check_validity(Start, Stop, UTC) of
			true ->
			    [#cert{id = ID,
				   sources = Sources,
				   destinations = Dests,
				   validity = {Start, Stop},
				   jwt = Bin,
				   cert = Cert} | Acc];
			false ->
			    %% Cert outdated
			    ?warning("Outdated cert: ~p~nValidity = ~p~n"
				     "UTC = ~p~n", [F, Validity, UTC]),
			    Acc
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

parse_validity(V) ->
    Start = rvi_common:get_json_element(["start"], V),
    Stop  = rvi_common:get_json_element(["stop"] , V),
    {Start, Stop}.
