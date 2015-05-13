-module(authorize_keys).

-export([get_key_pair/0]).
-export([public_key_to_json/1,
	 json_to_public_key/1]).

-include_lib("lager/include/log.hrl").
-include_lib("public_key/include/public_key.hrl").

public_key_to_json(#'RSAPublicKey'{modulus = N, publicExponent = E}) ->
    {struct, [{"kty", "RSA"},
	      {"alg", "RS256"},
	      {"use", "sig"},
	      {"kid", "1"},
	      {"e", base64url:encode(binary:encode_unsigned(E))},
	      {"n", base64url:encode(binary:encode_unsigned(N))}
	     ]}.

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
    case application:get_env(rvi, key_pair, undefined) of
	undefined ->
	    {undefined, undefined};
	{openssl_pem, Pem} ->
	    get_key_pair_from_pem(openssl, Pem)
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
