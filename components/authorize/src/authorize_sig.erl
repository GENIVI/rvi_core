-module(authorize_sig).

-compile(export_all).

-include_lib("lager/include/log.hrl").
-define(DIGEST_TYPE, sha256).

%% Inspired by
%% http://blog.differentpla.net/blog/2015/04/19/jwt-rs256-erlang/
decode_jwt(JWT, PubKey) ->
    ?debug("authorize_sig:decode_jwt(JWT, PubKey=~p)~n", [PubKey]),
    [H, P, S] = binary:split(JWT, <<".">>, [global]),
    Header = decode_json(base64url:decode(H)),
    Payload = decode_json(base64url:decode(P)),
    Signature = base64url:decode(S),
    SigningInput = <<H/binary, ".", P/binary>>,
    case public_key:verify(SigningInput, ?DIGEST_TYPE, Signature, PubKey) of
	false ->
	    invalid;
	true ->
	    {Header, Payload}
    end.

encode_jwt(JSON, PrivKey) ->
    encode_jwt(JSON, header(), PrivKey).

encode_jwt(Payload0, Header0, PrivKey) ->
    ?debug("encode_jwt(~p,~p,_)~n", [ensure_json(Payload0),
				     ensure_json(Header0)]),
    Header = base64url:encode(ensure_json(Header0)),
    Payload = base64url:encode(ensure_json(Payload0)),
    SigningInput = <<Header/binary, ".", Payload/binary>>,
    Signature = base64url:encode(
		  public_key:sign(SigningInput, ?DIGEST_TYPE, PrivKey)),
    <<SigningInput/binary, ".", Signature/binary>>.

header() ->
    "{\"alg\": \"RS256\"}".

ensure_json("{" ++ _ = JSON) ->
    list_to_binary(JSON);
ensure_json(<<"{", _/binary>> = JSON) ->
    JSON;
ensure_json({struct, _} = JSON) ->
    list_to_binary(exo_json:encode(JSON)).

decode_json("{" ++ _ = JSON) ->
    {ok, Res} = exo_json:decode_string(JSON),
    Res;
decode_json(<<"{", _/binary>> = JSON) ->
    {ok, Res} = exo_json:decode_string(binary_to_list(JSON)),
    Res.
