%% -*- mode: erlang; indent-tabs-mode: nil; -*-
-module(author).

-define(verbose(Fmt, Args), case verbose() of true ->
				    io:fwrite(Fmt, Args);
				false ->
				    ok
			    end).

-export([main/1]).

-import(proplists, [get_value/2, get_value/3]).

main([]) ->
    help(),
    halt(1);
main(Args) ->
    Opts = opts(Args),
    check_verbose(Opts),
    case lists:keyfind(command, 1, Opts) of
        {_, Cmd} ->
            ?verbose("Cmd = ~p; Options = ~p~n",
                     [Cmd, lists:keydelete(command, 1, Opts)]),
            cmd(Cmd, Opts);
        false ->
            fail("No command given~n", [])
    end.

opts(["-v"   , "true"  |T]) -> [{v, true}|opts(T)];
opts(["-v"   , "false" |T]) -> [{v, false}|opts(T)];
opts(["-v"             |T]) -> [{v, true}|opts(T)];
opts(["-pub" , PubKey  |T]) -> [{pub, PubKey}|opts(T)];
opts(["-root", RootKey |T]) -> [{root, RootKey}|opts(T)];
opts(["-sig", SigFile  |T]) -> [{sig, SigFile}|opts(T)];
opts(["-o"   , OutF    |T]) -> [{out, OutF}|opts(T)];
opts(["-c"   , Cert    |T]) -> [{cert, Cert}|opts(T)];
opts(["-b"   , Bits    |T]) -> [{b, l2i(Bits)}|opts(T)];
opts(["-fmt" , Fmt     |T]) -> [{fmt, Fmt}|opts(T)];
opts([Cmd]) ->
    [{command, Cmd}];
opts([]) ->
    [].

check_verbose(Opts) ->
    V = get_value(v, Opts, false),
    put({?MODULE, verbose}, V),
    V.

verbose() ->
    get({?MODULE, verbose}).

cmd("make_auth", Opts) ->
    case {get_value(root, Opts), get_value(pub, Opts), get_value(fmt, Opts)} of
	{undefined, _, "jwt"} ->
	    fail("Cannot create JWT without root key~n", []);
	{_Root, undefined, "jwt"} ->
	    fail("Cannot create JWT without pub key~n", []);
	{Root, Pub, Fmt} ->
            {RPriv, _} = get_key_pair(Root),
            PubKey = get_pub_key(Pub),
            make_auth(RPriv, PubKey, Fmt, Opts)
    end;
cmd("make_root", Opts) ->
    [Out] = mandatory([out], Opts),
    Bits = bits(Opts),
    make_root(Out, Bits, Opts);
cmd("make_dev", Opts) ->
    [Root, Out] = mandatory([root, out], Opts),
    Bits = bits(Opts),
    make_dev(Root, Out, Bits, Opts);
cmd("read_sig", Opts) ->
    [Root, Sig] = mandatory([root, sig], Opts),
    {_, Pub} = get_key_pair(Root),
    case file:read_file(Sig) of
        {ok, JWT} ->
            case authorize_sig:decode_jwt(JWT, Pub) of
                invalid ->
                    fail("Cannot validate ~s~n", [Sig]);
                {Header, Payload} ->
                    io:fwrite("Header: ~s~n"
                              "Payload: ~s~n",
                              [exo_json:encode(Header),
                               exo_json:encode(Payload)])
            end;
        {error, E} ->
            fail("Cannot read ~s (~w)~n", [Sig, E])
    end.

make_root_msg(X) ->
    {"~s_priv.pem - private root key~n"
     "~s_pub.pem  - public root key~n"
     "~n"
     "Use ./author -root ~s_priv.pem [-b <bits>] -o <device_key_file>"
     " make_dev~n"
     "to create a device key pair signed by the generated private root key.~n"
     "~n"
     "Use ./rvi_create_certificate.py ... --root_key=~s_priv.pem ...~n"
     "to sign a created certificate with the generated private root key.~n",
     [X, X, X, X]}.

get_key_pair(Root) ->
    case authorize_keys:get_key_pair_from_pem(openssl, Root) of
        {undefined, undefined} ->
            fail("Cannot read root key (~p)~n", [Root]);
        {_RPriv, _RPub} = Pair ->
            Pair
    end.

get_pub_key(Pub) ->
    case authorize_keys:get_pub_key(Pub) of
        undefined ->
            fail("Cannot read pub key (~p)~n", [Pub]);
        PubKey ->
            PubKey
    end.

mandatory(Keys, Opts) ->
    lists:map(
      fun(K) ->
              case lists:keyfind(K, 1, Opts) of
                  false ->
                      fail("Mandatory option: -~w~n", [K]);
                  {_, V} ->
                      V
              end
      end, Keys).

bits(Opts) ->
    get_value(b, Opts, 2048).

l2i(Str) ->
    try list_to_integer(Str)
    catch
        error:_ ->
            fail("Invalid number argument, ~s~n", [Str])
    end.

i2l(I) ->
    integer_to_list(I).


make_auth(RPriv, Pub, Fmt, Opts) ->
    case Fmt of
	"json" ->
	    JSON = authorize_sig:ensure_json(
		     authorize_keys:public_key_to_json(Pub)),
	    out(JSON, Opts);
	"jwt" ->
	    JWT = authorize_keys:signed_public_key(Pub, RPriv),
	    out(JWT, Opts)
    end.

make_root(Out, Bits, _Opts) ->
    make_key_pair(Out, Bits),
    {Fmt, Args} = make_root_msg(Out),
    io:fwrite(Fmt, Args).

make_dev(Root, Out, Bits, Opts) ->
    make_key_pair(Out, Bits),
    {RPriv, _} = get_key_pair(Root),
    Pub = get_pub_key(pub_f(Out)),
    make_auth(RPriv, Pub, "jwt", [{out, Out ++ "_pub_sign.jwt"}|Opts]).

make_key_pair(Out, Bits) ->
    os:cmd(["openssl genrsa -out ", priv_f(Out), " ", i2l(Bits)]),
    os:cmd(["openssl rsa -pubout -in ", priv_f(Out), " -out ", pub_f(Out)]).

priv_f(Out) -> Out ++ "_priv.pem".
pub_f (Out) -> Out ++ "_pub.pem".

out(Str, Opts) ->
    case get_value(out, Opts, tty) of
	tty ->
	    io:fwrite("~s", [Str]);
	OutF when is_list(OutF) ->
	    case file:open(OutF, [write]) of
		{ok, Fd} ->
		    try io:fwrite(Fd, "~s", [Str])
		    after
			file:close(Fd)
		    end;
		Error ->
		    fail("Cannot write output (~p): ~p~n", [OutF, Error])
	    end
    end.

help() ->
    io:fwrite(
      "Usage: " ++ escript:script_name() ++ "[Options] Cmd~n"
      "Options:~n"
      "  -pem PemFile   : Name of OpenSSL (2048 bit RSA) pem key file~n"
      "  -c CertFile    : Name of JSON-formatted RVI Certificate~n"
      "  -fmt Format    : Output format (json|jwt)~n"
      "  -o OutFile     : Name of output (tty output, if not specified)~n"
      "Command:~n"
      "  make_auth      : Create an authorization JWT~n"
      "   Options: -root <RootKeyFile> -pub <PubKey> [-o <Outfile>]~n"
      "  make_root      : Create a provisioning key pair~n"
      "   Options: [-b <Bits>] -o <Outfile>~n"
      "  read_sig       : Validate and read a JWT~n"
      "   Options: -sig <SignatureFile> -root <RootKey>~n", []).

fail(Fmt, Args) ->
    io:fwrite(Fmt, Args),
    help(),
    erlang:halt(1).


