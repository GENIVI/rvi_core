%% -*- mode: erlang; indent-tabs-mode: nil; -*-
-module(auth).

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
    {_, Cmd} = lists:keyfind(command, 1, Opts),
    ?verbose("Cmd = ~p; Options = ~p~n",
	     [Cmd, lists:keydelete(command, 1, Opts)]),
    cmd(Cmd, Opts).

opts(["-v"   , "true"  |T]) -> [{verbose, true}|opts(T)];
opts(["-v"   , "false" |T]) -> [{verbose, false}|opts(T)];
opts(["-v"             |T]) -> [{verbose, true}|opts(T)];
opts(["-pub" , PubKey  |T]) -> [{pub, PubKey}|opts(T)];
opts(["-root", RootKey |T]) -> [{root, RootKey}|opts(T)];
opts(["-o"   , OutF    |T]) -> [{out, OutF}|opts(T)];
opts(["-c"   , Cert    |T]) -> [{cert, Cert}|opts(T)];
opts(["-fmt" , Fmt     |T]) -> [{fmt, Fmt}|opts(T)];
opts([Cmd]) ->
    [{command, Cmd}];
opts([]) ->
    [].

check_verbose(Opts) ->
    V = get_value(verbose, Opts, false),
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
	    case authorize_keys:get_key_pair_from_pem(openssl, Root) of
		{undefined, undefined} ->
		    fail("Cannot read root key (~p)~n", [Root]);
		{RPriv, _RPub} ->
		    case authorize_keys:get_pub_key(Pub) of
			undefined ->
			    fail("Cannot read pub key (~p)~n", [Pub]);
			PubKey ->
			    make_auth(RPriv, PubKey, Fmt, Opts)
		    end
	    end
    end.

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
      "Usage: " ++ excript:script_name() ++ "[Options] Cmd~n"
      "Options:~n"
      "  -pem PemFile   : Name of OpenSSL (2048 bit RSA) pem key file~n"
      "  -c CertFile    : Name of JSON-formatted RVI Certificate~n"
      "  -fmt Format    : Output format (json|jwt)~n"
      "  -o OutFile     : Name of output (tty output, if not specified)~n"
      "Command:~n"
      "  auth           : ~n", []).

fail(Fmt, Args) ->
    io:fwrite(Fmt, Args),
    help(),
    erlang:halt(1).


