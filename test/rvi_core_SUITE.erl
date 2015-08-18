-module(rvi_core_SUITE).

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    t_backend_keys_and_cert/1,
    t_sample_keys_and_cert/1,
    t_install_backend_node/1,
    t_install_sample_node/1,
    t_install_sms_backend_node/1,
    t_install_sms_sample_node/1,
    t_start_basic_backend/1,
    t_start_basic_sample/1,
    t_register_lock_service/1,
    t_call_lock_service/1
   ]).

-include_lib("common_test/include/ct.hrl").

-define(DATA, rvi_core_data).

all() ->
    [
     {group, test_install},
     {group, test_run}
    ].

groups() ->
    [
     {test_install, [],
      %% Note that order is significant in this test group.
      %% The test cases produce files on disk that are used in later tests
      [
       t_backend_keys_and_cert,
       t_sample_keys_and_cert,
       t_install_backend_node,
       t_install_sample_node,
       t_install_sms_backend_node,
       t_install_sms_sample_node
      ]},
     {test_run, [],
      [
       t_start_basic_backend,
       t_start_basic_sample,
       t_register_lock_service,
       t_call_lock_service
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    spawn(fun() ->
		  register(?DATA, self()),
		  ets:new(?DATA, [set, public, named_table]),
		  receive stop -> ok end
	  end),
    ok = application:start(gproc),
    ok = application:start(exec),
    Config.

end_per_suite(_Config) ->
    stop_nodes(),
    application:stop(exec),
    application:stop(gproc),
    exit(whereis(?DATA), kill),
    ok.

init_per_testcase(Case, Config) ->
    ct:log("DATA bef. ~p: ~p~n", [Case, ets:tab2list(?DATA)]),
    Config.

end_per_testcase(Case, _Config) ->
    ct:log("DATA aft. ~p: ~p~n", [Case, ets:tab2list(?DATA)]),
    ok.

%% ======================================================================
%% Test cases
%% ======================================================================

t_backend_keys_and_cert(Config) ->
    RootKeyDir = ensure_dir(root_keys()),
    cmd([scripts(),"/rvi_create_root_key.sh -o ",
	 RootKeyDir, "/root -b 2048"]),
    Dir = ensure_dir("basic_backend_keys"),
    generate_device_keys(Dir, Config),
    generate_cert(backend, Dir, ensure_dir("basic_backend_certs"), Config).

t_sample_keys_and_cert(Config) ->
    Dir = ensure_dir("basic_sample_keys"),
    generate_device_keys(Dir, Config),
    generate_cert(sample, Dir, ensure_dir("basic_sample_certs"), Config).

t_install_backend_node(Config) ->
    install_rvi_node("basic_backend", env(),
		     [root(), "/test/config/backend.config"]).


t_install_sample_node(Config) ->
    Env = [env(),
	   " RVI_BACKEND=127.0.0.1 RVI_PORT=8900"
	   " RVI_MY_NODE_ADDR=127.0.0.1:8900"],
    install_rvi_node("basic_sample", Env,
		     [root(), "/test/config/sample.config"]).

t_install_sms_backend_node(Config) ->
    install_rvi_node("sms_backend", env(),
		     [root(), "/test/config/sms_backend.config"]).

t_install_sms_sample_node(Config) ->
    install_rvi_node("sms_sample", env(),
		     [root(), "/test/config/sms_sample.config"]).

t_start_basic_backend(Config) ->
    cmd([scripts(), "/rvi_node.sh -d -n basic_backend"]),
    await_started("basic_backend"),
    ok.

t_start_basic_sample(Config) ->
    cmd([scripts(), "/rvi_node.sh -d -n basic_sample"]),
    await_started("basic_sample"),
    ok.

t_register_lock_service(Config) ->
    Pid =
	spawn_cmd(
	  [python(),
	   "/rvi_service.py -n ", service_edge("sample"), " lock"]),
    save({service, lock}, Pid),
    timer:sleep(2000).

t_call_lock_service(Config) ->
    CallPid = spawn_cmd(
		[python(),
		 "/rvi_call.py -n ", service_edge("sample"),
		 " jlr.com/vin/abc/lock arg1='val1'"]),
    timer:sleep(2000),
    [{_, Svc}] = lookup({service, lock}),
    SvcRes = fetch(Svc),
    verify_service_res(join_stdout_msgs(SvcRes)),
    ct:log("SvcRes = ~p~n", [SvcRes]),
    CallRes = fetch(CallPid),
    verify_call_res(join_stdout_msgs(CallRes)),
    ct:log("CallRes = ~p~n", [CallRes]).

verify_service_res(Bin) ->
    {match,_} =
	re:run(Bin, <<"Service:[\\h]*jlr.com/vin/abc/lock">>, []),
    {match,_} =
	re:run(Bin, <<"Service invoked![\\s]*args: {u'arg1': u'val1'}">>, []),
    nomatch = re:run(Bin, <<"Traceback">>, []),
    ok.

verify_call_res(Bin) ->
    nomatch = re:run(Bin, <<"Traceback">>, []),
    ok.

join_stdout_msgs(L) ->
    lists:foldl(
      fun({stdout,_,Bin}, Acc) ->
	      <<Acc/binary, Bin/binary>>;
	 (_, Acc) ->
	      Acc
      end, <<>>, L).

spawn_cmd(Cmd0) ->
    Cmd = binary_to_list(iolist_to_binary(Cmd0)),
    Me = self(),
    Pid = spawn(fun() ->
			Res = exec:run(Cmd, [stdin, stdout, stderr]),
			ct:log("~s ->~n~p~n", [Cmd, Res]),
			Me ! {self(), ok},
			cmd_loop()
		end),
    receive
	{Pid, ok} ->
	    Pid
    end.

fetch(Pid) ->
    ct:log("fetch(~p)", [Pid]),
    Pid ! {self(), fetch},
    receive
	{Pid, Res} ->
	    Res
    after 3000 ->
	    error(timeout)
    end.

cmd_loop() -> cmd_loop([]).

cmd_loop(Acc) ->
    receive
	{From, fetch} ->
	    From ! {self(), lists:reverse(Acc)},
	    cmd_loop([]);
	Msg ->
	    io:fwrite(user, "~p <- ~p", [self(), Msg]),
	    cmd_loop([Msg|Acc])
    end.

generate_device_keys(Dir, Config) ->
    ensure_dir(Dir),
    cmd([scripts(),"/rvi_create_device_key.py ",
	 "-p ", root_keys(), "/root_priv.pem -o ", Dir, "/dev -b 2048"]).

generate_cert(sample, KeyDir, CertDir, Config) ->
    %% Don't put lock_cert.json in the certs directory, since rvi_core
    %% will report a parse failure for it.
    UUID = uuid(),
    {Start, Stop} = start_stop(),
    cmd([scripts(), "/rvi_create_certificate.py"
	 " --id=", UUID,
	 " --device_key=", KeyDir, "/dev_pub.pem",
	 " --start='", Start, "'"
	 " --stop='", Stop, "'"
	 " --root_key=", root_keys(), "/root_priv.pem"
	 " --register='jlr.com/vin/abc/unlock jlr.com/vin/abc/lock'"
	 " --invoke='jlr.com/backend/set_state'"
	 " --jwt_out=", CertDir, "/lock_cert.jwt"
	 " --cert_out=", KeyDir, "/lock_cert.json"]),
    ok;
generate_cert(backend, KeyDir, CertDir, Config) ->
    UUID = uuid(),
    {Start, Stop} = start_stop(),
    cmd([scripts(), "/rvi_create_certificate.py"
	 " --id=", UUID,
	 " --device_key=", KeyDir, "/dev_pub.pem",
	 " --start='", Start, "'"
	 " --stop='", Stop, "'"
	 " --root_key=", root_keys(), "/root_priv.pem"
	 " --register='jlr.com'"
	 " --invoke='jlr.com'"
	 " --jwt_out=", CertDir, "/backend_cert.jwt"
	 " --cert_out=", KeyDir, "/backend_cert.json"]),
    ok.

start_stop() ->
    DT = erlang:localtime(),
    GS = calendar:datetime_to_gregorian_seconds(DT),
    Start_GS = GS - 3600,   % valid since one hour ago
    Stop_GS = GS + 24*3600, % valid for the next 24 hrs
    Start = calendar:gregorian_seconds_to_datetime(Start_GS),
    Stop = calendar:gregorian_seconds_to_datetime(Stop_GS),
    {fmt_date(Start), fmt_date(Stop)}.

fmt_date({{Y,Mo,D}, {H,Mi,S}}) ->
    io_lib:fwrite("~4..0w-~2..0w-~2..0w "
		  "~2..0w:~2..0w:~2..0w", [Y,Mo,D,H,Mi,S]).

ensure_dir(Dir) ->
    ok = filelib:ensure_dir(filename:join(Dir, "foo")),
    Dir.

env() ->
    "RVI_LOGLEVEL=debug RVI_MYIP=127.0.0.1".

root() ->
    code:lib_dir(rvi_core).

scripts() ->
    [root(), "/scripts"].

python() ->
    [root(), "/python"].

root_keys() ->
    "root_keys".

service_edge("backend") -> "http://localhost:8801";
service_edge("sample" ) -> "http://localhost:8901".

install_rvi_node(Name, Env, ConfigF) ->
    Root = code:lib_dir(rvi_core),
    Scripts = filename:join(Root, "scripts"),
    ct:log("Root = ~p", [Root]),
    Cmd = lists:flatten(
	    [Env, " ", Scripts, "/setup_rvi_node.sh -d -n ", Name,
	     " -c ", ConfigF]),
    ct:log("Cmd = `~s`", [Cmd]),
    Res = cmd(Cmd),
    ct:log("install_rvi_node/1 -> ~p", [Res]),
    Res.

in_priv_dir(F, Cfg) ->
    %% PrivDir = ?config(priv_dir, Cfg),
    %% in_dir(PrivDir, F, Cfg).
    F(Cfg).

cmd(C) ->
    cmd(C, []).

cmd(C0, Opts) ->
    C = binary_to_list(iolist_to_binary(C0)),
    CmdRes = exec:run(C, [sync, stdout, stderr] ++ Opts),
    {Fmt, Args} =
	case cmd_res(CmdRes) of
	    {Out, "", []}  -> {"> ~s~n~s", [C, Out]};
	    {Out, Err, []} -> {"> ~s~n~s~nERR: ~s", [C, Out, Err]};
	    {Out, Err, Rest} ->
		{"> ~s~n~s~nERR: ~s~nRest = ~p", [C, Out, Err, Rest]}
	end,
    ct:log(Fmt, Args),
    {ok, Res} = CmdRes.

cmd_res({_, L}) ->
    {Err,L1} = take(stderr, L, ""),
    {Out,L2} = take(stdout, L, ""),
    {Out, Err, L2}.

take(K, L, Def) ->
    case lists:keytake(K, 1, L) of
	false ->
	    {Def, L};
	{value, {_, V}, Rest} ->
	    {V, Rest}
    end.

%%% ============================================================
await_started(Name) ->
    timer:sleep(1000),
    Node = node_name(Name),
    ct:log("Node = ~p", [Node]),
    true = net_kernel:hidden_connect(Node),
    save_ospid(Node),
    timer:sleep(3000),
    Res = gproc:await(Node, {n,l,rvi_core}, 10000),
    ct:log("await_started(~p) -> ~p", [Node, Res]),
    ok.

save_ospid(Node) ->
    save({Node,pid}, rpc:call(Node, os, getpid, [])).

get_ospid(Node) ->
    lookup({Node, pid}).

stop_nodes() ->
    Nodes = ets:select(?DATA, [{ {{'$1',pid},'$2'}, [], [{{'$1','$2'}}] }]),
    ct:log("Stopping, Nodes = ~p~n", [Nodes]),
    rpc:multicall([N || {N,_} <- Nodes], init, stop, []),
    [cmd(["kill -9 ", P]) || {_,P} <- Nodes],
    [ets:delete(?DATA, K) || K <- Nodes].

lookup(Key) ->
    ets:lookup(rvi_core_data, Key).

save(Key, Data) ->
    ets:insert(rvi_core_data, {Key, Data}).

node_name(Name) ->
    [_, Host] = re:split(atom_to_list(node()), "@", [{return, list}]),
    list_to_atom(Name ++ "@" ++ Host).

%% Copied from gsms_plivo.erl
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


json_rpc(URL, Method, Args) ->
    Req = binary_to_list(
	    iolist_to_binary(
	      exo_json:encode({struct, [{"jsonrpc", "2.0"},
					{"id", 1},
					{"method", Method},
					{"params", Args}]}))),
    Hdrs = [{'Content-Type', "application/json"}],
    exo_http:wpost(URL, {1,1}, Hdrs, Req, 1000).
