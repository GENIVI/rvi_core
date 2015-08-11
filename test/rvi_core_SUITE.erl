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
    t_backend_keys/1,
    t_sample_keys_and_cert/1,
    t_install_backend_node/1,
    t_install_sample_node/1,
    t_install_sms_backend_node/1,
    t_install_sms_sample_node/1,
    t_start_basic_backend/1,
    t_start_basic_sample/1
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
       t_backend_keys,
       t_sample_keys_and_cert,
       t_install_backend_node,
       t_install_sample_node,
       t_install_sms_backend_node,
       t_install_sms_sample_node
      ]},
     {test_run, [],
      [
       t_start_basic_backend,
       t_start_basic_sample
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

t_backend_keys(Config) ->
    RootKeyDir = ensure_dir(root_keys()),
    cmd([scripts(),"/rvi_create_root_key.sh -o ",
	 RootKeyDir, "/root -b 2048"]),
    generate_device_keys("basic_backend_keys", Config).

t_sample_keys_and_cert(Config) ->
    Dir = ensure_dir("basic_sample_keys"),
    generate_device_keys(Dir, Config),
    generate_cert(Dir, ensure_dir("basic_sample_certs"), Config).

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

generate_device_keys(Dir, Config) ->
    ensure_dir(Dir),
    cmd([scripts(),"/rvi_create_device_key.py ",
	 "-p ", root_keys(), "/root_priv.pem -o ", Dir, "/dev -b 2048"]).

generate_cert(KeyDir, SampleDir, Config) ->
    ok.

ensure_dir(Dir) ->
    ok = filelib:ensure_dir(filename:join(Dir, "foo")),
    Dir.

env() ->
    "RVI_LOGLEVEL=debug RVI_MYIP=127.0.0.1".

root() ->
    code:lib_dir(rvi_core).

scripts() ->
    [root(), "/scripts"].

root_keys() ->
    "root_keys".

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
