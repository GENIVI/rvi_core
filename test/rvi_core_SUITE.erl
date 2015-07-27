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
    test_install_backend_node/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, test_install}
    ].

groups() ->
    [
     {test_install, [shuffle],
      [
       test_install_backend_node
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% ======================================================================
%% Test cases
%% ======================================================================

test_install_backend_node(Config) ->
    ct:log("Current working dir: ~p", [file:get_cwd()]),
    ct:log("Config = ~p", [Config]),
    PrivDir = ?config(priv_dir, Config),
    ct:log("PrivDir = ~p", [PrivDir]),
    in_dir(PrivDir, fun(Cfg) -> install_backend_node_(Cfg) end, Config),
    ok.

install_backend_node_(_Config) ->
    Root = code:lib_dir(rvi_core),
    Scripts = filename:join(Root, "scripts"),
    ct:log("Root = ~p", [Root]),
    Cmd = lists:flatten(
	    ["RVI_LOGLEVEL=debug RVI_MYIP=127.0.0.1 ",
	     Scripts, "/setup_rvi_node.sh -d -n backend -c ",
	     Root, "/rvi_backend.config"]),
    ct:log("Cmd = `~s`", [Cmd]),
    Res = os:cmd(Cmd),
    ct:log("install_backend_node/1 -> ~s", [Res]),
    Res.

in_dir(D, F, Cfg) ->
    {ok, Cur} = file:get_cwd(),
    try
	ok = file:set_cwd(D),
	F(Cfg)
    after
	file:set_cwd(Cur)
    end.
