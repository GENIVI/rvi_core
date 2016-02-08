-module(rvi_core_SUITE).

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
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
    t_install_tls_backend_node/1,
    t_install_tls_sample_node/1,
    t_install_tlsj_backend_node/1,
    t_install_tlsj_sample_node/1,
    t_install_tls_backend_noverify_node/1,
    t_install_tls_sample_noverify_node/1,
    t_install_bt_backend_node/1,
    t_install_bt_sample_node/1,
    t_start_basic_backend/1,
    t_start_basic_sample/1,
    t_start_bt_backend/1,
    t_start_bt_sample/1,
    t_start_tls_backend/1,
    t_start_tls_sample/1,
    t_start_tlsj_backend/1,
    t_start_tlsj_sample/1,
    t_start_tls_backend_noverify/1,
    t_start_tls_sample_noverify/1,
    t_register_lock_service/1,
    t_register_sota_service/1,
    t_call_lock_service/1,
    t_call_sota_service/1,
    t_multicall_sota_service/1,
    t_remote_call_lock_service/1,
    t_check_rvi_log/1,
    t_no_errors/1
   ]).

-export([handle_body/4]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-define(DATA, rvi_core_data).

all() ->
    [
     {group, test_install},
     {group, test_run},
     {group, test_run_tls},
     {group, test_run_tlsj},
     {group, test_run_tls_noverify},
     {group, test_run_bt}
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
       t_install_sms_sample_node,
       t_install_tls_backend_node,
       t_install_tls_sample_node,
       t_install_tlsj_backend_node,
       t_install_tlsj_sample_node,
       t_install_tls_backend_noverify_node,
       t_install_tls_sample_noverify_node,
       t_install_bt_backend_node,
       t_install_bt_sample_node
      ]},
     {test_run, [],
      [
       t_start_basic_backend,
       t_start_basic_sample,
       t_register_lock_service,
       t_register_sota_service,
       t_call_lock_service,
       t_call_sota_service,
       t_multicall_sota_service,
       t_remote_call_lock_service,
       t_no_errors
      ]},
     {test_run_tls, [],
      [
       t_start_tls_backend,
       t_start_tls_sample,
       t_register_lock_service,
       t_register_sota_service,
       t_call_lock_service,
       t_remote_call_lock_service,
       t_call_sota_service,
       t_multicall_sota_service,
       t_check_rvi_log,
       t_no_errors
      ]},
     {test_run_tlsj, [],
      [
       t_start_tlsj_backend,
       t_start_tlsj_sample,
       t_register_lock_service,
       t_register_sota_service,
       t_call_lock_service,
       t_call_sota_service,
       t_multicall_sota_service,
       t_remote_call_lock_service,
       t_no_errors
      ]},
     {test_run_tls_noverify, [],
      [
       t_start_tls_backend_noverify,
       t_start_tls_sample_noverify,
       t_register_lock_service,
       t_register_sota_service,
       t_call_lock_service,
       t_remote_call_lock_service,
       t_call_sota_service,
       t_multicall_sota_service,
       t_check_rvi_log,
       t_no_errors
      ]},
     {test_run_bt, [],
      [
       t_start_bt_backend,
       t_start_bt_sample,
       t_register_lock_service,
       t_register_sota_service,
       t_call_lock_service,
       t_call_sota_service,
       t_multicall_sota_service,
       t_remote_call_lock_service,
       t_no_errors
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
    application:stop(exec),
    application:stop(gproc),
    exit(whereis(?DATA), kill),
    ok.

init_per_group(Grp, Config) ->
    {ok, CWD} = file:get_cwd(),
    TestNodes = case Grp of
		    test_run -> ["basic_backend", "basic_sample"];
		    test_run_bt -> ["bt_backend", "bt_sample"];
		    test_run_tls -> ["tls_backend", "tls_sample"];
		    test_run_tlsj -> ["tlsj_backend", "tlsj_sample"];
		    _ -> []
		end,
    [{test_dir, CWD}, {test_nodes, TestNodes} | Config].


end_per_group(test_install, _) ->
    ok;
end_per_group(_, _) ->
    stop_nodes(),
    stop_services(),
    ok.

init_per_testcase(Case, Config) ->
    ct:log("DATA bef. ~p: ~p~n", [Case, ets:tab2list(?DATA)]),
    save(group_leader, group_leader()),
    Config.

end_per_testcase(Case, _Config) ->
    ct:log("DATA aft. ~p: ~p~n", [Case, ets:tab2list(?DATA)]),
    ok.

%% ======================================================================
%% Test cases
%% ======================================================================

t_backend_keys_and_cert(Config) ->
    RootKeyDir = ensure_dir(root_keys()),
    cmd(["openssl genrsa -out ", RootKeyDir, "/root_key.pem 1024"]),
    cmd(["openssl req -x509 -batch -new -nodes -batch"
	 " -key ", RootKeyDir, "/root_key.pem",
	 " -days 365 -out ", RootKeyDir, "/root_cert.crt"]),
    Dir = ensure_dir("basic_backend_keys"),
    CredDir = ensure_dir("basic_backend_creds"),
    generate_device_keys(Dir, Config),
    generate_cred(backend, Dir, CredDir, Config),
    generate_sota_cred(backend, Dir, CredDir, Config).

t_sample_keys_and_cert(Config) ->
    Dir = ensure_dir("basic_sample_keys"),
    generate_device_keys(Dir, Config),
    CredDir = ensure_dir("basic_sample_creds"),
    generate_cred(sample, Dir, CredDir, Config),
    generate_sota_cred(sample, Dir, CredDir, Config).

t_install_backend_node(_Config) ->
    install_rvi_node("basic_backend", env(),
		     [root(), "/priv/test_config/basic_backend.config"]).


t_install_sample_node(_Config) ->
    install_sample_node("basic_sample", "basic_sample.config").

t_install_sms_backend_node(_Config) ->
    install_rvi_node("sms_backend", env(),
		     [root(), "/priv/test_config/sms_backend.config"]).

t_install_sms_sample_node(_Config) ->
    install_sample_node("sms_sample", "sms_sample.config").

t_install_tls_backend_node(_Config) ->
    install_rvi_node("tls_backend", env(),
		     [root(), "/priv/test_config/tls_backend.config"]).

t_install_tls_sample_node(_Config) ->
    install_sample_node("tls_sample", "tls_sample.config").

t_install_tlsj_backend_node(_Config) ->
    install_rvi_node("tlsj_backend", env(),
		     [root(), "/priv/test_config/tlsj_backend.config"]).

t_install_tlsj_sample_node(_Config) ->
    install_sample_node("tlsj_sample", "tlsj_sample.config").

t_install_tls_backend_noverify_node(_Config) ->
    install_rvi_node("tls_backend_noverify", env(),
		     [root(), "/priv/test_config/tls_backend_noverify.config"]).

t_install_tls_sample_noverify_node(_Config) ->
    install_sample_node("tls_sample_noverify", "tls_sample_noverify.config").

t_install_bt_backend_node(_Config) ->
    install_rvi_node("bt_backend", env(),
		     [root(), "/priv/test_config/bt_backend.config"]).

t_install_bt_sample_node(_Config) ->
    install_sample_node("bt_sample", "bt_sample.config").

generic_start(Name) ->
    F = filename:join([".", Name, "start_me.sh"]),
    Cmd = [env(),
	   " ./", Name, "/rvi.sh",
	   " -s ", Name,
	   " -l ./", Name, "/rvi/log",
	   " -d ./", Name,
	   " -c ./", Name, "/priv/test_config/", Name, ".config",
	   " $1"],
    ok = save_cmd(F, Cmd),
    cmd([F, " start"]),
    await_started(Name).

t_start_basic_backend(_Config) ->
    generic_start("basic_backend").

t_start_basic_sample(_Config) ->
    generic_start("basic_sample").

t_start_bt_backend(_Config) ->
    generic_start("bt_backend").

t_start_bt_sample(_Config) ->
    generic_start("bt_sample").

t_start_tls_backend(_Config) ->
    generic_start("tls_backend").

t_start_tls_sample(_Config) ->
    generic_start("tls_sample").

t_start_tlsj_backend(_Config) ->
    generic_start("tlsj_backend").

t_start_tlsj_sample(_Config) ->
    generic_start("tlsj_sample").

t_start_tls_backend_noverify(_Config) ->
    generic_start("tls_backend_noverify").

t_start_tls_sample_noverify(_Config) ->
    generic_start("tls_sample_noverify").

t_register_lock_service(_Config) ->
    Pid =
	spawn_cmd(
	  [python(),
	   "/rvi_service.py -n ", service_edge("sample"), " lock"]),
    save({service, lock}, Pid),
    timer:sleep(2000).

t_register_sota_service(_Config) ->
    Pid = start_json_rpc_server(9987),
    reg_service_request("sample", <<"sota">>, <<"http://localhost:9987">>),
    save({service, sota}, Pid),
    timer:sleep(2000).

t_call_sota_service(_Config) ->
    call_sota_service_(sota_client, sota_bin()).

t_multicall_sota_service(_Config) ->
    Data = <<"abc">>,
    Pids = [spawn_monitor(fun() ->
				  exit({ok, call_sota_service_(N, Data)})
			  end)
	    || N <- [client1,
		     client2,
		     client3,
		     client4,
		     client5]],
    collect(Pids).

collect([{Pid, Ref} | T]) ->
    receive
	{'DOWN', Ref, _, _, {ok, ok}} ->
	    collect(T);
	{'DOWN', Ref, _, _, Reason} ->
	    [exit(P, kill) || {P,_} <- T],
	    error(Reason)
    after 30000 ->
	    error(timeout)
    end;
collect([]) ->
    ok.


call_sota_service_(RegName, Data) ->
    {Mega, Secs, _} = os:timestamp(),
    Timeout = Mega * 1000000 + Secs + 60,
    register(RegName, self()),
    json_rpc_request(service_edge("backend"),
		     <<"message">>,
		     [
		      {<<"service_name">>, <<"jlr.com/vin/abc/sota">>},
		      {<<"timeout">>, Timeout},
		      {<<"parameters">>,
		       [
			{<<"data">>, Data},
			{<<"sendto">>, atom_to_binary(RegName, latin1)},
			{<<"rvi.max_msg_size">>, 100}
		       ]}
		     ]),
    receive
	{message, [{service_name, <<"/sota">>},
		   {data, Data}]} = Res ->
	    ct:log("got json_rpc_result: ~p", [Res]),
	    ok;
	{message, Other} ->
	    ct:log("wrong message: ~p", [Other]),
	    error({unmatched, Other})
    after 30000 ->
	    error(timeout)
    end.

t_call_lock_service(_Config) ->
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

t_remote_call_lock_service(_Config) ->
    CallPid = spawn_cmd(
                [python(),
                 "/rvi_call.py -n ", service_edge("backend"),
                 " jlr.com/vin/abc/lock arg1='val2'"]),
    timer:sleep(2000),
    [{_, Svc}] = lookup({service, lock}),
    ok = fetch(
           Svc,
           {match, <<"Service invoked![\\s]*args: {u'arg1': u'val2'}">>}),
    ct:log("Verified service invoked~n", []),
    CallRes = fetch(CallPid),
    verify_call_res(join_stdout_msgs(CallRes)),
    ct:log("CallRes = ~p~n", [CallRes]).

t_check_rvi_log(_Config) ->
    [URLb, URLs] = [rvi_log_addr(Node) || Node <- ["backend", "sample"]],
    {Method, Args} = {<<"fetch">>, [{<<"tid">>, <<".*">>}]},
    Logs =
	[{URL, unwrap_log(
		 json_rpc_request(URL, Method, Args))}
	 || URL <- [URLb, URLs]],
    ct:log("Logs = ~p", [Logs]),
    ok.

unwrap_log(Res) ->
    {_, R} = lists:keyfind(<<"result">>, 1, Res),
    {_, 0} = lists:keyfind(<<"status">>, 1, R),
    {_, L} = lists:keyfind(<<"log">>, 1, R),
    lists:flatmap(
      fun({Id, Es}) ->
	      [unwrap_event(Id, E) || E <- Es]
      end, L).

unwrap_event(Id, [{<<"ts">>,T},{<<"lvl">>,L},{<<"cmp">>,C},{<<"evt">>,E}]) ->
    {Id, T, binary_to_integer(L), C, E}.

json_rpc_request(URL, Method, Args) ->
    Req = jsx:encode([{<<"jsonrpc">>, <<"2.0">>},
		      {<<"id">>, 1},
		      {<<"method">>, Method},
		      {<<"params">>, Args}]),
    Hdrs = [{'Content-Type', "application/json"}],
    json_result(exo_http:wpost(URL, {1, 1}, Hdrs, Req, 1000)).

reg_service_request(Node, Svc, URL) ->
    check_reg_svc_response(
      json_rpc_request(service_edge(Node),
		       <<"register_service">>,
		       [{<<"service">>, Svc},
			{<<"network_address">>, URL}])).

check_reg_svc_response(JSON) ->
    ct:log("check_reg_svc: ~p", [JSON]),
    {_, Res} = lists:keyfind(<<"result">>, 1, JSON),
    {_, 0} = lists:keyfind(<<"status">>, 1, Res),
    ok.

sota_bin() ->
    <<"11111111111111111111111111111111111111111111111111"
      "22222222222222222222222222222222222222222222222222"
      "33333333333333333333333333333333333333333333333333"
      "44444444444444444444444444444444444444444444444444"
      "55555555555555555555555555555555555555555555555555"
      "66666666666666666666666666666666666666666666666666"
      "77777777777777777777777777777777777777777777777777"
      "88888888888888888888888888888888888888888888888888"
      "99999999999999999999999999999999999999999999999999"
      "00000000000000000000000000000000000000000000000000">>.

json_result({ok, {http_response, {_V1, _V2}, 200, _Text, _Hdr}, JSON}) ->
    jsx:decode(JSON).

start_json_rpc_server(Port) ->
    {ok, Pid} = exo_http_server:start(Port, [{request_handler,
					      {?MODULE, handle_body, [foo]}}]),
    save({server,Port}, Pid),
    Pid.

handle_body(Socket, Request, Body, St) ->
    ct:log("handle_body(Body = ~p)", [Body]),
    JSON = jsx:decode(Body),
    ct:log("Got JSON Req: ~p", [JSON]),
    case JSON of
	[{<<"jsonrpc">>, <<"2.0">>},
	 {<<"id">>, ID},
	 {<<"method">>, <<"message">>},
	 {<<"params">>,
	  [{<<"service_name">>, SvcName},
	   {<<"parameters">>,
	    [ {<<"data">>, Data},
	      {<<"sendto">>, SendTo},
	      {<<"rvi.max_msg_size">>, _}]}
	  ]}] ->
	    binary_to_existing_atom(SendTo, latin1)
		! {message, [{service_name, SvcName},
			     {data, Data}]},
	    Reply = [{<<"jsonrpc">>, <<"2.0">>},
		     {<<"id">>, ID},
		     {<<"result">>, <<"ok">>}],
	    exo_http_server:response(
	      Socket, undefined, 200, "OK",
	      jsx:encode(Reply),
	      [{'content_type', "application/json"}]);
	Other ->
	    exo_http_server:response(
	      Socket, undefined, 501, "Internal Error", "Internal Error"),
	    error({unrecognized, Other})
    end.

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

join_stdout_msgs_rev(L) ->
    lists:foldr(
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

%% Ensure that we have the right group leader
log(Fmt, Args) ->
    [{_, GL}] = lookup(group_leader),
    group_leader(GL, self()),
    ct:log(Fmt, Args).

fetch(Pid) ->
    fetch(Pid, fetch).

fetch(Pid, Op) ->
    ct:log("fetch(~p)", [Pid]),
    Pid ! {self(), Op},
    Fetched = receive
		  {Pid, Res} ->
		      Res
	      after 3000 ->
		      error(timeout)
	      end,
    ct:log("Fetch result = ~p", [Fetched]),
    Fetched.

cmd_loop() -> cmd_loop([], []).

cmd_loop(Acc, Pats) ->
    receive
        {From, fetch} ->
            log("~p <- fetch; Acc = ~p~n", [self(), Acc]),
            From ! {self(), lists:reverse(Acc)},
            cmd_loop(Acc, Pats);
        {From, {match, Pat}} ->
            log("~p <- match ~p; Acc = ~p~n", [self(), Pat, Acc]),
            case try_match(Pat, join_stdout_msgs_rev(Acc)) of
                true ->
                    From ! {self(), ok},
                    cmd_loop(Acc, Pats);
                false ->
                    cmd_loop(Acc, [{From, Pat}|Pats])
            end;
        Msg ->
            log("~p <- ~p~n", [self(), Msg]),
            Acc1 = [Msg|Acc],
            Bin = join_stdout_msgs_rev(Acc1),
            Pats1 = lists:foldr(
                      fun({P, Pat}, Ps) ->
                              case re:run(Bin, Pat) of
                                  {match,_} -> P ! {self(), ok},
                                               Ps;
                                  nomatch   -> [{P, Pat}|Ps]
                              end
                      end, [], Pats),
            cmd_loop(Acc1, Pats1)
    end.

try_match(Pat, Data) ->
    Match = re:run(Data, Pat),
    Res = (Match =/= nomatch),
    ct:log("try_match(S, ~p) -> ~p~nS=~s~n", [Pat, Res, Data]),
    Res.

generate_device_keys(Dir, _Config) ->
    ensure_dir(Dir),
    RootKeyDir = root_keys(),
    cmd(["openssl genrsa -out ", Dir, "/device_key.pem 2014"]),
    cmd(["openssl req -new -batch"
	 " -subj ", subj(),
	 " -key ", Dir, "/device_key.pem"
	 " -out ", Dir, "/device_cert.csr"]),
    cmd(["openssl x509 -req -days 365"
	 " -in ", Dir, "/device_cert.csr"
	 " -CA ", root_keys(), "/root_cert.crt"
	 " -CAkey ", RootKeyDir, "/root_key.pem"
	 " -set_serial 01"
	 " -out ", Dir, "/device_cert.crt"]).

generate_cred(sample, KeyDir, CredDir, _Config) ->
    %% Don't put lock_cred.json in the certs directory, since rvi_core
    %% will report a parse failure for it.
    UUID = uuid(),
    {Start, Stop} = start_stop(),
    cmd([scripts(), "/rvi_create_credential.py"
	 " --id=", UUID,
	 " --issuer=GENIVI"
	 " --device_cert=", KeyDir, "/device_cert.crt",
	 " --start='", Start, "'"
	 " --stop='", Stop, "'"
	 " --root_key=", root_keys(), "/root_key.pem"
	 " --register='jlr.com/vin/abc/unlock jlr.com/vin/abc/lock'"
	 " --invoke='jlr.com/backend/set_state'"
	 " --jwt_out=", CredDir, "/lock_cred.jwt"
	 " --cred_out=", KeyDir, "/lock_cred.json"]),
    ok;
generate_cred(backend, KeyDir, CertDir, _Config) ->
    UUID = uuid(),
    {Start, Stop} = start_stop(),
    cmd([scripts(), "/rvi_create_credential.py"
	 " --id=", UUID,
	 " --issuer=GENIVI"
	 " --device_cert=", KeyDir, "/device_cert.crt",
	 " --start='", Start, "'"
	 " --stop='", Stop, "'"
	 " --root_key=", root_keys(), "/root_key.pem"
	 " --register='jlr.com'"
	 " --invoke='jlr.com'"
	 " --jwt_out=", CertDir, "/backend_cred.jwt"
	 " --cred_out=", KeyDir, "/backend_cred.json"]),
    ok.

generate_sota_cred(sample, KeyDir, CredDir, _Config) ->
    %% Don't put sota_cred.json in the certs directory, since rvi_core
    %% will report a parse failure for it.
    UUID = uuid(),
    {Start, Stop} = start_stop(),
    cmd([scripts(), "/rvi_create_credential.py"
	 " --id=", UUID,
	 " --issuer=GENIVI"
	 " --device_cert=", KeyDir, "/device_cert.crt",
	 " --start='", Start, "'"
	 " --stop='", Stop, "'"
	 " --root_key=", root_keys(), "/root_key.pem"
	 " --register='jlr.com/vin/abc/store'"
	 " --invoke='jlr.com/backend/set_state'"
	 " --jwt_out=", CredDir, "/sota_cred.jwt"
	 " --cred_out=", KeyDir, "/sota_cred.json"]),
    ok;
generate_sota_cred(backend, KeyDir, CertDir, _Config) ->
    UUID = uuid(),
    {Start, Stop} = start_stop(),
    cmd([scripts(), "/rvi_create_credential.py"
	 " --id=", UUID,
	 " --issuer=GENIVI"
	 " --device_cert=", KeyDir, "/device_cert.crt",
	 " --start='", Start, "'"
	 " --stop='", Stop, "'"
	 " --root_key=", root_keys(), "/root_key.pem"
	 " --register='jlr.com'"
	 " --invoke='jlr.com'"
	 " --jwt_out=", CertDir, "/sota_backend_cred.jwt"
	 " --cred_out=", KeyDir, "/sota_backend_cred.json"]),
    ok.

subj() ->
    "/C=US/ST=OR/O=JLR/localityName=Portland/organizationalUnitName=Ostc".



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
    "RVI_LOGLEVEL=debug RVI_MYIP=127.0.0.1 RVI_BACKEND=127.0.0.1".

root() ->
    code:lib_dir(rvi_core).

scripts() ->
    [root(), "/scripts"].

python() ->
    [root(), "/python"].

root_keys() ->
    "root_keys".

service_edge("backend") -> "http://localhost:8801";
service_edge("sample" ) -> "http://localhost:9001".

rvi_log_addr("backend") -> "http://localhost:8809";
rvi_log_addr("sample" ) -> "http://localhost:9009".

install_rvi_node(Name, Env, _ConfigF) ->
    Root = code:lib_dir(rvi_core),
    Scripts = filename:join(Root, "scripts"),
    ct:log("Root = ~p", [Root]),
    Cmd = lists:flatten(
	    [Env, " ", Scripts, "/rvi_install.sh ./", Name]),

    ct:log("Cmd = `~s`", [Cmd]),
    Res = cmd(Cmd),
    ct:log("install_rvi_node/1 -> ~p", [Res]),


    Res1 = cmd(lists:flatten(["install -d --mode 0755 ./", Name])),
    ct:log("install_rvi_node/2 -> ~p", [Res1]),

    Res2 = cmd(lists:flatten(["cp -r ", Root, "/priv ", Name])),
    ct:log("install_rvi_node/3 -> ~p", [Res2]),

    Res2.

install_sample_node(Name, ConfigF) ->
    Env = [env(),
	   " RVI_BACKEND=127.0.0.1 RVI_PORT=9000"
	   " RVI_MY_NODE_ADDR=127.0.0.1:9000"],
    install_rvi_node(Name, Env,
		     [root(), "/priv/test_config/", ConfigF]).

%% in_priv_dir(F, Cfg) ->
%%     %% PrivDir = ?config(priv_dir, Cfg),
%%     %% in_dir(PrivDir, F, Cfg).
%%     F(Cfg).

cmd(C) ->
    cmd(C, []).

cmd(C, Opts) ->
    {ok, _Res} = cmd_(C, Opts).

save_cmd(F, Cmd) ->
    ct:log("save_cmd ~s:~n~s", [F, Cmd]),
    ok = file:write_file(F, iolist_to_binary(Cmd)),
    {ok, FI} = file:read_file_info(F),
    ok = file:write_file_info(F, FI#file_info{mode = 8#755}, []).

cmd_(C0, Opts) ->
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
    CmdRes.

cmd_res({_, L}) ->
    {Err,_L1} = take(stderr, L, ""),
    {Out,L2} = take(stdout, L, ""),
    {Out, Err, L2}.

take(K, L, Def) ->
    case lists:keytake(K, 1, L) of
	false ->
	    {Def, L};
	{value, {_, V}, Rest} ->
	    {V, Rest}
    end.

verify_killed({ok, []}) ->
    ok;
verify_killed(CmdRes) ->
    case cmd_res(CmdRes) of
	{_, [], _} -> ok;
	{_, Err, _} ->
	    case re:run(Err, "No such process", []) of
		{match, _} ->
		    ok;
		nomatch ->
		    error({failed_kill, Err})
	    end
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

%% get_ospid(Node) ->
%%     lookup({Node, pid}).

stop_nodes() ->
    Nodes = ets:select(?DATA, [{ {{'$1',pid},'$2'}, [], [{{'$1','$2'}}] }]),
    ct:log("Stopping, Nodes = ~p~n", [Nodes]),
    rpc:multicall([N || {N,_} <- Nodes], init, stop, []),
    [verify_killed(cmd_(["kill -9 ", P], [])) || {_,P} <- Nodes],
    [delete_node(N) || {N,_} <- Nodes].

stop_services() ->
    Services =
	ets:select(?DATA, [{ {{service,'_'},'_'}, [], ['$_'] },
			   { {{server,'_'},'_'}, [], ['$_'] }]),
    ct:log("Stopping services: ~p~n", [Services]),
    [exit(Pid, shutdown) || {_, Pid} <- Services],
    ok.

delete_node(N) ->
    ct:log("delete_node(~p)", [N]),
    ets:delete(?DATA, {N, pid}).

lookup(Key) ->
    ets:lookup(?DATA, Key).

save(Key, Data) ->
    ets:insert(?DATA, {Key, Data}).

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

t_no_errors(Config) ->
    no_errors(?config(test_nodes, Config), ?config(test_dir, Config)).

no_errors(Dirs, PDir) ->
    ct:log("Will check errors in ~p", [Dirs]),
    true = lists:all(
	     fun(D) ->
		     no_errors_(filename:join([PDir, D, "rvi", "log", "lager"]), D)
	     end, Dirs),
    ok.

no_errors_(Dir, Name) ->
    lists:all(fun(F) ->
		      log_is_empty(filename:join(Dir, F), F, Name)
	      end, ["error.log", "crash.log"]).

log_is_empty(Log, F, Name) ->
    case file:read_file(Log) of
	{ok, <<>>} ->
	    true;
	{ok, Content} ->
	    ct:log("~s: ~s is not empty:~n~s", [Name, F, Content]),
	    false;
	{error, Reason} ->
	    ct:log("~s: Cannot read log ~s (~p)", [Name, F, Reason]),
	    false
    end.
