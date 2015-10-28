%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Marina Westman Lönne <malotte@malotte.net>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Test suite for ale.
%%% @end
%%% Created : 2012 by Marina Westman Lönne <malotte@malotte.net>
%%%-------------------------------------------------------------------
-module(ale_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,10}}].


%%--------------------------------------------------------------------
%% @doc
%%  Returns the list of groups and test cases that
%%  are to be executed.
%%
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
all() -> 
    [start_of_ale_app,
     start_with_it_console,
     start_with_it_file,
     start_with_it_no_file,
     add_trace,
     add_trace_gl,
     add_trace_pid,
     add_trace_and_die,
     add_trace_wrong_level,
     remove_non_existing_trace].
%%     break].



%%--------------------------------------------------------------------
%% @doc
%% Initialization before the whole suite
%%
%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    start_system(),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the whole suite
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(ale),
    stop_system(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_testcase(TC, Config) ->
    ct:pal("Init testcase: ~p", [TC]),
    tc_init(TC, Config).

tc_init(start_with_it_file, Config) ->
    os:cmd("touch ./ale.log"),
    Config;
tc_init(_Any, Config) ->
    Config.


%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% @end
%%--------------------------------------------------------------------
end_per_testcase(TC, Config) ->
    ct:pal("End testcase: ~p", [TC]),
    tc_end(TC, Config).

tc_end(start_of_ale_app, _Config) ->
    application:stop(ale),
    ct:pal("Stopped ale."),
    ok;
tc_end(start_with_it_file, Config) ->
    os:cmd("rm ./ale.log"),
    tc_end(any, Config);
tc_end(_Any, _Config) ->
    ale_srv:stop(),
    ct:pal("Stopped ale server."),
    ok.


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc 
%% Start of ale application.
%% @end
%%--------------------------------------------------------------------
-spec start_of_ale_app(Config::list(tuple())) -> ok.

start_of_ale_app(_Config) ->
    ale:start(),
    ok = ale_srv:dump(),
    ct:pal("Ale up and running"),
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% Start of ale with init trace to console.
%% @end
%%--------------------------------------------------------------------
-spec start_with_it_console(Config::list(tuple())) -> ok.

start_with_it_console(_Config) ->
    {ok, Pid} = ale_srv:start([{init_traces, 
				[{[{module, ale_SUITE}], debug}]}]),
    ok = ale_srv:dump(),
    [{trace_item, {[{module, ale_SUITE}], debug, console}, _LagRef, Pid}] = 
	ale_srv:traces(),
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% Start of ale with init trace to file.
%% @end
%%--------------------------------------------------------------------
-spec start_with_it_file(Config::list(tuple())) -> ok.

start_with_it_file(_Config) ->
    {ok, Pid} = ale_srv:start([{init_traces, 
				[{[{module, ale_SUITE}], debug, "./ale.log"}]}]),
    ok = ale_srv:dump(),
    [{trace_item, {[{module, ale_SUITE}], debug, _File}, _LagRef, Pid}] = 
	ale_srv:traces(),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Start of ale with init trace refering to non-existing file.
%% @end
%%--------------------------------------------------------------------
-spec start_with_it_no_file(Config::list(tuple())) -> ok.

start_with_it_no_file(_Config) ->
    {ok, _Pid} = ale_srv:start([{init_traces, 
				[{[{module, ale_SUITE}], debug, "./ale.log"}]}]),
    ok = ale_srv:dump(),
    [] = ale_srv:traces(),
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% Add/remove trace on module.
%% @end
%%--------------------------------------------------------------------
-spec add_trace(Config::list(tuple())) -> ok.

add_trace(_Config) ->
    Self = self(),
    {ok, _Pid} = ale_srv:start(),
    ok = ale_srv:dump(),
    ok = ale:trace(on, ale_SUITE, debug),
    [{trace_item, {[{module, ale_SUITE}], debug, console}, _LagRef, Self}] = 
	ale_srv:traces(),
    ok = ale:trace(off, ale_SUITE, debug),
    [] = ale_srv:traces(),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Add/remove trace on module with groupleader.
%% @end
%%--------------------------------------------------------------------
-spec add_trace_gl(Config::list(tuple())) -> ok.

add_trace_gl(_Config) ->
    GL = group_leader(),
    {ok, _Pid} = ale_srv:start(),
    ok = ale:trace_gl(on, ale_SUITE, info),
    ok = ale_srv:dump(),
    [{trace_item, {[{module, ale_SUITE}], info, console}, _LagRef, GL}] = 
	ale_srv:traces(),
    ok = ale:trace_gl(off, ale_SUITE, info),
    [] = ale_srv:traces(),
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% Add/remove trace on pid.
%% @end
%%--------------------------------------------------------------------
-spec add_trace_pid(Config::list(tuple())) -> ok.

add_trace_pid(_Config) ->
    Self = self(),
    SelfStr = pid_to_list(Self),
    {ok, _Pid} = ale_srv:start(),
    ok = ale:trace(on, Self, debug),
    ok = ale_srv:dump(),
    [{trace_item, {[{pid, SelfStr}], debug, console}, _LagRef, Self}] = 
	ale_srv:traces(),
    ok = ale:trace(off, Self, debug),
    [] = ale_srv:traces(),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Have tmp processes add trace and die.
%% @end
%%--------------------------------------------------------------------
-spec add_trace_and_die(Config::list(tuple())) -> ok.

add_trace_and_die(_Config) ->
    {ok, _AlePid} = ale_srv:start(),
    TmpPid = proc_lib:spawn(?MODULE, trace_starter, []),
    timer:sleep(100),
    ok = ale_srv:dump(),
    [{trace_item, {[{module, ale_SUITE}], debug, console}, _LagRef, TmpPid}] = 
	ale_srv:traces(),
    TmpPid ! die,
    timer:sleep(100),
    [] = ale_srv:traces(),
    ok.

trace_starter() ->
    ok = ale:trace(on, ale_SUITE, debug),
    receive
	die ->
	    ct:pal("Told to die")
    after
	1000 ->
	    ct:fail("Timeout")
    end.

%%--------------------------------------------------------------------
%% @doc 
%% Add trace with incorrect level.
%% @end
%%--------------------------------------------------------------------
-spec add_trace_wrong_level(Config::list(tuple())) -> ok.

add_trace_wrong_level(_Config) ->
    {ok, _AlePid} = ale_srv:start(),
    {error, E} = ale:trace(on, ale_SUITE, wrong),
    ok = ale_srv:dump(),
    ct:pal("Error ~p", [E]),
    [] = ale_srv:traces(),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Remove non existing trace.
%% @end
%%--------------------------------------------------------------------
-spec remove_non_existing_trace(Config::list(tuple())) -> ok.

remove_non_existing_trace(_Config) ->
    {ok, _AlePid} = ale_srv:start(),
    %% Is ok OK??
    ok = ale:trace(off, ale_SUITE, debug),
    ok = ale_srv:dump(),
    [] = ale_srv:traces(),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Dummy test case to have a test environment running.
%% Stores Config in ets table.
%% @end
%%--------------------------------------------------------------------
-spec break(Config::list(tuple())) -> ok.

break(Config) ->
    ets:new(config, [set, public, named_table]),
    ets:insert(config, Config),
    test_server:break("Break for test development\n" ++
		     "Get Config by ets:tab2list(config)"),
    ok.


%%--------------------------------------------------------------------
%% Help functions
%%--------------------------------------------------------------------
start_system() ->
    app_ctrl([kernel, stdlib, compiler, syntax_tools, sasl, lager],start).
    
stop_system() ->
    app_ctrl([lager], stop).

app_ctrl([], _F) ->
    ok;
app_ctrl([App|Apps], F) ->
    error_logger:info_msg("~p: ~p\n", [F,App]),
    case application:F(App) of
	{error,{not_started,App1}} ->
	    case F of
		start ->
		    app_ctrl([App1,App|Apps], F);
		stop ->
		    app_ctrl(Apps, F)
	    end;
	{error,{already_started,App}} ->
	    app_ctrl(Apps, F);
	ok ->
	    app_ctrl(Apps, F);
	Error ->
	    Error
    end.
