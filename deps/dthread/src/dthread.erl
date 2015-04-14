%%%---- BEGIN COPYRIGHT -------------------------------------------------------
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
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    dthread test 
%%% @end
%%% Created :  6 May 2012 by Tony Rogvall <tony@rogvall.se>

-module(dthread).

-compile(export_all).


open() ->
    case erl_ddll:load_driver(code:priv_dir(dthread), "dthread_drv") of
	ok ->
	    open_port({spawn_driver, dthread_drv}, [binary]);
	{error,Error} ->
	    io:format("erl_ddll: error:\n~s\n",
		      [erl_ddll:format_error(Error)]),
	    {error, Error}
    end.

close(Port) ->
    port_close(Port).

ctl1(Port) ->
    port_control(Port, 1, "hello").

ctl2(Port) ->
    port_control(Port, 2, "olleh").

command1(Port) ->
    port_command(Port, "Hello").

call1(Port, Value) when is_integer(Value) ->
    ValueBin = <<Value:32>>,
    Resp = port_control(Port, 100, [ValueBin]),
    %% io:format("port_control: value=~w, resp = ~w\n", [ValueBin,Resp]),
    case Resp of
	<<0, RefNum:32>> ->
	    %% io:format("wait for ref = ~p\n", [RefNum]),
	    receive 
		{RefNum, Value1} ->
		    {ok,Value1};
		Other ->
		    io:format("Got ~p\n", [Other]),
		    {error, Other}
	    end;
	<<1, Error/binary>> ->
	    {error, binary_to_atom(Error, latin1)}
    end.

seq_call(Port, N) ->
    lists:foreach(
      fun(I) ->
	      case call1(Port, I) of
		  {ok,I1} ->
		      io:format("~w: result (~w)\n", [I,I1]);
		  Error ->
		      io:format("~w: result error=~w\n",[I,Error])
	      end	      
      end, lists:seq(1, N)).

par_call(Port, N) ->
    lists:foreach(
      fun(I) ->
	      spawn(fun() -> 
			    io:format("~w: call\n", [I]),
			    case call1(Port, I) of
				{ok,I1} ->
				    io:format("~w: result (~w)\n", [I,I1]);
				Error ->
				    io:format("~w: result error=~w\n",[I,Error])
			    end
		    end)
      end, lists:seq(1, N)).

