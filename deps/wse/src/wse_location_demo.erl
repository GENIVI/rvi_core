%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
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
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%      This demo is called directly from web page
%%% @end
%%% Created :  24 Apr 2014 by Tony Rogvall <tony@rogvall.se>

-module(wse_location_demo).
-export([run/2,
	 goto/1]).

-compile(export_all).

run(Ws,_Id) ->
    io:format("location_demo: called\n"),
    register(wse_location_demo, self()),
    listen_loop(Ws).

goto(Location) ->
    wse_location_demo ! {goto, Location}.

listen_loop(Ws) ->
    receive 
	{goto, Location} ->
	    io:format("goto: ~p~n", [Location]),
	    ok = wse:set(Ws,  wse:window(), ["location", "href"], Location);
	{'DOWN',_Mon,process,_Pid,Reason} ->
	    io:format("process crashed: ~p~n", [Reason])
    end,
    listen_loop(Ws).
