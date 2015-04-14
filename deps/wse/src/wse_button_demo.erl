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
%%%      Demo a button with notification callback
%%% @end
%%% Created : 16 Jun 2014 by Tony Rogvall <tony@rogvall.se>

-module(wse_button_demo).

-export([run/2]).

%% Button demo
run(Ws, Where) ->
    io:format("button_demo: called\n"),
    Button = wse:createElement(Ws, "button"),
    io:format("button=~w\n", [Button]),
    Text = wse:createTextNode(Ws, "Press me"),
    io:format("text=~w\n", [Text]),
    wse:appendChild(Ws, Button, Text),
    wse:appendChild(Ws, wse:id(Where), Button),
    {ok,ID} = wse:create_event(Ws),
    io:format("ID=~w\n", [ID]),
    Func = wse:newf(Ws, "",
		    "{ Wse.notify("++integer_to_list(ID)++",'click'); }"),
    wse:set(Ws,Button,"onclick",Func),
    run_loop(Ws, Text, ID).

run_loop(Ws, Text, ID) ->
    receive
	{notify,ID,_Local,Data} ->
	    io:format("~p\n", [Data]),
	    wse:set(Ws, Text, "nodeValue", "Again"),
	    run_loop(Ws, Text, ID)
    end.
