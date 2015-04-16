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

-module(wse_document_demo).
-export([run/2]).

-compile(export_all).

run(Ws, Where) ->
    io:format("document_demo: called\n"),

    screen_info(Ws,  wse:id(Where), 
		[availHeight, availWidth,colorDepth, 
		 height, pixelDepth, width ]),

    window_info(Ws, wse:id(Where),
		[closed, innerHeight,innerWidth,length,name,
		 outerHeight,outerWidth]),

    navigator_info(Ws, wse:id(Where), 
		   [appCodeName,appName,appVersion,cookieEnabled,
		    language,onLine,platform,product,userAgent]),

    document_info(Ws,  wse:id(Where), 
		  [cookie,docType,inputEncoding,lastModified,
		   referrer,domain,baseURI,title,'URL']),
    %% wse:close(Ws, done),
    ok.

screen_info(Ws, Parent, As=[A|_]) when is_atom(A) ->
    [ info(Ws, wse:screen(), "screen", What, Parent) || What <- As ].

window_info(Ws, Parent, As=[A|_]) when is_atom(A) ->
    [ info(Ws, wse:window(), "window", What, Parent) || What <- As ].

navigator_info(Ws, Parent, As=[A|_]) when is_atom(A) ->
    [ info(Ws, wse:navigator(), "navigator", What, Parent) || What <- As ].

document_info(Ws, Parent, As=[A|_]) when is_atom(A) ->
    [ info(Ws, wse:document(), "document", What, Parent) || What <- As ].

info(Ws, Object, ObjName, What, Parent) ->
    {ok,Value} = wse:get(Ws, Object, What),
    Text = ObjName++"."++to_text(What)++"="++to_text(Value),
    ElemNode = wse:createElement(Ws, "p"),
    TextNode = wse:createTextNode(Ws, Text),
    wse:appendChild(Ws, ElemNode, TextNode),
    wse:appendChild(Ws, Parent, ElemNode).

to_text(X) ->
    lists:flatten(io_lib:format("~p", [X])).
