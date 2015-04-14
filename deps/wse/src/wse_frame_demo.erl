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

-module(wse_frame_demo).
-export([run/2]).

-compile(export_all).

run(Ws, Where) ->
    io:format("frame_demo where=~s: called\n", [Where]),

    case Where of
	"i0" ->
	    window_info(Ws, wse:id(Where), [frames]),
	    {ok,Frames} = wse:get(Ws, wse:window(), frames),
	    io:format("Frames object=~p\n", [Frames]),
	    {ok,Length} = wse:get(Ws, Frames, length),
	    io:format("#Frames=~p\n", [Length]),
	    lists:foreach(
	      fun(I) ->
		      %% Access frames and ping the wse in each 
		      {ok,F} = wse:get(Ws, Frames, I),
		      io:format("Frame ~w: ~p\n", [I,F]),
		      {ok,D} = wse:get(Ws, F, document),
		      io:format("Frame Document: ~p\n", [D]),
		      {ok,Wse} = wse:get(Ws, F, 'Wse'),
		      io:format("Frame Wse: ~p\n", [Wse]),
		      {ok,ID} = wse:get(Ws, Wse, id),
		      io:format("Frame Wse: Wse.id=~p\n", [ID]),
		      %% wse:cast(Ws, Wse, notify, [120+I, hello_world]),
		      ok
	      end, lists:seq(0, Length-1)),
	    ok;
	_ ->
	    window_info(Ws, wse:id(Where), [name,innerHeight,innerWidth]),
	    
	    %% sleep short time ?
	    timer:sleep(500),
	    Target = case Where of
			 "i1" -> "w2";
			 "i2" -> "w1"
		     end,
	    io:format("target = ~p\n", [Target]),
	    {ok,Wse} = get_wse_frame(Ws, Target),
	    %% try send message across to other frames wse endpoint
	    case Wse of
		undefined ->
		    ok;
		_ ->
		    io:format("~s: get info of other system ~p\n", 
			      [Where,Target]),
		    Func = wse:newf(Ws, "wse,iref,value",
				    "{ console.debug('frame_demo.reply_fun'); wse.reply(iref,value); }"),
		    Call = [lists,reverse,[[1,2,3]],Func],
		    io:format("rcall ~p\n", [Call]),
		    Ret=wse:rcall(Ws, Wse, call, Call),
		    io:format("rcall return = ~p\n", [Ret]),
		    %% wse:cast(Ws, Wse, notify, [111, [inter_frame,Where]]),
		    ok
	    end,
	    %% wse:close(Ws, done),
	    ok
    end.

get_wse_frame(Ws, Wid) ->
    {ok,P} = wse:get(Ws, wse:window(), parent),    %% p = window.parent,
    io:format("window.parent = ~p\n", [P]),
    {ok,Wse} = wse:get(Ws, P, 'Wse'),
    io:format("window.parent.Wse = ~p\n", [Wse]),
    {ok,WseID} = wse:get(Ws, Wse, id),
    io:format("window.parent.Wse.id = ~p\n", [WseID]),
    {ok,Wse1} = wse:call(Ws, Wse, getWse, [Wid]),
    io:format("Wse1 = ~p\n", [Wse1]),
    {ok,Wse1}.


all_info(Ws, Where) ->
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

    
    
    
