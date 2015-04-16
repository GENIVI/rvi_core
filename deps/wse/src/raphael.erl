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
%%% File    : raphael.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Raphael interface towards browser
%%% Created : 21 Dec 2009 by Tony Rogvall <tony@rogvall.se>

-module(raphael).

-compile(export_all).

%% New returns a reference to the canvase object
new(Ws, Array) ->
    wse:call(Ws, wse:window(), "Raphael", [Array]).

new(Ws, Object, W, H) ->
    wse:call(Ws, wse:window(), "Raphael", [Object,W,H]).

new(Ws, X, Y, W, H) ->
    wse:call(Ws, wse:window(), "Raphael", [X,Y,W,H]).

setSize(Ws,Paper,W,H) ->
    wse:call(Ws, Paper, setSize, [W, H]).

close(Ws,Paper) ->
    wse:call(Ws, Paper, clone, []).

%% get attribute value
attr(Ws,Object,Name) ->
    wse:call(Ws,Object,attr,[Name]).

%% set attribute value
attr(Ws,Object,Name,Value) ->
    wse:call(Ws,Object,attr,[Name,Value]).


circle(Ws,Paper, X, Y, R) ->
    wse:call(Ws, Paper, circle, [X, Y, R]).

rect(Ws,Paper, X, Y, W, H) ->
    wse:call(Ws, Paper, rect, [X, Y, W, H]).

rect(Ws,Paper, X, Y, W, H, R) ->
    wse:call(Ws, Paper, rect, [X, Y, W, H, R]).

ellipse(Ws,Paper,X,Y,Rx,Ry) ->
    wse:call(Ws,Paper,ellipse,[X,Y,Rx,Ry]).

image(Ws,Paper,Src,X,Y,Width,Height) ->
    wse:call(Ws,Paper,image,[Src,X,Y,Width,Height]).

set(Ws,Paper) ->
    wse:call(Ws,Paper,set,[]).

push(Ws,Set,Objects) when is_list(Objects) ->
    wse:call(Ws,Set,push,Objects);
push(Ws, Set, Object) ->
    wse:call(Ws,Set,push,[Object]).

%% FIXME: utf8!
text(Ws,Paper,X,Y,Text) ->
    wse:call(Ws,Paper,text,[X,Y,Text]).

%% return a font object
getFont(Ws,Paper,Family) ->
    wse:call(Ws,Paper,getFont,[Family]).

print(Ws,Paper,X,Y,Text,Font,FontSize) ->
    wse:call(Ws,Paper,print,[X,Y,Text,Font,FontSize]).

path(Ws,Paper,SVG) ->
    wse:call(Ws,Paper,path,[SVG]).

path(Ws,Paper) ->
    wse:call(Ws,Paper,path,[]).



    
    


    





	    
    


