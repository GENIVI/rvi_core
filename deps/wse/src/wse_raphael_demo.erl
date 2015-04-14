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
%%% Created :  9 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(wse_raphael_demo).

-export([run/2]).

%%
%% Small demo
%%
run(Ws, Where) ->
    io:format("raphael_demo: called\n"),
    ok = wse:load(Ws, "raphael-min.js"),
    {ok,Paper} = raphael:new(Ws, Where, 640, 540),

    %% must create the image element here!
    {ok,Image} = wse:load_image(Ws, "bd.jpg"),
    {ok,Src} = wse:get(Ws, Image, "src"),
    {ok,_I1} = raphael:image(Ws, Paper, Src, 140, 140, 320, 240),
    {ok,I2}  = raphael:image(Ws, Paper, Src, 140, 380, 320, 240),
    raphael:attr(Ws, I2, "transform", "s1-1"),
    raphael:attr(Ws, I2, "opacity", "0.5"),
    {ok,R6} = raphael:rect(Ws, Paper, 0, 380, 600, 160),
    raphael:attr(Ws, R6, "fill", "90-#333-#333"),
    raphael:attr(Ws, R6, "stroke", "none"),    
    raphael:attr(Ws, R6, "opacity", "0.5"),


    {ok,C1} = raphael:circle(Ws, Paper, 10, 10, 10),
    raphael:attr(Ws, C1, "fill", "#FF0000"),
    {ok,C2} = raphael:circle(Ws, Paper, 40, 10, 10),
    raphael:attr(Ws, C2, "fill", "#00FF00"),
    {ok,C3} = raphael:circle(Ws, Paper, 70, 10, 10),
    raphael:attr(Ws, C3, "fill", "#0000FF"),
    {ok,C4} = raphael:circle(Ws, Paper, 100, 10, 10),
    raphael:attr(Ws, C4, "fill", "90-#fff-#000"),
    
    {ok,R1} = raphael:rect(Ws, Paper, 10, 30, 10, 10),
    raphael:attr(Ws, R1, "fill", "#FF0000"),    
    {ok,R2} = raphael:rect(Ws, Paper, 40, 30, 10, 10),
    raphael:attr(Ws, R2, "fill", "#00FF00"),
    
    {ok,R3} = raphael:rect(Ws, Paper, 70, 30, 10, 10),
    raphael:attr(Ws, R3, "fill", "#0000FF"),
    {ok,R4} = raphael:rect(Ws, Paper, 100, 30, 10, 10),
    raphael:attr(Ws, R4, "fill", "90-#fff-#000"),


    ok.

    
    
    
