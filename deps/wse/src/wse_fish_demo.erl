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
%%%      Show how to do remote animation with png
%%%      Fishes from: http://www.squaregoldfish.co.uk
%%% @end
%%% Created :  24 Apr 2014 by Tony Rogvall <tony@rogvall.se>

-module(wse_fish_demo).
-export([run/2]).

-compile(export_all).

run(Ws, Where) ->
    io:format("fish_demo: called\n"),
    %% "preload" fish images
    Fishes =
	[ begin
	      FNo = tl(integer_to_list(10000+I)),
	      File = "fish/fish"++FNo++".png",
	      {ok,Image} = wse:load_image(Ws, File),
	      %% io:format("loaded: ~s\n", [File]),
	      Image
	  end ||  I <- lists:seq(1,23)],
    Image = wse:createElement(Ws, "img"),
    Parent = wse:id(Where),
    wse:appendChild(Ws, Parent, Image),
    update_loop(Ws, Image, Fishes, Fishes).

update_loop(Ws, Image, [F|Fs],  Fishes) ->
    {ok,Src} = wse:get(Ws, F, "src"),
    %% io:format("Src=~p\n", [Src]),
    wse:set(Ws, Image, "src", Src),
    timer:sleep(100),
    update_loop(Ws, Image, Fs, Fishes);
update_loop(Ws, Image, [],  Fishes) ->
    update_loop(Ws, Image, Fishes, Fishes).

