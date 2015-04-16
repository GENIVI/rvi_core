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

-module(wse_table_demo).
-export([run/2]).

-compile(export_all).

table_data() ->
    [ ["1111", on, 1],
      ["1112", off, 2],
      ["1113", off, 3],
      ["1114", off, 4],
      ["1115", off, 5]
    ].

run(Ws, Where) ->
    io:format("table_demo: called\n"),

    Parent = wse:id(Where),
    {Table,TableElems} = make_table(Ws, table_data()),
    wse:set(Ws, Table, "border", "2"),
    wse:appendChild(Ws, Parent, Table),
    update_loop(Ws, TableElems, 1).

update_loop(Ws, Rs, I) ->
    update_rows(Ws, Rs, 0, I),
    timer:sleep(1000),
    update_loop(Ws, Rs, I+1).

update_rows(Ws, [{Tr,Tds}|Rs], Ri, I) ->
    {ok,Style} = wse:get(Ws, Tr, "style"),
    case Ri rem 2 of
	0 -> wse:set(Ws, Style, "backgroundColor", "rgb(0,255,0)");
	1 -> wse:set(Ws, Style, "backgroundColor", "rgb(255,0,0)")
    end,
    I1 = update_row(Ws, Tds, I),
    update_rows(Ws, Rs, Ri+1, I1);
update_rows(_Ws, [], _Ri, _I) ->
    ok.

update_row(Ws, [{_Td,Txt}|Tds], I) ->
    wse:set(Ws, Txt, "nodeValue", integer_to_list(I)),
    update_row(Ws, Tds, I+1);
update_row(_Ws, [], I) ->
    I.


%%
%% return:
%%    {Table,[{Tr1,[Td11,Td12,..Td1m1]},
%%            {Tr2,[Td21,Td22,..Td2m1]}
%%            {Trn,[Tdn1,Tn22,..Tdnmn]}]}
%%
make_table(Ws, TableData) ->
    %% create a table from table data
    Table = wse:createElement(Ws, "table"),
    TBody = wse:createElement(Ws, "tbody"),
    TableElems =
	[ begin
	      Tr = wse:createElement(Ws, "tr"),
	      R = {Tr, [begin Td = wse:createElement(Ws, "td"),
			      Text = lists:flatten(io_lib:format("~p", [Col])),
			      TextNode = wse:createTextNode(Ws, Text),
			      wse:appendChild(Ws, Td, TextNode),
			      wse:appendChild(Ws, Tr, Td),
			      {Td,TextNode}
			end || Col <-  Row ]},
	      wse:appendChild(Ws, TBody, Tr),
	      R
	  end || Row <- TableData ],
    wse:appendChild(Ws, Table, TBody),
    {Table, TableElems}.
    

info(Ws, Object, ObjName, What, Parent) ->
    {ok,Value} = wse:get(Ws, Object, What),
    Text = ObjName++"."++to_text(What)++"="++to_text(Value),
    ElemNode = wse:createElement(Ws, "p"),
    TextNode = wse:createTextNode(Ws, Text),
    wse:appendChild(Ws, ElemNode, TextNode),
    wse:appendChild(Ws, Parent, ElemNode).

to_text(X) ->
    lists:flatten(io_lib:format("~p", [X])).

    
    
    
