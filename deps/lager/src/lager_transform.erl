%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc The parse transform used for lager messages.
%% This parse transform rewrites functions calls to lager:Severity/1,2 into
%% a more complicated function that captures module, function, line, pid and
%% time as well. The entire function call is then wrapped in a case that
%% checks the mochiglobal 'loglevel' value, so the code isn't executed if
%% nothing wishes to consume the message.

-module(lager_transform).

-include("lager.hrl").

-export([parse_transform/2]).
-export([format_error/1]).

%% @private
parse_transform(AST, _Options) ->
    try walk_ast([], AST) of
	Forms -> Forms
    catch
	throw:E ->
	    E
    end.


walk_ast(Acc, []) ->
    lists:reverse(Acc);
walk_ast(Acc, [{attribute, _, module, {Module, _PmodArgs}}=H|T]) ->
    %% A wild parameterized module appears!
    put(module, Module),
    walk_ast([H|Acc], T);
walk_ast(Acc, [{attribute, _, module, Module}=H|T]) ->
    put(module, Module),
    walk_ast([H|Acc], T);
walk_ast(Acc, [{function, Line, Name, Arity, Clauses}|T]) ->
    put(function, Name),
    walk_ast([{function, Line, Name, Arity,
                walk_clauses([], Clauses)}|Acc], T);
walk_ast(Acc, [H|T]) ->
    walk_ast([H|Acc], T).

walk_clauses(Acc, []) ->
    lists:reverse(Acc);
walk_clauses(Acc, [{clause, Line, Arguments, Guards, Body}|T]) ->
    walk_clauses([{clause, Line, Arguments, Guards, walk_body([], Body)}|Acc], T).

walk_body(Acc, []) ->
    lists:reverse(Acc);
walk_body(Acc, [H|T]) ->
    walk_body([transform_statement(H)|Acc], T).
%%
%% lager:debug([{a,1},{b,2}], "Hello ~w", [world])
%%
%% generate:
%%      case lager_mochiglobal:get(loglevel, {- 1,[]}) of
%%          {_lager_transform_level3,[]}
%%              when _lager_transform_level3 < 7 ->
%%              ok;
%%          {_,_lager_transform_match4} ->
%%              lager:dispatch_log1(_lager_transform_match4,
%%                                  debug,
%%                                  ?MODULE,
%%                                  ?FUNCTION,
%%                                  ?LINE,
%%                                  self(),
%%                                  fun({a,1}) -> true;
%%                                     ({a,'*'}) -> true;
%%                                     ({b,2}) -> true;
%%                                     ({b,'*'}) -> true;
%%                                     ({module,?MODULE}) -> true;
%%                                     ({module,'*'}) -> true;
%%                                     ({function,?FUNCTION}) -> true;
%%                                     ({function,'*'}) -> true;
%%                                     ({line,?LINE}) -> true;
%%                                     ({line,'*'}) -> true;
%%                                     ({pid,'*'}) -> true;
%%                                     ({pid,PID}) ->
%%                                        PID =:= pid_to_list(self());
%%                                     (_) -> false
%%                                  end,
%%                                  "Hello ~w",
%%                                 [world])
%%      end
%%
transform_statement({call, L, {remote, L1, {atom, L2, lager},
			       {atom, L3, Severity}}, Arguments0} = Stmt) ->
    case lists:member(Severity, ?LEVELS) of
        true ->
	    Trace0 = [trace_clause({atom,L,module},{atom,L,'*'}),
		      trace_clause({atom,L,module},{atom,L,get(module)}),
		      trace_clause({atom,L,function},{atom,L,'*'}),
		      trace_clause({atom,L,function},{atom,L,get(function)}),
		      trace_clause({atom,L,line},{atom,L,'*'}),
		      trace_clause({atom,L,line},{integer,L,L}),
		      trace_clause({atom,L,pid}, {atom,L,'*'}),
		      trace_clause({atom,L,pid}, {atom,L,pid})
		     ],
            {Trace1, Message, Arguments} = 
		case Arguments0 of
		    [Format] ->
			{Trace0, Format, {nil, L}};
		    [Arg1, Arg2] ->
		    %% some ambiguity here, figure out if these arguments are
                    %% [Format, Args] or [Attr, Format].
                    %% The trace attributes will be a list of tuples, so check
                    %% for that.
			case Arg1 of
			    {cons, _, {tuple, _, _}, _} ->
				{concat_clauses(L,Arg1,Trace0),
				 Arg2, {nil,L}};
			    _ ->
				{Trace0, Arg1, Arg2}
			end;
		    [Attrs, Format, Args] ->
			{concat_clauses(L,Attrs,Trace0), Format, Args}
		end,
	    Trace2 =  lists:ukeysort(1, Trace1),
	    Clauses = generate_clauses(L,Trace2) ++
		[{clause,L,[{var,L,'_'}],[],
		  [{atom,L,false}]}],
	    Traces = {'fun',L,{clauses,Clauses}},
	    SeverityNum =
		case Severity of
		    debug -> ?DEBUG;
		    info -> ?INFO;
		    notice -> ?NOTICE;
		    warning -> ?WARNING;
		    error -> ?ERROR;
		    critical -> ?CRITICAL;
		    alert -> ?ALERT;
		    emergency -> ?EMERGENCY;
		    none -> ?LOG_NONE
		end,
	    LevelThreshold = new_var("level",L),
	    Match = new_var("match",L),
            {block, L,
	     [
	      {'case',L,
	       {call,L,
		{remote,L,{atom,L,lager_mochiglobal},{atom,L,get}},
		[{atom,L,loglevel},
		 {tuple,L,[{op,L,'-',{integer,L,1}},{nil,L}]}]},
		  [{clause,L,
		    [{tuple,L,[LevelThreshold,{nil,L}]}],
		    [[{op,L,'<',LevelThreshold,{integer,L,SeverityNum}}]],
		    [{atom,L,ok}]},
		   {clause,L,
		    [{tuple,L,[{var,L,'_'},Match]}],
		    [],
		    [{call,L,
		      {remote,L1,{atom,L,lager},
		       {atom,L2,dispatch_log1}},
		      [Match,
		       {atom,L3,Severity},
		       {atom,L3,get(module)},
		       {atom,L3,get(function)},
		       {integer,L3,L},
		       {call, L3, {atom, L3 ,self}, []},
		       Traces,
		       Message,
		       Arguments
		       ]}]}]}
	     ]};
	false ->
	    Stmt
    end;
transform_statement({call, L, {remote, L1, {atom, L2, boston_lager},
            {atom, L3, Severity}}, Arguments}) ->
    NewArgs =
	case Arguments of
	    [{string, L, Msg}] ->
		[{string, L, re:replace(Msg, "r", "h", 
					[{return, list}, global])}];
	    [{string, L, Format}, Args] ->
		[{string, L, re:replace(Format, "r", "h",
					[{return, list}, global])}, Args];
	    Other -> Other
        end,
    transform_statement({call, L, {remote, L1, {atom, L2, lager},
				   {atom, L3, Severity}}, NewArgs});
transform_statement(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt)));
transform_statement(Stmt) when is_list(Stmt) ->
    [transform_statement(S) || S <- Stmt];
transform_statement(Stmt) ->
    Stmt.

%%
%% Generate from key value pairs
%% remove duplicates!
%%

generate_clauses(L,[{{pid,pid},{_K,_V}}|Cs]) ->
    make_pid_clause(L) ++ generate_clauses(L, Cs);
generate_clauses(L,[{{_,_},{K,V}}|Cs]) ->
    make_clause(L,K,V) ++ generate_clauses(L, Cs);
generate_clauses(_L, []) ->
    [].

%%
%% create trace clause
%%   ({key, Value}) -> true;
%%     
make_clause(L,K,V) ->
    [{clause,L,[{tuple,L,[K,V]}],[],[{atom,L,true}]}].

%%
%% create special pid clauses
%%  ({pid, PID}) -> PID =:= pid_to_list(self())
%%
make_pid_clause(L) ->
    PID = new_var("pid", L),
    K = {atom,L,pid},
    [{clause,L,[{tuple,L,[K,PID]}],[],
      [{op,L,'=:=', PID, {call,L,{atom,L,pid_to_list},
			  [{call,L,{atom,L,self},[]}]}}]}].

%%
%% Concat clause from input
%%	    
concat_clauses(_L,{nil, _Line}, B) ->
    B;
concat_clauses(L,{cons, _Line, {tuple,_L,[K,V]}, Tail}, B) ->
    [ trace_clause(K, {atom,L,'*'}),
      trace_clause(K,V) | concat_clauses(L,Tail, B)];
concat_clauses(L, _, _) ->
    Err = {"*current*",[{L,?MODULE,"traces must be key value pairs"}]},
    throw({error,[Err],[]}).

format_error(Str) ->
    io_lib:format("~s", [Str]).

trace_clause(K,V) ->
    Kn = erl_parse:normalise(K),
    Vn = erl_parse:normalise(V), 
    {{Kn,Vn},{K,V}}.


new_var(Base,L) ->
    N = case get(lager_transform_var) of
	    undefined -> 
		1;
	    N0 ->
		N0
	end,
    put(lager_transform_var, N+1),
    Name = list_to_atom("_lager_transform_"++Base++integer_to_list(N)),
    {var, L, Name}.

