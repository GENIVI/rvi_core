%%% @author tony <tony@rogvall.se>
%%% @copyright (C) 2013, tony
%%% @doc
%%%    Get network statistics
%%% @end
%%% Created : 18 Aug 2013 by tony <tony@rogvall.se>

-module(netlink_stat).

-compile(export_all).

-include("../include/netlink.hrl").

get_value() ->
    get_value("*").

get_value("") ->
    get_value("*");
get_value(Counter) ->
    case string:tokens(Counter, ".") of
	[]         -> get_all_counters();
	["*"]      -> get_all_counters();
	["*","*"]  -> get_all_counters();
	[Name,"*"] -> get_all_counters(Name);
	[Name]     -> get_all_counters(Name);
	["*",Var] ->
	    Field = list_to_atom(Var),
	    Fi = index(Field, record_info(fields, rtnl_link_stats)),
	    if Fi =:= 0 ->
		    [];
	       true ->
		    {ok,Ifs} = inet:getiflist(),
		    select_counters_(Ifs, Fi+1, Var, [])
	    end;
	[Name,Var] ->
	    Field = list_to_atom(Var),
	    {ok,S} = get_statistics(Name),
	    case index(Field, record_info(fields, rtnl_link_stats)) of
		0 -> [];
		Fi -> [{Counter,element(Fi+1, S)}]
	    end
    end.

get_all_counters() ->
    {ok,Ifs} = inet:getiflist(),
    get_all_counters_(Ifs,[]).

get_all_counters(Name) ->
    {ok,S} = get_statistics(Name),
    Acc = get_counters_(Name, 2, record_info(fields, rtnl_link_stats),S,[]),
    lists:reverse(Acc).

select_counters_([Name|As],Fi,Var,Acc) ->
    {ok,S} = get_statistics(Name),
    Value = element(Fi,S),
    select_counters_(As,Fi,Var,[{Name++"."++Var,Value}|Acc]);
select_counters_([], _Fi, _Var, Acc) ->
    lists:reverse(Acc).

get_all_counters_([Name|As],Acc) ->
    {ok,S} = get_statistics(Name),
    Acc1 = get_counters_(Name,2,record_info(fields, rtnl_link_stats),S,Acc),
    get_all_counters_(As,Acc1);
get_all_counters_([], Acc) ->
    lists:reverse(Acc).

get_counters_(Name, I, [F|Fs], S, Acc) ->
    Var = atom_to_list(F),
    get_counters_(Name, I+1, Fs, S, [{Name++"."++Var, element(I,S)} |Acc]);
get_counters_(_Name, _I, [], _S, Acc) ->
    Acc.

%%
%% Find first Key in Position Pos in the List 
%% return the position in the list or 0 if not found
index(Value, List) ->
    index_(1, Value, List).

index_(I,Value,[Value|_List]) ->  I;
index_(I,Value,[_|List]) -> index_(I+1,Value,List);
index_(_I,_Value,[]) -> 0.


get_statistics(Interface) ->
    netlink:start(),
    {ok,Ref} = netlink:subscribe(Interface),
    netlink:invalidate(Interface, [stats,stats64]),
    ok = netlink:get_match(link, inet, [{stats,native,[]}]),
    Res = get_stats64(Ref,1000),
    flush_stats(Ref),
    netlink:unsubscribe(Ref),
    case Res of
	{ok,Stats} ->
	    #rtnl_link_stats{} = R =list_to_tuple([rtnl_link_stats | Stats]),
	    {ok,R};
	Error ->
	    Error
    end.

get_stats64(Ref,Timeout) ->
    receive
	{netlink,Ref,_Interface,stats64,_Old,New} ->
	    {ok,New}
    after Timeout ->
	    {error,timeout}
    end.

flush_stats(Ref) ->
    receive
	_Msg={netlink,Ref,_Interface,_,_Old,_New} ->
	    %% io:format("flushed: ~p\n", [_Msg]),
	    flush_stats(Ref)
    after 0 ->
	    ok
    end.
