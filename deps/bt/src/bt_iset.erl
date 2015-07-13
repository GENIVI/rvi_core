%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2006 - 2014, Rogvall Invest AB, <tony@rogvall.se>
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
%%% File    : iset.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : Integer Set operations
%%% Created : 26 Jan 2006 by Tony Rogvall <tony@iMac.local>

-module(bt_iset).

%%
%% bt_iset:
%%
%% representation:
%%  List of interval and points
%%
%%  Points
%%       A sorted list of values and/or closed intervals where
%%       intervals are represented as pairs with as {Low,High}.
%%       It may also be the empty list if no specific points are
%%       given even though Max and Min has values.
%%
%%  operations:
%%       new() -> ISet
%%       from_list([Integer|{RangeStart,RangeStop}]) -> ISet
%%       to_list(ISet) -> List
%%
%%       add_element(Element, ISet) -> ISet U [Element]
%%       del_element(Element, ISet) -> ISet - [Element]
%%       intersection(ISet1, ISet2) -> (ISet1 ^ ISet2)
%%       subtract(ISet1, ISet2) -> (ISet1 - ISet2)
%%       union(ISet1, ISet2)  -> (ISet1 U ISet2)
%%       is_subset(ISet1, ISet2) ->  boolean()
%%       is_psubset(ISet1,ISet2) -> boolean()
%%       size(ISet) -> integer()
%%       is_element(Element, ISet) -> boolean()
%%       first(ISet) -> integer() | 'EXIT'
%%       last(ISet)  -> integer() | 'EXIT'
%%       format(ISet)
%%

-export([new/0]).
-export([add_element/2, del_element/2]).
-export([add_elements/2, del_elements/2]).
-export([from_list/1, to_list/1]).
-export([is_element/2, size/1, is_subset/2, is_psubset/2]).
-export([first/1, last/1]).
-export([format/1]).
-export([union/2, intersection/2, subtract/2]).
-export([sum/2, product/2, negate/1]).
-export([map/2, fold/3, filter/2]).

-import(lists, [reverse/1]).

%%
%% Create a new iset
%%
new()      ->  [].

%%
%% is_element(N, Domain)
%%
%% returns:
%%   true if N is a member of Domain
%%   false otherwise
%%
is_element(N, [N | _]) -> true;
is_element(N, [V | _]) when is_integer(V), N < V -> false;
is_element(N, [V | L]) when is_integer(V), N > V -> is_element(N,L);
is_element(N, [{Low,High}|_]) when N >= Low, N =< High -> true;
is_element(N, [{Low,_High} | _]) when N < Low -> false;
is_element(N, [{_Low,High} | L]) when N  > High -> is_element(N, L);
is_element(_N, []) -> false.

%%
%% Calculate size of domain
%%
%% returns:
%%       0 if empty set
%%       N number of members
%%  
size(L) ->
    size(L, 0).

size([V | L], N) when is_integer(V) -> size(L, N+1);
size([{Low, High}|L], N) -> size(L, N+(High-Low)+1);
size([], N) -> N.

%%
%% Get maximum and minimum value
%%
first([{V,_} | _]) -> V;
first([V | _]) -> V.

last(D) ->
    case lists:reverse(D) of
	[{_,V} | _] -> V;
	[V | _]     -> V
    end.

%%
%% Map over iset
%%
map(_F, []) ->
    [];
map(F, [D|Ds]) when is_integer(D) ->
    [F(D) | map(F,Ds)];
map(F, [{L,H}|Ds]) ->
    map_range(F, L, H, Ds).

map_range(F, I, N, Ds) when I > N ->
    map(F, Ds);
map_range(F, I, N, Ds) ->
    [F(I) | map_range(F,I+1,N,Ds)].

fold(F, Acc, [{A,B}|Es]) ->
    fold(F,fold_range(F, Acc, A, B),Es);
fold(F, Acc, [A|Es]) ->
    fold(F, F(A,Acc), Es);
fold(_F, Acc, []) ->
    Acc.

fold_range(_Fun, Acc, I, N) when I > N ->
    Acc;
fold_range(Fun, Acc, I, N) ->
    fold_range(Fun, Fun(I,Acc), I+1, N).


filter(F, ISet) ->
    filter(F, ISet, new()).

filter(F, [{A,B}|Es], ISet) ->
    filter(F,Es,filter_range(F,A,B,ISet));
filter(F, [A|Es], ISet) ->
    case F(A) of
	true ->
	    filter(F, Es, add_element(A,ISet));
	false ->
	    filter(F, Es, ISet)
    end;
filter(_F, [], ISet) ->
    ISet.

filter_range(_F, I, N, ISet) when I > N ->
    ISet;
filter_range(F,  I, N, ISet) ->
    case F(I) of
	true -> filter_range(F, I+1, N, add_element(I, ISet));
	false -> filter_range(F, I+1, N, ISet)
    end.

%%
%% Utilities:
%%
%% value(A,B)   make A-B a domain value
%% min(A,B)     return the minimum of A and B
%% max(A,B)     return the maximum of A and B
%% min_max(A,B) returns {Min,Max}
%%
-define(low(X), if is_tuple((X)) -> element(1,(X));
		   true -> (X)
		end).
-define(high(X), if is_tuple((X)) -> element(2,(X));
		    true -> (X)
		 end).
		       
value(N,N) -> N;
value(Low, High) -> {Low,High}.

min_max(A,B) when A < B -> {A,B};
min_max(A,B) -> {B,A}.

%%
%% Check if D1 is a subset D2
%%
is_subset(D1,D2) ->
    case intersection(D1, D2) of
	D1 -> true;
	_  -> false
    end.

%%
%% Check if D1 is a proper subset of D2
%%
is_psubset(D1, D1) -> false;
is_psubset(D1, D2) -> is_subset(D1,D2).


%%
%% from_list(List, ISet)
%%
%% description:
%%   Updates a domain given a list of integers and
%%   intervals and simplifies them as much as possible
%%
%% returns:
%%   Domain
%%
from_list(List) ->
    add_elements(List, new()).

to_list(ISet) ->
    ISet.

add_elements([E|List], ISet) ->
    add_elements(List, add_element(E,ISet));
add_elements([], ISet) ->
    ISet.

%%
%% add_element(Element, ISet) -> ISet'
%%
add_element(E={A,B},ISet) when is_integer(A),is_integer(B),A<B ->
    union([E], ISet);
add_element({A,A},ISet) when is_integer(A) ->
    union([A], ISet);
add_element(A,ISet) when is_integer(A) ->
    union([A], ISet).

%%
%% delete(Points, Domain)
%%
%% description:
%%   Remove values from a domain given a list of integers and
%%   intervals and simplifies them as much as possible
%%
%% returns:
%%   Domain
%%

del_elements([E|List], ISet) ->
    del_elements(List, del_element(E,ISet));
del_elements([], ISet) -> ISet.

del_element(E={A,B},ISet) when is_integer(A),is_integer(B),A<B ->
    subtract(ISet, [E]);
del_element({A,A},ISet) when is_integer(A) ->
    subtract(ISet, [A]);
del_element(A,ISet) when is_integer(A) ->
    subtract(ISet, [A]).

%%
%% union(Domain1, Domain2)
%%
%% description:
%%  Create a union of two domains
%% 
%%
union(D,D) -> D;
union(D1,D2) ->
    union(D1,D2,new()).

union(D, [], U) -> cons(U,D);
union([],D, U)  -> cons(U,D);
union(S1=[D1 | D1s], S2=[D2 | D2s], U) ->
    Min1 = ?low(D1),
    Max1 = ?high(D1),
    Min2 = ?low(D2),
    Max2 = ?high(D2),
    if Min2 == Max1+1 ->
	    union(D1s,[value(Min1,Max2) | D2s], U);
       Min1 == Max2+1 ->
	    union([value(Min2,Max1)|D1s], D2s, U);
       Min2 > Max1 ->
	    union(D1s, S2, [D1|U]);
       Min1 > Max2 ->
	    union(S1, D2s, [D2|U]);
       Max1 > Max2 ->
	    union([value(min(Min1,Min2),Max1)|D1s], D2s, U);
       true ->
	    union(D1s,[value(min(Min1,Min2),Max2)|D2s], U)
    end.

%%
%% intersection(ISet1, ISet2)
%%
%% description:
%%   Create the intersection between two domains
%%
%%

intersection([], _) -> [];
intersection(_, []) -> [];
intersection(D1, D2) ->
    intersection(D1, D2, new()).

intersection(_D, [], I) -> reverse(I);
intersection([], _D, I) -> reverse(I);
intersection(S1=[D1|D1s], S2=[D2|D2s], I) ->
    Min1 = ?low(D1),
    Max1 = ?high(D1),
    Min2 = ?low(D2),
    Max2 = ?high(D2),
    if Min2 > Max1 ->
 	    intersection(D1s, S2, I);
       Min1 > Max2 ->
	    intersection(S1, D2s, I);
       Max1<Max2 ->
	    intersection(D1s,S2,
			 [value(max(Min1,Min2),Max1)|I]);
       true ->
	    intersection(S1, D2s,
			 [value(max(Min1,Min2),Max2)|I])
    end.

%%
%% subtract(ISet1,ISet2)
%%
%% returns:
%%   The difference between Iset1 and ISet2, by removeing the
%%   values in ISet1 from ISet2 i.e D1 - D2.
%%   
%%

subtract(D, []) -> D;
subtract([], _) -> [];
subtract(D1, D2) ->
    subtract(D1,D2,new()).

subtract(D1s,[],Diff) -> cons(Diff,D1s);
subtract(D1s,[D2|D2s],Diff) ->
    Min2 = ?low(D2),
    Max2 = ?high(D2),
    ddiff(D1s, Min2, Max2, D2, D2s, Diff).

ddiff([], _Min2, _Max2, _D2, _D2s, Diff) ->
    reverse(Diff);
ddiff([D1 | D1s], Min2, Max2, D2, D2s, Diff) ->
    Min1 = ?low(D1),
    Max1 = ?high(D1),
    ddiffer(Min1, Max1, Min2, Max2, D1, D2, D1s,D2s,Diff).

ddiffer(Min1,Max1,Min2,Max2,D1,D2,D1s,D2s,Diff) ->
    if Min2 > Max1 ->
	    ddiff(D1s, Min2, Max2, D2, D2s, [D1 | Diff]);
       Min1 > Max2 ->
	    subtract([D1 | D1s], D2s, Diff);
       Min1<Min2 ->
	    D = value(Min1, Min2-1),
	    NewD1 = value(Min2, Max1),
	    ddiffer(Min2, Max1, Min2, Max2, 
		    NewD1, D2, D1s, D2s, [D|Diff]);
       Max1 > Max2 ->
	    NewD1 = value(Max2+1, Max1),
	    ddiffer(Max2+1, Max1, Min2, Max2, 
		    NewD1, D2, D1s, D2s, Diff);
       true ->
	    ddiff(D1s, Min2, Max2, D2, D2s, Diff)
    end.

%%
%% Arithmetic in universe
%%
%% negate(A)      Integer negate values in domain A
%% sum(A,B)       Integer addition of values in domains A and B
%% multiply(A,B)  Integer multiplication of values in domains A and B
%% divide(A,B)    Integer division of values in domain A with values in B
%%

%%
%% Negate domain
%% returns:
%%  { x | x = -a, a in A}
%%
negate(A) ->
    negate(A, new()).

negate([{I,J} | As], Neg) ->
    negate(As, [value(-J,-I)|Neg]);    
negate([I | As], Neg) ->
    negate(As, [-I|Neg]);
negate([], Neg) ->
    Neg.

%%
%% Sum of domains
%% returns:
%%   { x | x = a+b, a in A, b in B }
%%
sum(A, B) ->
    sum(A,B,new()).

sum([A|As], Bs, Sum) ->
    MinA = ?low(A),
    MaxA = ?high(A),
    sum(As,Bs,union(sum1(Bs,MinA,MaxA,new()), Sum));
sum([], _, Sum) ->
    Sum.

sum1([B|Bs],MinA,MaxA,Sum) ->
    MinB = ?low(B),
    MaxB = ?high(B),
    sum1(Bs, MinA, MaxA, union([value(MinA+MinB,MaxA+MaxB)], Sum));
sum1([],_,_,Sum) ->
    Sum.

%%
%% Product of domains
%% returns:
%%   { x | x = a*b, a in A, b in B }
%%
product(A, B) ->
    product(A,B,new()).

product([A|As], Bs, Prod) ->
    product(As,Bs,union(prod(Bs,A,new()), Prod));
product([], _, Prod) ->
    Prod.

prod([B|Bs],A,Prod) ->
    MinA = ?low(A),
    MaxA = ?high(A),
    MinB = ?low(B),
    MaxB = ?high(B),
    P1 = MinA*MinB,
    P2 = MinA*MaxB,
    P3 = MaxA*MinB,
    P4 = MaxA*MaxB,
    {Min1,Max1} = min_max(P1,P2),
    {Min2,Max2} = min_max(P3,P4),
    N = add_element({min(Min1,Min2),max(Max1,Max2)}, new()),
    prod(Bs, A, union(N, Prod));
prod([],_, Prod) ->
    Prod.


cons([E | L1], L2) ->
    cons(L1, [E | L2]);
cons([], L) ->
    L.

%%
%% format(D)
%%
%% output a domain
%%
format([]) -> "{}";
format([D]) -> ["{", format1(D), "}"];
format([D|Ds]) -> 
    ["{", format1(D), lists:map(fun(D1) -> [",",format1(D1)] end, Ds), "}"].

format1({Low,High}) -> [$[,integer_to_list(Low),"..",integer_to_list(High),$]];
format1(Value)      -> [integer_to_list(Value)].

