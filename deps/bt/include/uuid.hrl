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
%%
%% Some UUID macros
%%
-ifndef(UUID_HRL).
-define(UUID_HRL, true).

-define(is_uuid(UUID), is_binary(UUID), size(UUID) == 16).

-define(is_ieee_802(A),
	is_tuple((A)), size((A)) == 6,
	((element(1,(A)) bor element(2,(A)) bor  element(3,(A)) bor
	  element(4,(A)) bor element(5,(A)) bor  element(6,(A))) band
	 -16#100 == 0)).

%% UUID all components
-define(UUIDx(Ver,TimeLow,TimeMid,TimeHi,Var,Clock,Node),
	<<(TimeLow):32,(TimeMid):16,(Ver):4,(TimeHi):12,
	 (Var):2,(Clock):14,(Node):48>>).

%% UUID major components - for formating etc
-define(UUID(TimeLow,TimeMid,TimeHigh,Clock,Node),
	<<TimeLow:32,TimeMid:16,TimeHigh:16, Clock:16, Node:48>>).

-define(NameSpace_DNS,
	?UUID(16#6ba7b810, 16#9dad, 16#11d1, 16#80b4, 16#00c04fd430c8)).

-define(NameSpace_URL,
	?UUID(16#6ba7b811, 16#9dad, 16#11d1, 16#80b4, 16#00c04fd430c8)).

-define(NameSpace_OID,
	?UUID(16#6ba7b812, 16#9dad, 16#11d1, 16#80b4, 16#00c04fd430c8)).

-define(NameSpace_X500,
	?UUID(16#6ba7b814, 16#9dad, 16#11d1, 16#80b4, 16#00c04fd430c8)).

-endif.
