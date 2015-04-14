%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    URL definition
%%% @end
%%% Created : 16 Dec 2011 by Tony Rogvall <tony@rogvall.se>

-ifndef(_EXO_URL_HRL_).
-define(_EXO_URL_HRL_, true).

-record(url,
	{
	  scheme,
	  host, 
	  port,            %% undefined means not set
	  path = "",
	  querypart = ""
	 }). 

-endif.

