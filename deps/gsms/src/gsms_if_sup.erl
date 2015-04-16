%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2013, Rogvall Invest AB, <tony@rogvall.se>
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
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%  sms interface supervisor
%%%
%%% Created: 2013 by Malotte W Lönne 
%%% @end

-module(gsms_if_sup).

-behaviour(supervisor).

-include("log.hrl").
%% external exports
-export([start_link/0, 
	 start_link/1, 
	 stop/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Args) ->
    ?info("~p: start_link: args = ~p\n", [?MODULE, Args]),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, Args) of
	{ok, Pid} ->
	    {ok, Pid, {normal, Args}};
	Error -> 
	    Error
    end.

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

stop(_StartArgs) ->
    ok.

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init(Args) ->
    ?info("~p: init: args = ~p,\n pid = ~p\n", [?MODULE, Args, self()]),
    Interfaces = 
	lists:foldr(
	  fun({Mod,I,Opts},Acc) ->
		  Spec={{Mod,I},
			{Mod, start_link, [I,Opts]},
			permanent, 5000, worker, [Mod]},
		  [Spec | Acc]
	  end, [], 
	  case application:get_env(gsms, interfaces) of
	      undefined -> [];
	      {ok,IfList} when is_list(IfList) ->
		  IfList
	  end),
    ?info("~p: init: starting interfaces ~p", [?MODULE, Interfaces]),
    {ok,{{one_for_one,3,5}, Interfaces}}.
