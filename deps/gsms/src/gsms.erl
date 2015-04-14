%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
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
%%%---- END COPYRIGHT ----------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%
%%% Created : 18 Apr 2013 by Tony Rogvall
%%% @end

-module(gsms).
-include("gsms.hrl").

%% Interface
-export([send/2, 
	 subscribe/1,
	 unsubscribe/1]).

%% For testing
%% @private
-export([start/0, 
	 stop/0]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Send an SMS.
%% @end
%%--------------------------------------------------------------------
-spec send(Options::list({Key::atom(), Value::term()}), Body::string()) ->
		  {ok, Ref::reference()} | 
		  {error, Reason::term()}.
		  
send(Opts, Body) ->
    gsms_router:send(Opts, Body).

%%--------------------------------------------------------------------
%% @doc
%% Subscribe to incoming SMS:s filtered by Filter.
%% @end
%%--------------------------------------------------------------------
-spec subscribe(Filter::list(filter())) -> 
		       {ok,Ref::reference()} |
		       {error,Reason::term()}.

subscribe(Filter) ->
    case lists:keytake(reg_exp, 1, Filter) of
	{value, RegExp, Rest} when is_list(RegExp) ->
	    %% Convert to mp-format (and verify format)
	    case re:compile(RegExp, [unicode]) of
		{ok, MP} ->
		    gsms_router:subscribe([{reg_exp, MP} | Rest]);
		{error, _ErrSpec} = E->
		    E
	    end;
	{value, _RegExp, _Rest} ->
	    %% Assuming mp-format
	    gsms_router:subscribe(Filter);
	false ->
	    gsms_router:subscribe(Filter)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Remove previous subscription refered to by Ref.
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(Ref::reference()) -> ok.

unsubscribe(Ref) ->
    gsms_router:unsubscribe(Ref).

%%%===================================================================
start() ->
    call([lager,ale,uart,gsms], start).

stop() ->
    call([gsms,uart,ale,lager], stop).

call([App|Apps], F) ->
    error_logger:info_msg("~p: ~p\n", [F,App]),
    case application:F(App) of
	{error,{not_started,App1}} ->
	    call([App1,App|Apps], F);
	{error,{already_started,App}} ->
	    call(Apps, F);
	ok ->
	    call(Apps, F);
	Error ->
	    Error
    end;
call([],_F) ->
    ok.


