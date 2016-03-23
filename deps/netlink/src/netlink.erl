%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Netlink state monitor
%%% @end
%%% Created : 11 Jun 2012 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(netlink).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([i/0, list/1]).
-export([subscribe/1, subscribe/2, subscribe/3]).
-export([unsubscribe/1]).
-export([invalidate/2]).
-export([get_root/2, get_match/3, get/4]).

-include("log.hrl").
-include("netlink.hrl").
-include("netl_codec.hrl").

-define(SERVER, ?MODULE). 

-type if_addr_field() :: address | local | broadcast | anycast | multicast.

-type if_link_field() :: name | index | mtu | txqlen | flags | 
			 operstate | qdisc | address | broadcast.

-type uint8_t() :: 0..16#ff.
-type uint16_t() :: 0..16#ffff.

-type ipv4_addr() :: {uint8_t(),uint8_t(),uint8_t(),uint8_t()}.
-type ipv6_addr() :: {uint16_t(),uint16_t(),uint16_t(),uint16_t(),
		      uint16_t(),uint16_t(),uint16_t(),uint16_t()}.

-type if_addr() :: ipv4_addr() | ipv6_addr().

-type if_field() :: if_link_field() | if_addr_field() |
		    {link,if_link_field()} | {addr,if_addr_field()}.

-type if_name() :: string().


-record(link,
	{
	  name     :: if_name(),          %% interface name
	  index    :: non_neg_integer(),  %% interface index
	  attr     :: term()              %% attributes {atom(),term}
	}).

-record(addr,
	{
	  addr     :: if_addr(),          %% the address
	  name     :: if_name(),          %% interface label
	  index    :: non_neg_integer(),  %% interface index
	  attr     :: term()              %% attributes
	}).

-record(subscription,
	{
	  pid  :: pid(),               %% subscriber
	  mon  :: reference(),         %% monitor
	  name :: string(),            %% name
	  fields=all :: all | addr | link | [if_field()]
	}).

-define(MIN_RCVBUF,  (128*1024)).
-define(MIN_SNDBUF,  (32*1024)).

-define(REQUEST_TMO, 2000).

-record(request,
	{
	  tmr,      %% timer reference
	  call,     %% call request
	  from,     %% caller
	  reply=ok, %% reply to send when done
	  seq=0     %% sequence to expect in reply
	}).

-record(state, 
	{
	  port,
	  link_list  = [] :: [#link {}],
	  addr_list  = [] :: [#addr {}],
	  sub_list   = [] :: [#subscription {}],
	  request         :: undefined | #request{},
	  request_queue = [] :: [#request{}],
	  o_seq = 0,
	  i_seq = 0,
	  ospid
        }).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(netlink).

i() ->
    gen_server:call(?SERVER, {list,[]}).

stop() ->
    gen_server:call(?SERVER, stop).

list(Match) ->
    gen_server:call(?SERVER, {list,Match}).

%% @doc
%%   Subscribe to interface changes, notifications will be
%%   sent in {netlink,reference(),if_name(),if_field(),OldValue,NewValue}
%% @end

-spec subscribe(Name::string()) ->
		       {ok,reference()}.

subscribe(Name) ->
    subscribe(Name,all,[]).

-spec subscribe(Name::string(),Fields::all|[if_field()]) ->
		       {ok,reference()}.

subscribe(Name,Fields) ->
    subscribe(Name,Fields, []).

-spec subscribe(Name::string(),Fields::all|[if_field()],Otions::[atom()]) ->
		       {ok,reference()}.
subscribe(Name,Fields,Options) ->
    gen_server:call(?SERVER, {subscribe, self(),Name,Options,Fields}).

unsubscribe(Ref) ->
    gen_server:call(?SERVER, {unsubscribe,Ref}).

%% clear all attributes for interface Name
invalidate(Name,Fields) ->
    gen_server:call(?SERVER, {invalidate,Name,Fields}).

get_root(What,Fam) ->
    get(What,Fam,[root,match,request],[]).

get_match(What,Fam,GetAttrs) ->
    get(What,Fam,[match,request],GetAttrs).

get(What,Fam,GetFlags,GetAttrs) ->
    gen_server:call(?SERVER, {get,What,Fam,GetFlags,GetAttrs}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Opts]) ->
    OsPid = list_to_integer(os:getpid()),
    I_Seq = O_Seq = 1234, %% element(2,now()),
    State = #state{ ospid = OsPid, 
		    o_seq = O_Seq, 
		    i_seq = I_Seq },

    case os:type() of
	{unix, linux} ->
	    init_drv(Opts, State);
	_ ->
	    {ok, State}
    end.

init_drv(Opts, State) ->
    Port = netlink_drv:open(?NETLINK_ROUTE),

    netlink_drv:debug(Port, proplists:get_value(debug,Opts,none)),

    {ok,Rcvbuf} = update_rcvbuf(Port, ?MIN_RCVBUF),
    {ok,Sndbuf} = update_sndbuf(Port, ?MIN_SNDBUF),

    ?info("Rcvbuf: ~w, Sndbuf: ~w", [Rcvbuf, Sndbuf]),

    {ok,Sizes} = netlink_drv:get_sizeof(Port),
    ?info("Sizes: ~w", [Sizes]),

    ok = netlink_drv:add_membership(Port, ?RTNLGRP_LINK),
    ok = netlink_drv:add_membership(Port, ?RTNLGRP_IPV4_IFADDR),
    ok = netlink_drv:add_membership(Port, ?RTNLGRP_IPV6_IFADDR),

    netlink_drv:activate(Port),
    %% init sequence to fill the cache
    T0 = erlang:start_timer(200, self(), request_timeout),
    R0 = #request { tmr  = T0, 
		    call = noop, 
		    from = {self(),make_ref()} 
		  },
    R1 = #request { tmr  = {relative, ?REQUEST_TMO},
		    call = {get,link,unspec,
			    [root,match,request],
			    []},
		    from = {self(),make_ref()}
		  },
    R2 = #request { tmr  = {relative, 1000}, 
		    call = noop,
		    from = {self(),make_ref()}
		  },
    R3 = #request { tmr = {relative,?REQUEST_TMO},
		    call = {get,addr,unspec,
			    [root,match,request],
			    []},
		    from = {self(),make_ref()}
		  },
    {ok, State#state{ port=Port,
		      request = R0,
		      request_queue = [R1,R2,R3]
		    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({list,Match}, _From, State) ->
    lists:foreach(
      fun(L) ->
	      %% select addresses that belong to link L
	      Ys = [Y || Y <- State#state.addr_list, 
			 Y#addr.index =:= L#link.index],
	      FYs = [format_addr(Y) || Y <- Ys ],
	      case match(L#link.attr,dict:new(),Match) of
		  true ->
		      io:format("link {~s~s}\n",
				[FYs,format_link(L)]);
		  false ->
		      ok
	      end
      end, State#state.link_list),
    {reply, ok, State};
handle_call({subscribe, Pid, Name, Options, Fs}, _From, State) ->
    Mon = erlang:monitor(process, Pid),
    S = #subscription { pid=Pid, mon=Mon, name=Name, fields=Fs },
    SList = [S | State#state.sub_list],
    case proplists:get_bool(flush, Options) of
	false ->
	    {reply, {ok,Mon}, State#state { sub_list = SList }};
	true ->
	    lists:foreach(
	      fun(L) ->
		      As = dict:to_list(L#link.attr),
		      update_attrs(L#link.name, link, As, dict:new(), [S])
	      end, State#state.link_list),
	    lists:foreach(
	      fun(Y) ->
		      As = dict:to_list(Y#addr.attr),
		      update_attrs(Y#addr.name, addr, As, dict:new(), [S])
	      end, State#state.addr_list),
	    {reply, {ok,Mon}, State#state { sub_list = SList }}
    end;
handle_call({unsubscribe,Ref}, _From, State) ->
    case lists:keytake(Ref, #subscription.mon, State#state.sub_list) of
	false ->
	    {reply,ok,State};
	{value,_S,SubList} ->
	    erlang:demonitor(Ref),
	    {reply,ok,State#state { sub_list=SubList }}
    end;
handle_call({invalidate,Name,Fields},_From,State) ->
    case lists:keytake(Name, #link.name, State#state.link_list) of 
	false -> {reply, {error,enoent}, State};
	{value,L,Ls} ->
	    Attr = lists:foldl(
		     fun(F,D) when is_atom(F) ->
			     dict:erase(F, D)
		     end, L#link.attr, Fields),
	    L1 = L#link { attr = Attr },
	    {reply, ok, State#state { link_list = [L1|Ls] } }
    end;

handle_call(Req={get,_What,_Fam,_Flags,_Attrs}, From, State) ->
    ?debug("handle_call: GET: ~p", [Req]),
    State1 = enq_request(Req, From, State),
    State2 = dispatch_command(State1),
    {noreply, State2};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(_Info={nl_data,Port,Data},State) when Port =:= State#state.port ->
    try netlink_codec:decode(Data,[]) of
	MsgList ->
	    %% FIXME: the messages should be delivered one by one from
	    %% the driver so the decoding could simplified.
	    State1 = 
		lists:foldl(
		  fun(Msg,StateI) ->
			  ?debug("handle_info: msg=~p", [Msg]),
			  _Hdr = Msg#nlmsg.hdr,
			  MsgData = Msg#nlmsg.data,
			  handle_nlmsg(MsgData, StateI)
		  end, State, MsgList),
	    {noreply, State1}
    catch
	error:_ ->
	    ?error("netlink: handle_info: Crash: ~p", 
		   [erlang:get_stacktrace()]),
	    {noreply, State}
    end;

handle_info({'DOWN',Ref,process,Pid,Reason}, State) ->
    case lists:keytake(Ref, #subscription.mon, State#state.sub_list) of
	false ->
	    {noreply,State};
	{value,_S,SubList} ->
	    ?debug("subscription from pid ~p deleted reason=~p",
		   [Pid, Reason]),
	    {noreply,State#state { sub_list=SubList }}
    end;
handle_info({timeout,Tmr,request_timeout}, State) ->
    R = State#state.request,
    if R#request.tmr =:= Tmr ->
	    ?debug("Timeout: ref current", []),
	    gen_server:reply(R#request.from, {error,timeout}),
	    State1 = State#state { request = undefined },
	    {noreply, dispatch_command(State1)};
       true ->
	    case lists:keytake(Tmr, #request.tmr, State#state.request_queue) of
		false ->
		    ?debug("Timeout: ref not found", []),
		    {noreply, State};
		{value,#request { from = From},Q} ->
		    ?debug("Timeout: ref in queue", []),
		    gen_server:reply(From, {error,timeout}),
		    State1 = State#state { request_queue = Q },
		    {noreply,dispatch_command(State1)}
	    end
    end;
handle_info({Tag, Reply}, State) when is_reference(Tag) ->
    ?debug("INFO: SELF Reply=~p", [Reply]),
    {noreply, State};    
handle_info(_Info, State) ->
    ?debug("INFO: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

enq_request(Call, From, State) ->
    Tmr = erlang:start_timer(?REQUEST_TMO, self(), request_timeout),
    R = #request { tmr  = Tmr, 
		   call = Call,
		   from = From
		 },
    Q = State#state.request_queue ++ [R],
    State#state { request_queue = Q }.
    
dispatch_command(State) when State#state.request =:= undefined ->
    case State#state.request_queue of
	[R=#request { call = {get,What,Fam,Flags,Attrs} } | Q ] ->
	    R1 = update_timer(R),
	    State1 = State#state { request_queue = Q, request = R1 },
	    ?debug("dispatch_command: ~p", [R1]),
	    get_command(What,Fam,Flags,Attrs,State1);
	[R=#request { call = noop } | Q ] ->
	    R1 = update_timer(R),
	    State1 = State#state { request_queue = Q, request = R1 },
	    ?debug("dispatch_command: ~p", [R1]),
	    State1; %% let it timeout
	[] ->
	    State
    end;
dispatch_command(State) ->
    State.

update_timer(R = #request { tmr = {relative,Tmo} }) 
  when is_integer(Tmo), Tmo >= 0 ->
    Tmr = erlang:start_timer(Tmo, self(), request_timeout),
    R#request { tmr = Tmr };
update_timer(R = #request { tmr = Tmr }) when is_reference(Tmr) ->
    R.

update_sndbuf(Port, Min) ->
    case netlink_drv:get_sndbuf(Port) of
	{ok,Size} when Size >= Min ->
	    {ok,Size};
	{ok,_Size} ->
	    netlink_drv:set_sndbuf(Port, Min),
	    netlink_drv:get_sndbuf(Port);
	Err -> Err
    end.


update_rcvbuf(Port, Min) ->
    case netlink_drv:get_rcvbuf(Port) of
	{ok,Size} when Size >= Min ->
	    {ok,Size};
	{ok,_Size} ->
	    netlink_drv:set_rcvbuf(Port, Min),
	    netlink_drv:get_rcvbuf(Port);
	Err -> Err
    end.

get_command(link,Fam,Flags,Attrs,State) ->
    Seq = State#state.o_seq,
    Get = #getlink{family=Fam,arphrd=ether,index=0,
		   flags=[], change=[], attributes=Attrs},
    Hdr = #nlmsghdr { type  = getlink,
		      flags = Flags,
		      seq   = Seq,
		      pid   = State#state.ospid },
    Request = netlink_codec:encode(Hdr,Get),
    netlink_drv:send(State#state.port, Request),
    State#state { o_seq = (Seq+1) band 16#ffffffff };
get_command(addr,Fam,Flags,Attrs,State) ->
    Seq = State#state.o_seq,
    Get   = #getaddr{family=Fam,prefixlen=0,flags=[],scope=0,
		     index=0,attributes=Attrs},
    Hdr = #nlmsghdr { type=getaddr,
		      flags=Flags,
		      seq=Seq, 
		      pid=State#state.ospid },
    Request = netlink_codec:encode(Hdr,Get),
    netlink_drv:send(State#state.port, Request),
    State#state { o_seq = (Seq+1) band 16#ffffffff}.

handle_nlmsg(RTM=#newlink{family=_Fam,index=Index,flags=Fs,change=Cs,
			  attributes=As}, State) ->
    ?debug("RTM = ~p", [RTM]),
    Name = proplists:get_value(ifname, As, ""),
    As1 = [{index,Index},{flags,Fs},{change,Cs}|As],
    case lists:keytake(Index, #link.index, State#state.link_list) of
	false ->
	    Attr = update_attrs(Name, link, As1, dict:new(), State#state.sub_list),
	    L = #link { index = Index, name = Name, attr = Attr },
	    Ls = [L|State#state.link_list],
	    State#state { link_list = Ls };
	{value,L,Ls} ->
	    Attr = update_attrs(Name, link, As1, L#link.attr, State#state.sub_list),
	    L1 = L#link { name = Name, attr = Attr },
	    State#state { link_list = [L1|Ls] }
    end;
handle_nlmsg(RTM=#dellink{family=_Fam,index=Index,flags=_Fs,change=_Cs,
			  attributes=As}, State) ->
    ?debug("RTM = ~p\n", [RTM]),
    Name = proplists:get_value(ifname, As, ""),
    %% does this delete the link?
    case lists:keytake(Index, #link.index, State#state.link_list) of
	false ->
	    ?warning("Warning link index=~w not found", [Index]),
	    State;
	{value,L,Ls} ->
	    As1 = dict:to_list(L#link.attr),
	    update_attrs(Name, link, As1, undefined, State#state.sub_list),
	    State#state { link_list = Ls }
    end;
handle_nlmsg(RTM=#newaddr { family=Fam, prefixlen=Prefixlen,
			    flags=Flags, scope=Scope,
			    index=Index, attributes=As },
	     State) ->
    ?debug("RTM = ~p", [RTM]),
    Addr = proplists:get_value(address, As, {}),
    Name = proplists:get_value(label, As, ""),
    As1 = [{family,Fam},{prefixlen,Prefixlen},{flags,Flags},
	   {scope,Scope},{index,Index} | As],
    case lists:keymember(Index, #link.index, State#state.link_list) of
	false ->
	    ?warning("link index ~p does not exist", [Index]);
	true ->
	    ok
    end,
    case lists:keytake(Addr, #addr.addr, State#state.addr_list) of
	false ->
	    Attrs = update_attrs(Name,addr,As1,dict:new(),State#state.sub_list),
	    Y = #addr { addr=Addr, name = Name, index=Index, attr=Attrs },
	    Ys = [Y|State#state.addr_list],
	    State#state { addr_list = Ys };
	{value,Y,Ys} ->
	    Attr = update_attrs(Name,addr,As1,Y#addr.attr,State#state.sub_list),
	    Y1 = Y#addr { index=Index, name=Name, attr = Attr },
	    State#state { addr_list = [Y1|Ys] }
    end;

handle_nlmsg(RTM=#deladdr { family=_Fam, index=_Index, attributes=As },
	     State) ->
    ?debug("RTM = ~p", [RTM]),
    Addr = proplists:get_value(address, As, {}),
    Name = proplists:get_value(label, As, ""),
    case lists:keytake(Addr, #addr.addr, State#state.addr_list) of
	false ->
	    ?warning("Warning addr=~s not found", [Addr]),
	    State;
	{value,Y,Ys} ->
	    As1 = dict:to_list(Y#addr.attr),
	    update_attrs(Name, addr, As1, undefined, State#state.sub_list),
	    State#state { addr_list = Ys }
    end;
handle_nlmsg(#done { }, State) ->
    case State#state.request of
	undefined ->
	    dispatch_command(State);
	#request { tmr = Tmr, from = From, reply = Reply } ->
	    ?debug("handle_nlmsg: DONE: ~p", 
		   [State#state.request]),
	    erlang:cancel_timer(Tmr),
	    gen_server:reply(From, Reply),
	    State1 = State#state { request = undefined },
	    dispatch_command(State1)
    end;
handle_nlmsg(Err=#error { errno=Err }, State) ->
    ?debug("handle_nlmsg: ERROR: ~p", [State#state.request]),
    case State#state.request of
	undefined ->
	    dispatch_command(State);
	#request { tmr = Tmr, from = From } ->
	    ?debug("handle_nlmsg: DONE: ~p", 
		   [State#state.request]),
	    erlang:cancel_timer(Tmr),
	    %% fixme: convert errno to posix error (netlink.inc?)
	    gen_server:reply(From, {error,Err}),
	    State1 = State#state { request = undefined },
	    dispatch_command(State1)
    end;

handle_nlmsg(RTM, State) ->
    ?debug("netlink: handle_nlmsg, ignore ~p", [RTM]),
    State.

%% update attributes form interface "Name"
%% From to To Type is either link | addr
update_attrs(Name,Type,As,undefined,Subs) ->
    lists:foreach(
      fun({K,Vold}) ->
	      send_event(Name,Type,K,Vold,undefined,Subs)
      end, As),
    undefined;
update_attrs(Name,Type,As,To,Subs) ->
    lists:foldl(
      fun({K,Vnew},D) ->
	      case dict:find(K,D) of
		  error -> 
		      send_event(Name,Type,K,undefined,Vnew,Subs),
		      dict:store(K,Vnew,D);
		  {ok,Vnew} -> D;  %% already exist
		  {ok,Vold} ->
		      send_event(Name,Type,K,Vold,Vnew,Subs),
		      dict:store(K,Vnew,D)
	      end
      end, To, As).


send_event(Name,Type,Field,Old,New,[S|SList]) when 
      S#subscription.name =:= Name; S#subscription.name =:= "" ->
    case S#subscription.fields =:= all orelse
	S#subscription.fields =:= Type orelse 
	lists:member(Field,S#subscription.fields) orelse
	lists:member({Type,Field},S#subscription.fields) of
	true ->
	    S#subscription.pid ! {netlink,S#subscription.mon,
				  Name,Field,Old,New},
	    send_event(Name,Type,Field,Old,New,SList);
	false ->
	    send_event(Name,Type,Field,Old,New,SList)
    end;
send_event(Name,Type,Field,Old,New,[_|SList]) ->
    send_event(Name,Type,Field,Old,New,SList);
send_event(_Name,_Type,_Field,_Old,_New,[]) ->
    ok.
	    

match(Y,L,[{Field,Value}|Match]) when is_atom(Field) ->
    case find2(Field,Y,L) of
	{ok,Value} -> match(Y, L, Match);
	_ -> false
    end;
match(Y,L,[{Op,Field,Value}|Match]) when is_atom(Op),is_atom(Field) ->
    case find2(Y,L,Field) of
	{ok,FValue} ->
	    case compare(Op,FValue,Value) of
		true -> match(Y,L,Match);
		false -> false
	    end;
	error ->
	    false
    end;
match(_Y, _L, []) ->
    true.

find2(Key,D1,D2) ->
    case dict:find(Key,D1) of
	error ->
	    dict:find(Key,D2);
	Res -> Res
    end.


format_link(L) ->
    dict:fold(
      fun(af_spec,_V,A) -> A;
	 (map,_V,A) -> A;
	 (stats,_V,A) -> A;
	 (stats64,_V,A) -> A;
	 (change,_V,A) -> A;
	 (K,V,A) ->
	      [["\n    ",name_to_list(K), " ",value_to_list(K,V),";"]|A]
      end, [], L#link.attr).

format_addr(Y) ->
    ["\n", "    addr {",
     dict:fold(
       fun(cacheinfo,_V,A) -> A;
	  (K,V,A) ->
	       [[" ",name_to_list(K), " ",value_to_list(K,V),";"]|A]
       end, [], Y#addr.attr), "}"].

name_to_list(K) when is_atom(K) ->
    atom_to_list(K);
name_to_list(K) when is_integer(K) ->
    integer_to_list(K).


value_to_list(local,V)     -> format_a(V);
value_to_list(address,V)   -> format_a(V);
value_to_list(broadcast,V) -> format_a(V);
value_to_list(multicast,V) -> format_a(V);
value_to_list(anycast,V)   -> format_a(V);
value_to_list(_, V) -> io_lib:format("~p", [V]).

format_a(undefined) -> "";
format_a(A) when is_tuple(A), tuple_size(A) =:= 6 ->
    io_lib:format("~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b",
		  tuple_to_list(A));
format_a(A) when is_tuple(A), tuple_size(A) =:= 4 ->
    inet_parse:ntoa(A);
format_a(A) when is_tuple(A), tuple_size(A) =:= 8 ->
    inet_parse:ntoa(A).

compare('==',A,B) -> A == B;
compare('=:=',A,B) -> A =:= B;
compare('<' ,A,B) -> A < B;
compare('=<' ,A,B) -> A =< B;
compare('>' ,A,B) -> A > B;
compare('>=' ,A,B) -> A >= B;
compare('/=' ,A,B) -> A /= B;
compare('=/=' ,A,B) -> A =/= B;
compare(_,_,_) -> false.
