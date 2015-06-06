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
%%%-------------------------------------------------------------------
%%% File    : bt_sdp_srv.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : BLuetooth SDP server
%%%
%%% Created : 31 Jul 2006 by Tony Rogvall <tony@PBook.local>
%%%-------------------------------------------------------------------
-module(bt_sdp_srv).

-behaviour(gen_server).

-include("../include/bt.hrl").
-include("../include/sdp.hrl").

-compile(export_all).

%% API
-export([start/0, start_link/0, stop/0]).

-export([service_add/1,
	 service_add_persist/1,
	 service_del/1]).

-export([rfcomm_channel/1,
	 service_search/1,
	 service_attribute/2,
	 service_search_attribute/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(lists, [reverse/1, map/2, foldl/3, filter/2]).

-record(serviceRecord,
	{
	  handle,          %% generated 32 bit handle
	  monitor,         %% creator monitor (or undefined for persist)
	  uuid_set=[],     %% list of uuid included in this record
	  rfcomm_list=[],  %% list of used rfcomm channels
	  psm_list=[],     %% list of used psm (dynamic)
	  attributes=[]
	 }).

-record(state,
	{
	  %% rfcomm free list (iset)
	  rfcomm =  [],
	  %% dynamic psm free list (iset)
	  psm    =  [],
	  records = []
	 }).

-define(SERVER, bt_sdp_srv).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

service_add(ServiceRecord) when is_list(ServiceRecord) ->
    gen_server:call(?SERVER, {service_add,self(),ServiceRecord}).

service_add_persist(ServiceRecord) when is_list(ServiceRecord) ->
    gen_server:call(?SERVER, {service_add,undefined,ServiceRecord}).

service_del(Handle) when is_integer(Handle) ->
    gen_server:call(?SERVER, {service_del, Handle}).

rfcomm_channel(UUID) when is_binary(UUID) ->
    gen_server:call(?SERVER, {rfcomm_channel, UUID});
rfcomm_channel(Handle) when is_integer(Handle) ->
    gen_server:call(?SERVER, {rfcomm_channel, Handle}). 

service_search(UUIDList) when is_list(UUIDList) ->    
    gen_server:call(?SERVER, {service_search, uuid_set(UUIDList)}).

service_attribute(Handle, AttributeList) 
  when is_integer(Handle), is_list(AttributeList) ->
    gen_server:call(?SERVER, {service_attribute,Handle,
			      attribute_iset(AttributeList)}).

service_search_attribute(UUIDList,AttributeList) 
  when is_list(UUIDList), is_list(AttributeList) ->
    gen_server:call(?SERVER, {service_search_attribute,
			      uuid_set(UUIDList),
			      attribute_iset(AttributeList)}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    SDP = #serviceRecord { handle = 0,
			   monitor = undefined,
			   uuid_set=uuid_set(
				      [?UUID_ServiceDiscoveryServer,
				       ?UUID_L2CAP]),
			   rfcomm_list=[],
			   psm_list=[],
			   attributes=
			   [{?ATTR_ServiceRecordHandle,{uint32,0}},
			    {?ATTR_ServiceClassIDList,
			     {sequence,[{uuid,?UUID_ServiceDiscoveryServer}]}},
			    {?ATTR_ServiceRecordState,{uint32,0}},
			    {?ATTR_ProtocolDescriptorList,
			     {sequence,
			      [{sequence,[{uuid,?UUID_L2CAP}]}]}},
			    {?ATTR_BrowseGroupList,
			     {sequence,[{uuid,?UUID_ServiceDiscoveryServer}]}},
			    {?ATTR_LanguageBaseAttributeIDList,
			     {sequence, [{uint16, ?LANGUAGE($e,$n)}, 
					 {uint16, ?ENCODING_UTF8}, 
					 {uint16, 16#0100}]}},
			    {?ATTR_ServiceInfoTimeToLive,
			     {uint32, 1200}},
			    {?ATTR_ServiceAvailability,
			     {uint8, 255}},
			    {?ATTR_ServiceName+16#0100, {text,"SDP Server"}}
			   ]},
    {ok,
     #state { rfcomm = bt_iset:from_list([{1,30}]),
	      psm    = bt_iset:from_list([{?L2CAP_PSM_DynamicStart, 
					   ?L2CAP_PSM_DynamicEnd}]),
	      records = [SDP]
	     }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({service_add,Pid,ServiceRecord}, _From, State) ->
    case is_service_record(ServiceRecord) of
	true ->
	    case add_service(ServiceRecord, Pid, State) of
		{ok,{Handle,State1}} ->
		    {reply,{ok,Handle},State1};
		{Error,State1} ->
		    {reply,Error,State1}
	    end;
	false ->
	    {reply,{error,einval},State}
    end;
handle_call({service_del,Handle}, _From, State) ->
    {reply, ok, remove_service(Handle, State)};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({rfcomm_channel,UUID},_From,State) when is_binary(UUID) ->
    case match_service(ordsets:from_list([UUID]), State#state.records) of
	[] ->
	    {reply, {error, not_found}, State};
	[R] ->
	    {reply,{ok,get_rfcomm_channels(R#serviceRecord.attributes)},State};
	_Rs ->
	    {reply, {error,multiple_match},State}
    end;
handle_call({rfcomm_channel,Handle},_From,State) when is_integer(Handle) ->
    case lists:keysearch(Handle,#serviceRecord.handle,State#state.records) of
	false ->
	    {reply, {error, not_found}, State};
	{value,R} ->
	    {reply,{ok,get_rfcomm_channels(R#serviceRecord.attributes)},State}
    end;
handle_call({service_search, UUIDSet}, _From, State) ->
    Rs = match_service(UUIDSet, State#state.records),
    {reply, {ok,map(fun(R) -> R#serviceRecord.handle end,Rs)}, State};
handle_call({service_attribute,Handle,AttributeISet}, _From, State) ->
    case lists:keysearch(Handle,#serviceRecord.handle,State#state.records) of
	false ->
	    {reply,{error,bad_handle},State};
	{value,R} ->
	    Is = filter(fun({ID,_}) ->
				bt_iset:is_element(ID, AttributeISet)
			end, R#serviceRecord.attributes),
	    {reply,{ok,Is},State}
    end;
handle_call({service_search_attribute,UUIDSet,AttributeISet}, _From, State) ->
    Rs = match_service(UUIDSet, State#state.records),
    IRs = 
	map(fun(R) ->
		    filter(fun({ID,_}) ->
				   bt_iset:is_element(ID, AttributeISet)
			   end, R#serviceRecord.attributes)
	    end, Rs),
    {reply,{ok,IRs},State};
handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN',Ref,_Pid,process,_Reason}, State) ->
    State1 = remove_service(Ref, State),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

add_monitor(undefined) ->
    undefined;
add_monitor(Pid) when is_pid(Pid) ->
    erlang:monitor(process, Pid).

del_monitor(undefined) ->
    undefined;
del_monitor(Mon) when is_reference(Mon) ->
    erlang:demonitor(Mon).

new_handle(State) ->
    Handle = random:uniform(16#100000000)-1,
    case lists:keysearch(Handle,#serviceRecord.handle,State#state.records) of
	false ->
	    Handle;
	{value,_} ->
	    new_handle(State)
    end.

%% match all services that have the UUIDSet as a subset.
match_service(UUIDSet, Records) ->
    lists:filter(fun(R) ->
			 ordsets:is_subset(UUIDSet, R#serviceRecord.uuid_set)
		 end, Records).

%%
%% Add a service 
%%
add_service(ServiceRecord, Pid, State) ->
    %% Collect all UUID's for search operations
    UUIDSet =
	foldl(fun({_ID,Value}, Acc) ->
		      bt_sdp:fold_value(
			fun({uuid,UUID},Acc1) when is_binary(UUID) ->
				UUID128 = bt_sdp:uuid_128(UUID),
				ordsets:add_element(UUID128,Acc1);
			   ({uuid,UUID},Acc1) when 
				  is_list(UUID); is_atom(UUID) ->
				UUID1 = bt_sdp:string_to_uuid(UUID),
				UUID128 = bt_sdp:uuid_128(UUID1),
				ordsets:add_element(UUID128,Acc1);
			   (_, Acc1) -> Acc1
			end, Acc, Value)
	      end, ordsets:new(), ServiceRecord),
    RequestCn = get_rfcomm_channels(ServiceRecord),
    RequestPsm = get_l2cap_psms(ServiceRecord),
    case avail(RequestCn, State#state.rfcomm) andalso
	 avail(RequestPsm, State#state.psm) of
	true ->
	    {CnMap,RfComm} = allocate(RequestCn, State#state.rfcomm),
	    ServiceRecord1 = set_rfcomm_channels(ServiceRecord, CnMap),

	    {PsmMap,Psm} = allocate(RequestPsm, State#state.psm),
	    ServiceRecord2 = set_l2cap_psms(ServiceRecord1, PsmMap),

	    Handle = new_handle(State),
	    ServiceRecord3 = set_attribute({?ATTR_ServiceRecordHandle,
					    {uint32,Handle}},
					   ServiceRecord2),
	    R = #serviceRecord { handle = Handle,
				 monitor = add_monitor(Pid),
				 uuid_set = UUIDSet,
				 rfcomm_list = map(fun({_,CN}) -> CN end, CnMap),
				 psm_list = map(fun({_,PSM}) -> PSM end, PsmMap),
				 attributes = ServiceRecord3 },
	    Rs = [R | State#state.records],
	    {ok,{Handle,State#state { records = Rs,
				      rfcomm = RfComm,
				      psm = Psm }}};
	false ->
	    {{error,insufficient_resources},State}
    end.
      

remove_service(Handle, State) when is_integer(Handle) ->
    remove_service(Handle, #serviceRecord.handle, State);
remove_service(Monitor, State) when is_reference(Monitor) ->
    remove_service(Monitor, #serviceRecord.monitor, State).

remove_service(Key, Pos, State) ->
    case lists:keysearch(Key, Pos, State#state.records) of
	false ->
	    State;
	{value,R} ->
	    del_monitor(R#serviceRecord.monitor),
	    RFComm = bt_iset:add_elements(R#serviceRecord.rfcomm_list, 
					  State#state.rfcomm),
	    PSM    = bt_iset:add_elements(R#serviceRecord.psm_list,
					  State#state.psm),
	    Rs = lists:keydelete(Key, Pos, State#state.records),
	    State#state { rfcomm=RFComm, psm=PSM, records = Rs }
    end.

%% find attribute
lookup_attribute(ID, ServiceRecord) ->
    lists:keysearch(ID,1,ServiceRecord).

%% add or replace an attribute
set_attribute(Attribute={ID,_}, ServiceRecord) ->
    case lists:keysearch(ID, 1, ServiceRecord) of
	false ->
	    [Attribute | ServiceRecord];
	{value,_} ->
	    lists:keyreplace(ID,1,ServiceRecord,Attribute)
    end.

del_attribute(ID, ServiceRecord) ->
    lists:keydelete(ID, 1, ServiceRecord).

is_service_record([{ID,Elem}|IDList]) when ID>=0,ID=<16#ffff ->
    case bt_sdp:is_value(Elem) of
	true ->
	    is_service_record(IDList);
	false -> false
    end;
is_service_record([_|_]) -> false;
is_service_record([]) -> true.


    
    

avail(Channels, FreeSet) ->
    Sz = bt_iset:size(FreeSet),
    Len = length(Channels),
    if Sz < Len ->
	    false;
       true -> true
    end.

allocate(Channels,FreeSet) ->
    %% make dynamic channals (0) last in list
    RSortedChannels = reverse(lists:sort(Channels)),
    Sz = bt_iset:size(FreeSet),
    Len = length(RSortedChannels),
    if Sz < Len ->
	    io:format("Warning: can not allocate all channels\n");
       true ->
	    ok
    end,
    allocate(RSortedChannels,[],FreeSet).
    
allocate([0|Cns],Map,FreeSet) ->
    case bt_iset:size(FreeSet) of
	0 -> Map;
	_ -> 
	    Cn1 = bt_iset:first(FreeSet),
	    allocate(Cns,[{0,Cn1}|Map],bt_iset:del_element(Cn1,FreeSet))
    end;
allocate([Cn|Cns],Map,FreeSet) ->
    case bt_iset:is_element(Cn,FreeSet) of
	true ->
	    allocate(Cns,[{Cn,Cn}|Map],bt_iset:del_element(Cn,FreeSet));
	false ->
	    case bt_iset:size(FreeSet) of
		0 -> Map;
		_ -> 
		    Cn1 = bt_iset:first(FreeSet),
		    allocate(Cns,[{Cn,Cn1}|Map],bt_iset:del_element(Cn1,FreeSet))
	    end
    end;
allocate([], Map, FreeSet) ->
    {Map,FreeSet}.

    

get_rfcomm_channels(ServiceRecord) ->
    case lookup_attribute(?ATTR_ProtocolDescriptorList,ServiceRecord) of
	false -> [];
	{value,{_,Value}} ->
	    get_cn(Value)
    end.


set_rfcomm_channels(ServiceRecord, CnMap) ->
    case lookup_attribute(?ATTR_ProtocolDescriptorList,ServiceRecord) of
	false ->
	    ServiceRecord;
	{value,{_,Value}} ->
	    case set_cn(Value, CnMap) of
		{Value1,[]} ->
		    set_attribute({?ATTR_ProtocolDescriptorList,Value1},
				  ServiceRecord);
		{Value1,CnMap1} ->
		    io:format("Warning: unable to assign cn's=~p to ~p\n",
			      [CnMap1, Value1]),
		    set_attribute({?ATTR_ProtocolDescriptorList,Value1},
				  ServiceRecord)
	    end
    end.


get_l2cap_psms(ServiceRecord) ->
    case lookup_attribute(?ATTR_ProtocolDescriptorList,ServiceRecord) of
	false -> [];
	{value,{_,Value}} ->
	    get_psm(Value)
    end.

set_l2cap_psms(ServiceRecord, PsmMap) ->
    case lookup_attribute(?ATTR_ProtocolDescriptorList,ServiceRecord) of
	false ->
	    ServiceRecord;
	{value,{_,Value}} ->
	    case set_psm(Value, PsmMap) of
		{Value1,[]} ->
		    set_attribute({?ATTR_ProtocolDescriptorList,Value1},
				  ServiceRecord);
		{Value1,PsmMap1} ->
		    io:format("Warning: unable to assign psm's=~p to ~p\n",
			      [PsmMap1, Value1]),
		    set_attribute({?ATTR_ProtocolDescriptorList,Value1},
				  ServiceRecord)
	    end
    end.



%% locate rfcomm channels (0 = allocate)
get_cn({alternative,List}) ->
    lists:flatmap(fun(E) -> get_cn(E) end, List);
get_cn({sequence,[{sequence,[{uuid,?UUID_L2CAP}|_]},
		  {sequence,[{uuid,?UUID_RFCOMM},
			     {uint8,Channel}|_]} | _]}) ->
    [Channel];
get_cn({sequence,[{sequence,[{uuid,?UUID_L2CAP}|_]},
		  {sequence,[{uuid,?UUID_RFCOMM}]} | _]}) ->
    %% need allocation
    [0];
get_cn(_) ->
    [].


%% assign rfcomm channels
set_cn({alternative,List}, CnMap) ->
    {List1, CnMap1} = set_cn_list(List,[],CnMap),
    {{alternative,List1},CnMap1};
set_cn(Value=
       {sequence,[{sequence,L2CAP}, %% assume L2CAP
		  {sequence,[{uuid,?UUID_RFCOMM},
			     {uint8,Cn}|RFParam]} | T]}, CnMap) ->
    case lists:keysearch(Cn,1,CnMap) of
	{value,{Cn,Cn1}} ->
	    {{sequence,[{sequence,L2CAP},
			{sequence,[{uuid,?UUID_RFCOMM},
				   {uint8,Cn1}|RFParam]} | T]}, 
	     lists:keydelete(Cn,1,CnMap)};
	false ->
	    {Value, CnMap}
    end;
set_cn(Value={sequence,[{sequence,L2CAP}, %% assume L2CAP
			{sequence,[{uuid,?UUID_RFCOMM}]} | T]},CnMap) ->
    case lists:keysearch(0,1,CnMap) of
	{value,{0,Cn1}} ->	
	    {{sequence,[{sequence,L2CAP},
			{sequence,[{uuid,?UUID_RFCOMM},
				   {uint8,Cn1}]} | T]}, 
	     lists:keydelete(0,1,CnMap)};
	false ->
	    {Value,CnMap}
    end;
set_cn(Value,CnMap) ->
    {Value,CnMap}.

set_cn_list([V|Vs],Acc,CnMap) ->
    {V1, CnMap1} = set_cn(V, CnMap),
    set_cn_list(Vs,[V1|Acc],CnMap1);
set_cn_list([],Acc,CnMap) ->
    {reverse(Acc), CnMap}.


%% locate l2cap psm (0 = allocate)
get_psm({alternative,List}) ->
    lists:flatmap(fun(E) -> get_psm(E) end, List);
get_psm({sequence,[{sequence,[{uuid,?UUID_L2CAP},{uint16,PSM}|_]} | _]}) ->
    [PSM];
get_psm(_) ->
    [].


%% assign psm 
set_psm({alternative,List}, PsmMap) ->
    {List1, PsmMap1} = set_psm_list(List,[],PsmMap),
    {{alternative,List1},PsmMap1};
set_psm(Value={sequence,[{sequence,[{uuid,?UUID_L2CAP},
				    {uint16,PSM}|L2CAP]} | T]},PsmMap) ->
    case lists:keysearch(PSM,1,PsmMap) of
	{value,{PSM,PSM1}} ->
	    {{sequence,[{sequence,[{uuid,?UUID_L2CAP},
				   {uint16,PSM1}|L2CAP]} | T]},
	     lists:keydelete(PSM,1,PsmMap)};
	false ->
	    {Value, PsmMap}
    end;
set_psm(Value,PsmMap) ->
    {Value,PsmMap}.

set_psm_list([V|Vs],Acc,PsmMap) ->
    {V1, PsmMap1} = set_psm(V, PsmMap),
    set_psm_list(Vs,[V1|Acc],PsmMap1);
set_psm_list([],Acc,PsmMap) ->
    {reverse(Acc), PsmMap}.


%% convert a list of UUID (Mix of binary|string|atom) into 
%% a list of 128 full UUID
uuid_set(UUIDList) ->
    ordsets:from_list(
      map(
	fun(UUID) when is_binary(UUID) -> 
		bt_sdp:uuid_128(UUID);
	   (UUID) when is_list(UUID); is_atom(UUID) -> 
		bt_sdp:uuid_128(bt_sdp:string_to_uuid(UUID))
	end, UUIDList)).

attribute_iset(AttributeList) ->
    bt_iset:from_list(
      map(
	fun({A1,A2}) ->
		{if is_list(A1); is_atom(A1) ->
			 bt_sdp:string_to_attribute(A1);
		    is_integer(A1) ->
			 A1 band 16#ffff
		 end,
		 if is_list(A2); is_atom(A2) ->
			 bt_sdp:string_to_attribute(A2);
		    is_integer(A2) ->
			 A2 band 16#ffff
		 end};
	   (A) when is_list(A); is_atom(A) ->
		bt_sdp:string_to_attribute(A);
	   (A) when is_integer(A) ->
		A band 16#ffff
	end, AttributeList)).
