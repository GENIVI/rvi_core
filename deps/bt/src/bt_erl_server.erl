%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Bluetooth distribution
%%% @end
%%% Created :  3 Dec 2014 by Tony Rogvall <tony@rogvall.se>

-module(bt_erl_server).

-export([sdp_info/2]).
-compile(export_all).

-include("../include/sdp.hrl").

-define(UUID_ErlangNode,
	<<16#0f1662dc:32,16#0c1e:16,16#4e18:16,16#acd7:16,16#da61165bcd29:48>>).

sdp_info(NodeName,Channel) ->
    Base1 = 16#0100,
    [
     {?ATTR_ServiceRecordHandle, {uint32,0}},   %% Should not be needed ?
     {?ATTR_ServiceClassIDList, {sequence,[{uuid,?UUID_ErlangNode}]}},
     {?ATTR_ProtocolDescriptorList,
      {sequence,[{sequence,[{uuid,?UUID_L2CAP}]},
		 {sequence,[{uuid,?UUID_RFCOMM},{uint8,Channel}]}]}},
     {?ATTR_BrowseGroupList,
      {sequence,[{uuid,?UUID_PublicBrowseGroup}]}},
     {?ATTR_LanguageBaseAttributeIDList,
      {sequence, [{uint16, ?LANGUAGE($e,$n)},
		  {uint16, ?ENCODING_UTF8},
		  {uint16, Base1}]}},
     {?ATTR_ServiceName+Base1, {text,NodeName}}].


start(NodeName) ->
    start(NodeName,1).
start(NodeName, ChannelHint) ->
    spawn(fun() -> init(NodeName,ChannelHint) end).

init(NodeName, ChannelHint) ->
    case bt_drv:service_add(sdp_info(NodeName,ChannelHint)) of
	{ok,Handle} ->
	    case bt_drv:service_rfcomm(Handle) of
		{ok,RealChannel} ->
		    io:format("Registered server: ~p rfcomm=~p\n",
			      [Handle,RealChannel]),
		    init_rfcomm(RealChannel);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

init_rfcomm(Channel) ->
    process_flag(trap_exit, true),
    case rfcomm:listen(Channel) of
	{ok,ListenRef} ->
	    listen_loop(ListenRef);
	Error ->
	    Error
    end.

listen_loop(ListenRef) ->
    Pid = spawn_link(?MODULE, accept, [self(), ListenRef]),
    receive
	{accept,Pid,ok} ->
	    listen_loop(ListenRef);
	{accept,Pid,{error,Reason}} ->
	    io:format("Accept error ~p\n", [Reason]),
	    listen_loop(ListenRef)
    end.

accept(Listen, ListenRef) ->
    case rfcomm:accept(ListenRef) of
	{ok,ARef} ->
	    receive
		{rfcomm,ARef,{accept,Address,Channel}} ->
		    Listen ! {accept, self(), ok},
		    io:format("connection from ~w:~w\n", [Address,Channel]),
		    main(ARef,Address,Channel);
		{rfcomm,ARef,{data,_Data}} ->
		    io:format("error: got data ~p\n", [_Data]),
		    rfcomm:close(ARef),
		    Listen ! {accept, self(), {error,data}},
		    error;
		{rfcomm,ARef,closed} ->
		    rfcomm:close(ARef),
		    Listen ! {accept, self(), {error,closed}},
		    closed
	    end;
	Error ->
	    Error
    end.


main(Ref,Address,Channel) ->
    receive
	{rfcomm,Ref,{data,Data}} ->
	    rfcomm:send(Ref, ["Echo:",Data]),
	    main(Ref,Address,Channel);
	{rfcomm,Ref,closed} ->
	    rfcomm:close(Ref),
	    normal;
	Other ->
	    io:format("bt_erl_server: main other ~w\n", [Other]),
	    main(Ref,Address,Channel)
    end.
