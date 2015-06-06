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
%%% File    : bt.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : Bluetooth utilities
%%% Created : 31 Jan 2006 by Tony Rogvall <tony@iMac.local>

-module(bt).

-export([start/0, stop/0]).
-export([i/0, i/1]).
-export([li/0]).
-export([s/1, s/2]).
-export([debug/0, debug/1]).
-export([scan/1, scan/3]).
-export([getaddr/1, getaddr_by_name/1]).
-export([format_address/1]).
-export([service_info/1, service_info/2]).
-export([rfcomm_channel/2]).
-export([decode_service/1]).

-import(lists, [foreach/2, map/2]).

-include("../include/bt.hrl").
-include("../include/uuid.hrl").
-include("../include/sdp.hrl").

start() ->
    application:start(bt).

stop() -> %% restart=
    bt_drv:stop().


debug() ->
    bt_drv:debug(debug).

debug(true) ->
    bt_drv:debug(debug);
debug(false) ->
    bt_drv:debug(none);
debug(Level) ->
    bt_drv:debug(Level).

%%
%% Dump device information
%%
i() ->
    {ok,Devices} = bt_drv:devices(),
    foreach(fun(A) -> i(A) end, Devices).

i(paired) ->
    {ok, Devices} = bt_drv:paired_devices(),
    foreach(fun(A) -> i(A) end, Devices);
i(favorite) ->
    {ok, Devices} = bt_drv:favorite_devices(),
    foreach(fun(A) -> i(A) end, Devices);
i(BtAddr) ->
    {ok,A} = getaddr(BtAddr),
    case bt_drv:device_info(A, [inquiry,update]) of
	{ok, [{inquiry,?never},{update,_}]} ->
	    io:format("Address: ~s\n", [format_address(A)]);
	{ok, [{inquiry,_InQuiry},{update,Update}]}->
	    io:format("Address: ~s\n", [format_address(A)]),
	    {ok,DevInfo} = bt_drv:device_info(A,[name,
						 is_paired,
						 is_favorite,
						 is_connected,
						 class]),
	    foreach(fun({class,Value}) ->
			    {Service,Major,Minor} = bt_drv:decode_class(Value),
			    io:format("   major: ~p\n", [Major]),
			    io:format("   minor: ~p\n", [Minor]),
			    io:format(" service: ~p\n", [Service]);
		       ({name,Name}) ->
			    io:format("  name: ~s\n", [Name]);
		       ({What,Value}) ->
			    io:format("  ~p: ~p\n", [What,Value])
		    end, DevInfo),
	    if Update == ?never ->
		    ok;
	       true ->
		    {ok,SdpInfo} = bt_drv:service_info(A),
		    io:format("  Profiles:"),
		    foreach(
		      fun(Service) ->
			      As = map(fun(A1) -> 
					       bt_sdp:decode(A1) 
				       end, Service),
			      case lists:keysearch(256, 1, As) of
				  false -> ok;
				  {value,{_,{text,Name}}} -> 
				      io:format(" ~s,", [Name])
			      end
		      end, SdpInfo),
		    io:format("\n")
	    end;
	Error -> Error
    end.

%%
%% Dump information about the local bluetooth adapter
%%
li() ->
    case bt_drv:local_info([name,class,address,discoverable,power_state]) of
	{ok,Info} ->
	    io:format("Local Adapter:\n",[]),
	    foreach(fun({class,Value}) ->
			    {Service,Major,Minor} = bt_drv:decode_class(Value),
			    io:format("   major: ~p\n", [Major]),
			    io:format("   minor: ~p\n", [Minor]),
			    io:format(" service: ~p\n", [Service]);
		       ({name,Name}) ->
			    io:format("  name: ~s\n", [Name]);
		       ({address,Addr}) ->
			    io:format("  addr: ~s\n", [format_address(Addr)]);
		       ({What,Value}) ->
			    io:format("  ~p: ~p\n", [What,Value])
		    end, Info);
	Error ->
	    Error
    end.

%%
%% Dump service information
%% s(Addr [UUID])
%%
%% Addr is either the Bluetooth address (as string or tuple) or
%%  the name of the device (a bit slow)
%% UUID is UUID16 | UUID32 | UUID128 | Symbolic-Name
%%
s(Addr) ->
    case bt_drv:service_info(Addr,<<>>) of
	{ok, Info} when is_list(Info) ->
	    foreach(
	      fun(Service) when is_list(Service) ->
		      s_serv(Service),
		      io:format("\n")
	      end, Info);
	Error ->
	    Error
    end.

s(Addr, UUID) when is_binary(UUID), size(UUID) > 0 ->
    case bt_drv:service_info(Addr,UUID) of
	{ok, Service} ->
	    s_serv(Service);
	Error ->
	    Error
    end;
s(Addr, Name) when is_list(Name) ->
    UUID = bt_sdp:string_to_uuid(Name),
    case bt_drv:service_info(Addr,UUID) of
	{ok, Service} ->
	    s_serv(Service);
	Error ->
	    Error
    end.

s_serv(Attributes=[A1|_]) when is_binary(A1) ->
    Attrs0 = map(fun(A) -> bt_sdp:decode(A) end, Attributes),
    LangList = [{_,_,Base}|_] = get_language_base(Attrs0, 256),
    UUID = case lists:keysearch(?ATTR_ServiceClassIDList, 1, Attrs0) of
	       false -> <<>>;
	       {value,{_,{sequence,[{uuid,UUID0}|_]}}} -> UUID0
	   end,
    %% Fixme: present ServiceName,ServiceDescription,ProviderName with language base
    %% and delete them before printing further data.
    ServiceNames = s_attr(?ATTR_ServiceName, LangList, Attrs0, []),
    ServiceDescs = s_attr(?ATTR_ServiceDescription, LangList, Attrs0, []),
    ProviderNames = s_attr(?ATTR_ProviderName, LangList, Attrs0, []),
    
    case ServiceNames of
	[{_,{text,Name}}|_] -> io:format("Service Name: ~s\n", [Name]);
	[] -> io:format("Service Name: None\n", [])
    end,

    case ServiceDescs of
	[{_,{text,Desc}}|_] -> io:format("  Service Description: ~s\n", [Desc]);
	[] -> ok
    end,

    case ProviderNames of
	[{_,{text,Prov}}|_] -> io:format("  Provider Name: ~s\n", [Prov]);
	[] -> ok
    end,
    Attrs1 = Attrs0 -- (ServiceNames++ServiceDescs++ProviderNames),
    Attrs = lists:keydelete(?ATTR_LanguageBaseAttributeIDList, 1, Attrs1),
    foreach(
      fun ({ID,Value}) ->
	      AttrName = bt_sdp:attribute_to_string(ID,Base,UUID),
	      io:format("  ~s: ~s\n", [AttrName, bt_sdp:value_to_string(Value)])
      end, Attrs).

s_attr(Attr, [{_Lang,_Env,Base}|Ls], As, Acc) ->
    case lists:keysearch(Attr+Base, 1, As) of
	false ->
	    s_attr(Attr, Ls, As, Acc);
	{value,Value} ->
	    s_attr(Attr, Ls, As, [Value|Acc])
    end;
s_attr(_Attr, [], _As, Acc) ->
    lists:reverse(Acc).


get_language_base(Attrs,Default) ->
    case get_languages(Attrs) of
	[] -> [{25966,106,Default}];
	Ls -> Ls
    end.

get_languages(Attrs) ->
    case lists:keysearch(?ATTR_LanguageBaseAttributeIDList, 1, Attrs) of
	{value,{_,{sequence,Seq}}} -> get_lang3(Seq);
	_ -> []
    end.

get_lang3([{uint16,Lang},{uint16,Encoding},{uint16,Base}|Ls]) ->
    [{Lang,Encoding,Base} | get_lang3(Ls)];
get_lang3([]) ->
    [].

    
%%
%% Decode all services on a device
%%

service_info(Addr) ->
    case bt_drv:service_info(Addr,<<>>) of
	{ok, ServiceList} when is_list(hd(ServiceList)) ->
	    map(fun(Attributes) ->
			decode_service(Attributes)
		end, ServiceList);
	Error ->
	    Error
    end.
%%
%% 
%%
service_info(Addr, Service) when is_list(Service) ->
    service_info(Addr, bt_sdp:string_to_uuid(Service));
service_info(Addr, UUID) when is_binary(UUID), size(UUID) > 0 ->
    case bt_drv:service_info(Addr,UUID) of
	{ok, Service} when is_binary(hd(Service)) ->
	    decode_service(Service);
	Error ->
	    Error
    end.
%%
%% Extract rfcomm channel for the given service 
%%
rfcomm_channel(Addr, Service) when is_list(Service) ->
    rfcomm_channel(Addr, bt_sdp:string_to_uuid(Service));
rfcomm_channel(Addr, UUID) when is_binary(UUID), size(UUID) > 0 ->
    case bt_drv:service_info(Addr,UUID) of
	{ok, Service} when is_binary(hd(Service)) ->
	    As = decode_service(Service),
	    case lists:keysearch(?ATTR_ProtocolDescriptorList, 1, As) of
		{value,{_, {sequence,[{sequence,["L2CAP"]},
				      {sequence,["RFCOMM",Channel]}|_]}}} ->
		    {ok,Channel};
		_ ->
		    {error, no_channel}
	    end;
	Error -> Error
    end.
    
%%
%% Decode a binary encoded SDP list:
%% [ binary(<attribute><value>) ] in SDP format
%%    

decode_service(Attributes) when is_binary(hd(Attributes)) ->
    Attrs = map(fun(A) -> bt_sdp:decode(A) end, Attributes),
    map(fun({ID,Value}) -> 
		{ID, bt_sdp:decode_sdp_value(Value)}
	end, Attrs).

getaddr(Name) ->
    bt_util:getaddr(Name).

getaddr_by_name(Name) ->
    bt_util:getaddr_by_name(Name).

%%
%% Inquiry scan: 
%%   Note that the Fun can not make meaningfull remote calls
%%   while inquiry is running.
%% 
%%
scan(Timeout) ->
    scan(Timeout, fun(Addr,Acc) -> {continue,[Addr|Acc]} end, []).

scan(Timeout, Fun, Acc) ->
    case bt_drv:inquiry_start(Timeout) of
	{ok,Ref} ->
	    receive
		{bt,Ref,started} ->
		    scan_loop(Ref, Fun, Acc)
	    end;
	Error ->
	    Error
    end.

scan_loop(Ref, Fun, Acc) ->
    receive
	{bt,Ref,{device,Addr}} ->
	    case Fun(Addr,Acc) of
		{continue,Acc1} -> 
		    scan_loop(Ref, Fun, Acc1);
		{stop,Acc1} ->
		    bt_drv:inquiry_stop(Ref),
		    {ok,Acc1}
	    end;
	{bt,Ref, stopped} ->
	    bt_drv:inquiry_stop(Ref),
	    {ok,Acc}
    end.

%%
%% Format bluetooth address into a hex string
%%
format_address(A) when ?is_bt_address(A) ->
    case os:type() of
	{unix,darwin} ->
	    format_address_(A, $-);
	_ ->
	    format_address_(A, $:)
    end;
format_address(<<F,E,D,C,B,A>>) -> %% binary is reverse format
    format_address({A,B,C,D,E,F}).
		
format_address_({A,B,C,D,E,F}, S) ->
    [hexh(A),hexl(A),S,hexh(B),hexl(B),S,hexh(C),hexl(C),S,
     hexh(D),hexl(D),S,hexh(E),hexl(E),S,hexh(F),hexl(F)].

hexl(A) -> hex1(A band 16#f).
hexh(A) -> hex1((A bsr 4) band 16#f).

hex1(A) when A < 10 -> A+$0;
hex1(A) -> (A-10)+$a.
