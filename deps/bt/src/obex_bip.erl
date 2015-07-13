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
%%% File    : obex_bip.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Basic Imaging profile
%%% Created :  2 Jun 2006 by Tony Rogvall <tony@PBook.local>

-module(obex_bip).

-include("../include/uuid.hrl").
-include("../include/obex.hrl").

-compile(export_all).

-export([start/0, start/1]).
-export([init/3,
	 terminate/1,
	 handle_connect/2,
	 handle_disconnect/2,
	 handle_get/2,
	 handle_put/3,
	 handle_abort/2,
	 handle_setpath/3,
	 handle_command/2]).
-export([sdp_info/1]).

-include("../include/sdp.hrl").

-record(s,
	{
	  sref,
	  opts
	 }).

-ifdef(debug).
-define(dbg(Fmt,As), io:format("~s:~w:" Fmt "\n", [?FILE,?LINE | As])).
-else.
-define(dbg(Fmt,As), ok).
-endif.


%% Basic Imaging primary session UUID 
-define(Basic_Imaging_Image_Push,
	?UUID(16#E33D9545,16#8374,16#4AD7,16#9EC5,16#C16BE31EDE8E)).
-define(Basic_Imaging_Image_Pull,
	?UUID(16#8EE9B3D0,16#4608,16#11D5,16#841A,16#0002A5325B4E)).
%% Basic Imaging Advanced Image 
-define(Basic_Imaging_Advanced_Image_Printing,
	?UUID(16#92353350,16#4608,16#11D5,16#841A,16#0002A5325B4E)).
-define(Basic_Imaging_Automatic_Archive,
	?UUID(16#940126C0,16#4608,16#11D5,16#841A,16#0002A5325B4E)).
-define(Basic_Imaging_Remote_Camera,
	?UUID(16#947E7420,16#4608,16#11D5,16#841A,16#0002A5325B4E)).
-define(Basic_Imaging_Remote_Display,
	?UUID(16#94C7CD20,16#4608,16#11D5,16#841A,16#0002A5325B4E)).

%% Basic Imaging secondary session UUID 
-define(Basic_Imaging_Referenced_Objects,
	?UUID(16#8E61F95D,16#1A79,16#11D4,16#8EA4,16#00805F9B9834)).
-define(Basic_Imaging_Archived_Objects,
	?UUID(16#8E61F95E,16#1A79,16#11D4,16#8EA4,16#00805F9B9834)).

-define(BIP_HDR_IMG_HANDLE,     16#30). %% unicode null terminated
-define(BIP_HDR_IMG_DESCRIPTOR, 16#71). %% bytes sequence 

%% Capabilities
-define(CAPA_GENERIC_IMAGING, 2#0001).
-define(CAPA_CAPTURING,       2#0010).
-define(CAPA_PRINING,         2#0100).
-define(CAPA_DISPLAYING,      2#1000).

%% Features
-define(FEATURE_IMAGE_PUSH,             2#000000001).
-define(FEATURE_IMAGE_PUSH_STORE,       2#000000010).
-define(FEATURE_IMAGE_PUSH_PRINT,       2#000000100).
-define(FEATURE_IMAGE_PUSH_DISPLAY,     2#000001000).
-define(FEATURE_IMAGE_PULL,             2#000010000).
-define(FEATURE_ADVANCED_IMAGE_PRINING, 2#000100000).
-define(FEATURE_AUTOMATIC_ARCHIVE,      2#001000000).
-define(FEATURE_REMOTE_CAMERA,          2#001000000).
-define(FEATURE_REMOTE_DISPLAY,         2#010000000).

%% Functions
-define(FUNC_GetCapabilities,     2#00000000000000001).
-define(FUNC_PutImage,            2#00000000000000010).
-define(FUNC_PutLinkedAttachment, 2#00000000000000100).
-define(FUNC_PutLinkedThumbnail,  2#00000000000001000).
-define(FUNC_RemoteDisplay,       2#00000000000010000).
-define(FUNC_GetImagesList,       2#00000000000100000).
-define(FUNC_GetImageProperties,  2#00000000001000000).
-define(FUNC_GetImage,            2#00000000010000000).
-define(FUNC_GetLinkedThumbnail,  2#00000000100000000).
-define(FUNC_GetLinkedAttachment, 2#00000001000000000).
-define(FUNC_DeleteImage,         2#00000010000000000).
-define(FUNC_StartPrint,          2#00000100000000000).
-define(FUNC_Reserved_12,         2#00001000000000000).
-define(FUNC_StartArchive,        2#00010000000000000).
-define(FUNC_GetMonitoringImage,  2#00100000000000000).
-define(FUNC_Reserved_15,         2#01000000000000000).
-define(FUNC_GetStatus,           2#10000000000000000).


sdp_info(Channel) ->
    Base1 = 16#0100,
    [{?ATTR_ServiceRecordHandle, {uint32, 65000}},   %% Should not be needed ?
     {?ATTR_ServiceClassIDList, 
      {sequence,[{uuid,?UUID_ImagingResponder}]}},
     {?ATTR_ProtocolDescriptorList,
      {sequence,[{sequence,[{uuid,?UUID_L2CAP}]},
		 {sequence,[{uuid,?UUID_RFCOMM},{uint8,Channel}]},
		 {sequence,[{uuid,?UUID_OBEX}]}]}},
     {?ATTR_BrowseGroupList,
      {sequence,[{uuid,?UUID_PublicBrowseGroup}]}},
     {?ATTR_LanguageBaseAttributeIDList,
      {sequence, [{uint16, ?LANGUAGE($e,$n)}, 
		  {uint16, ?ENCODING_UTF8}, 
		  {uint16, Base1}]}},
     {?ATTR_ServiceName+Base1, {text,"Imaging"}},
     {?ATTR_BluetoothProfileDescriptorList,
      {sequence,[{uuid,?UUID_Imaging},
		 {uint16, 16#0100}]}},
     {?ATTR_BIP_SupportedCapabilities, 
      {uint8, ?CAPA_GENERIC_IMAGING bor ?CAPA_DISPLAYING}},
     {?ATTR_BIP_SupportedFeatures, 
      {uint16, ?FEATURE_IMAGE_PUSH bor ?FEATURE_IMAGE_PUSH_DISPLAY}},
     {?ATTR_BIP_SupportedFunctions, 
      {uint32, ?FUNC_GetCapabilities bor ?FUNC_RemoteDisplay}},
     {?ATTR_BIP_TotalImagingDataCapacity,{uint64, (1 bsl 32)}}
    ].

start() ->
    start(15).

start(Channel) ->
    %% NOTE: The Channel number is just a hint, the
    %%  number may be any in the range 1-30
    obex:server(Channel, [{transport,rfcomm},
			  {sdp, sdp_info(Channel)},
			  {target,{?Basic_Imaging_Remote_Display,?MODULE}}]).




%% Basic Imaging Image Push
connect_push(Obex) ->
    obex:connect(Obex, [{target, ?Basic_Imaging_Image_Push}]).

connect_remote_display(Obex) ->
    obex:connect(Obex, [{target, ?Basic_Imaging_Remote_Display}]).

getCapabilites(Obex) ->
    case obex:get(Obex, [{type,"x-bt/img-capabilities"}]) of
	{ok,_Hs,Bin} ->
	    makeup:string(Bin);
	Error ->
	    Error
    end.

getListing(Obex) ->
    obex:get(Obex, [{type,"x-bt/img-listing"}]).

getProperties(Obex,Name) ->
    obex:get(Obex, [{type,"x-bt/img-properties"},{name,Name}]).

getImage(Obex,Name) ->
    obex:get(Obex, [{type,"x-bt/img-imq"},{name,Name}]).

getThumbnail(Obex,Name) ->
    obex:get(Obex, [{type,"x-bt/img-thm"},{name,Name}]).

remoteDisplay(Obex) ->
    obex:get(Obex, [{type,"x-bt/img-display"}]).


putImage(Obex,Name,Descriptor,Image) ->
    obex:put(Obex, [{type,"x-bt/img-img"},
		    {name,Name},
		    {?BIP_HDR_IMG_DESCRIPTOR, Descriptor}], Image).

%%
%% Imageing server functions
%%


init(SrvRef, SrvOpts, _Mtu) ->
    ?dbg("init: ~p, opts=~p, peer_mtu=~p", [SrvRef, SrvOpts, _Mtu]),
    #s { sref = SrvRef, opts = SrvOpts}.

terminate(_S) ->
    ?dbg("termiate", []),
    ok.

handle_connect(Target,S) ->
    ?dbg("handle_connect: ~p", [Target]),
    ID = 1235,  %% FIXME
    {reply, {ok,[{connectionID,ID},{who,Target}]}, S}.

handle_disconnect(_Hs, S) ->
    ?dbg("handle_disconnect: ~p", [_Hs]),
    {reply, {ok,success}, S}.

handle_get(_Hs, S) ->
    ?dbg("handle_get: ~p", [_Hs]),
    {reply, {ok,[],<<"Hello">>}, S}.

handle_put(_Hs,<<>>,S) ->
    ?dbg("handle_put: ~p data=~p", [_Hs,<<>>]),
    {reply, {ok,success}, S};
handle_put(_Hs,_Data,S) ->
    ?dbg("handle_put: ~p data=~p", [_Hs,_Data]),
    {reply, continue, S}.

handle_abort(_Hs, S) ->
    ?dbg("handle_abort: ~p", [_Hs]),
    {reply, {ok,success}, S}.

handle_setpath(_Hs, _Args, S) ->
    ?dbg("handle_setpath: ~p args=~p", [_Hs,_Args]),
    {reply, {error,not_allowed}, S}.

handle_command(_Hs, S) ->
    ?dbg("handle_command: ~p", [_Hs]),
    {reply, {error,not_allowed}, S}.






