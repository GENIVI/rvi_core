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
-ifndef(SDP_HRL).
-define(SDP_HRL, true).

%% ATTRIBUTES
-define(ATTR_ServiceRecordHandle,  16#0000).
-define(ATTR_ServiceClassIDList,  16#0001).
-define(ATTR_ServiceRecordState, 16#0002).
-define(ATTR_ServiceID,  16#0003).
-define(ATTR_ProtocolDescriptorList, 16#0004).
-define(ATTR_BrowseGroupList,  16#0005).
-define(ATTR_LanguageBaseAttributeIDList, 16#0006).
-define(ATTR_ServiceInfoTimeToLive, 16#0007).
-define(ATTR_ServiceAvailability, 16#0008).
-define(ATTR_BluetoothProfileDescriptorList, 16#0009).
-define(ATTR_DocumentationURL, 16#000A).
-define(ATTR_ClientExecutableURL, 16#000B).
-define(ATTR_IconURL,  16#000C).
-define(ATTR_AdditionalProtocolsDescriptorList, 16#000D).

%% ATTRIBUTE
-define(ATTR_ServiceName,        16#0000).
-define(ATTR_ServiceDescription, 16#0001).
-define(ATTR_ProviderName,       16#0002).

%% SDP
-define(ATTR_SDP_VersionNumberList,    16#0200).
-define(ATTR_SDP_ServiceDatabaseState, 16#0201).

%% PAN 
-define(ATTR_PAN_IPSubnet, 16#0200).

%% BIP
-define(ATTR_BIP_GoepL2capPsm, 16#0200).
-define(ATTR_BIP_SupportedCapabilities, 16#0310).
-define(ATTR_BIP_SupportedFeatures,	16#0311).
-define(ATTR_BIP_SupportedFunctions,	16#0312).
-define(ATTR_BIP_TotalImagingDataCapacity, 16#0313).

%% AVRCP
-define(ATTR_ACRCP_SupportedFeatures,	16#0311).

%% BPP - basic printing profile, Direct Printing(1119), ReferencePrinting(1119), 
%% DirectPrintingReferenceObjectsService(1120),ReflectedUI(1121),PrintingStatus(1123)

%% UUID Def's

%% PROTOCOLS
-define(UUID_SDP, <<16#0001:16>>).
-define(UUID_UDP, <<16#0002:16>>).
-define(UUID_RFCOMM, <<16#0003:16>>).
-define(UUID_TCP, <<16#0004:16>>).
-define(UUID_TCS_BIN, <<16#0005:16>>).
-define(UUID_TCS_AT, <<16#0006:16>>).
-define(UUID_OBEX, <<16#0008:16>>).
-define(UUID_IP, <<16#0009:16>>).
-define(UUID_FTP, <<16#000A:16>>).
-define(UUID_HTTP, <<16#000C:16>>).
-define(UUID_WSP, <<16#000E:16>>).
-define(UUID_BNEP, <<16#000F:16>>).
-define(UUID_UPNP, <<16#0010:16>>).
-define(UUID_HIDP, <<16#0011:16>>).
-define(UUID_HCRP_CTRL, <<16#0012:16>>).
-define(UUID_HCRP_DATA, <<16#0014:16>>).
-define(UUID_HCRP_NOTE, <<16#0016:16>>).
-define(UUID_AVCTP, <<16#0017:16>>).
-define(UUID_AVDTP, <<16#0019:16>>).
-define(UUID_CMPT,  <<16#001B:16>>).
-define(UUID_UDI,   <<16#001D:16>>).
-define(UUID_MCAP_CTRL, <<16#001E:16>>).
-define(UUID_MCAP_DATA, <<16#001F:16>>).
-define(UUID_L2CAP, <<16#0100:16>>).

%% SERVICE Classes
-define(UUID_ServiceDiscoveryServer, <<16#1000:16>>).
-define(UUID_BrowseGroupDescriptor, <<16#1001:16>>).
-define(UUID_PublicBrowseGroup, <<16#1002:16>>).
-define(UUID_SerialPort, <<16#1101:16>>).
-define(UUID_LANAccessUsingPPP, <<16#1102:16>>).
-define(UUID_DialupNetworking, <<16#1103:16>>).
-define(UUID_IrMCSync, <<16#1104:16>>).
-define(UUID_OBEXObjectPush, <<16#1105:16>>).
-define(UUID_OBEXFileTransfer, <<16#1106:16>>).
-define(UUID_IrMCSyncCommand, <<16#1107:16>>).
-define(UUID_Headset, <<16#1108:16>>).
-define(UUID_CordlessTelephony, <<16#1109:16>>).
-define(UUID_AudioSource, <<16#110A:16>>).
-define(UUID_AudioSink, <<16#110B:16>>).
-define(UUID_AVRemoteControlTarget, <<16#110C:16>>).
-define(UUID_AdvancedAudioDistribution, <<16#110D:16>>).
-define(UUID_AVRemoteControl, <<16#110E:16>>).
-define(UUID_VideoConferencing, <<16#110F:16>>).  %% fixme RemoteControlController?
-define(UUID_Intercom, <<16#1110:16>>).
-define(UUID_Fax, <<16#1111:16>>).
-define(UUID_HeadsetAudioGateway, <<16#1112:16>>).
-define(UUID_WAP, <<16#1113:16>>).
-define(UUID_WAPClient, <<16#1114:16>>).
-define(UUID_PANU, <<16#1115:16>>).
-define(UUID_NAP, <<16#1116:16>>).
-define(UUID_GN, <<16#1117:16>>).
-define(UUID_DirectPrinting, <<16#1118:16>>).
-define(UUID_ReferencePrinting, <<16#1119:16>>).
-define(UUID_Imaging, <<16#111A:16>>).
-define(UUID_ImagingResponder, <<16#111B:16>>).
-define(UUID_ImagingAutomaticArchive, <<16#111C:16>>).
-define(UUID_ImagingReferencedObjects, <<16#111D:16>>).
-define(UUID_Handsfree, <<16#111E:16>>).
-define(UUID_HandsfreeAudioGateway, <<16#111F:16>>).
-define(UUID_DirectPrintingReferenceObjectsService, <<16#1120:16>>).
-define(UUID_ReflectedUI, <<16#1121:16>>).
-define(UUID_BasicPrinting, <<16#1122:16>>).
-define(UUID_PrintingStatus, <<16#1123:16>>).
-define(UUID_HumanInterfaceDeviceService, <<16#1124:16>>).
-define(UUID_HardcopyCableReplacement, <<16#1125:16>>).
-define(UUID_HCR_Print, <<16#1126:16>>).
-define(UUID_HCR_Scan, <<16#1127:16>>).
-define(UUID_CommonISDNAccess, <<16#1128:16>>).
-define(UUID_VideoConferencingGW, <<16#1129:16>>).
-define(UUID_UDI_MT, <<16#112A:16>>).
-define(UUID_UDI_TA, <<16#112B:16>>).
-define(UUID_Audio_Video, <<16#112C:16>>).
-define(UUID_SIM_Access, <<16#112D:16>>).
-define(UUID_PhonebookAccess_PCE, <<16#112E:16>>). %% Phonebook Access Profile
-define(UUID_PhonebookAccess_PSE, <<16#112F:16>>). %%Phonebook Access Profile
-define(UUID_PhonebookAccess, <<16#1130:16>>). %% Phonebook Access Profile
-define(UUID_Headset_HS, <<16#1131:16>>).           %% Headset Profile (HSP)
-define(UUID_Message_Access_Server, <<16#1132>>).   %%	Message Access Profile (MAP)
-define(UUID_Message_Notification_Server, <<16#1133>>).	%% Message Access Profile (MAP)
-define(UUID_Message_Access_Profile, <<16#1134>>).  %% Message Access Profile (MAP)
-define(UUID_GNSS, <<16#1135>>).  %% Global Navigation Satellite System Profile (GNSS)
-define(UUID_GNSS_Server, <<16#1136>>). %%	Global Navigation Satellite System Profile (GNSS)
-define(UUID_3D_Display, <<16#1137>>).	%% 3D Synchronization Profile (3DSP)
-define(UUID_3D_Glasses, <<16#1138>>).	 %% 3D Synchronization Profile (3DSP)
-define(UUID_3D_Synchronization, <<16#1139>>). %% 3D Synchronization Profile (3DSP)
-define(UUID_MPS_Profile, <<16#113A>>). %% Multi-Profile Specification (MPS)
-define(UUID_MPS_SC, <<16#113B>>). %% Multi-Profile Specification (MPS)
-define(UUID_CTN_Access_Service, <<16#113C>>). %% Calendar, Task, and Notes (CTN)
-define(UUID_CTN_Notification_Service, <<16#113D>>). %%	Calendar Tasks and Notes (CTN)
-define(UUID_CTN_Profile, <<16#113E>>). %% Calendar Tasks and Notes (CTN) Profile
-define(UUID_PnPInformation, <<16#1200:16>>).
-define(UUID_GenericNetworking, <<16#1201:16>>).
-define(UUID_GenericFileTransfer, <<16#1202:16>>).
-define(UUID_GenericAudio, <<16#1203:16>>).
-define(UUID_GenericTelephony, <<16#1204:16>>).
-define(UUID_UPNP_Service, <<16#1205:16>>).
-define(UUID_UPNP_IP_Service, <<16#1206:16>>).
-define(UUID_ESDP_UPNP_IP_PAN, <<16#1300:16>>).
-define(UUID_ESDP_UPNP_IP_LAP, <<16#1301:16>>).
-define(UUID_ESDP_UPNP_L2CAP, <<16#1302:16>>).
-define(UUID_VideoSource, <<16#1303:16>>).
-define(UUID_VideoSink, <<16#1304:16>>).
-define(UUID_VideoDistribution, <<16#1305:16>>).
-define(UUID_HDP, <<16#1400:16>>).  %% Health Device Profile
-define(UUID_HDP_Source, <<16#1401:16>>).  %% Health Device Profile (HDP)
-define(UUID_HDP_Sink, <<16#1402:16>>).  %% Health Device Profile (HDP)

-define(UUID_SyncMLServer, 
	<<16#00000001:32,16#0000:16,16#1000:16,16#8000:16,16#0002EE000002:48>>).
-define(UUID_SyncMLClient,
	<<16#00000002:32,16#0000:16,16#1000:16,16#8000:16,16#0002EE000002:48>>).
-define(UUID_SyncMLDMServer, 
	<<16#00000003:32,16#0000:16,16#1000:16,16#8000:16,16#0002EE000002:48>>).
-define(UUID_SyncMLDMClient, 
	<<16#00000004:32,16#0000:16,16#1000:16,16#8000:16,16#0002EE000002:48>>).

%% NOKIA
-define(UUID_NokiaSyncMLServer,
	?UUID(16#00005601,16#0000,16#1000,16#8000,16#0002EE000001)).
-define(UUID_NokiaObexPcSuiteServices,
	?UUID(16#00005005,16#0000,16#1000,16#8000,16#0002EE000001)).

%% 00005557-0000-1000-8000-0002EE000001    What is this? E60!

%% SONY-ERICSSON	
-define(UUID_Z520a_SyncMLClient,
	<<16#00001111:32,16#0000:16,16#0000:16,16#0000:16,16#000222000000:48>>).
	

-define(LANGUAGE(L1,L2), (((L1) bsl 8) + (L2))).

-define(ENCODING_UTF8, 106).

%%
%% SDP Request/Response codes
%%
-define(SDP_ErrorResponse,                  16#01).
-define(SDP_ServiceSearchRequest,           16#02).
-define(SDP_ServiceSearchResponse,          16#03).
-define(SDP_ServiceAttributeRequest,        16#04).
-define(SDP_ServiceAttributeResponse,       16#05).
-define(SDP_ServiceSearchAttributeRequest,  16#06).
-define(SDP_ServiceSearchAttributeResponse, 16#07).

-define(SDP_ErrorInvalidVersion,        16#0001).
-define(SDP_ErrorInvalidHandle,         16#0002).
-define(SDP_ErrorInvalidSyntax,         16#0003).
-define(SDP_ErrorInvaludPduSize,        16#0004).
-define(SDP_ErrorInvalidContinuation,   16#0005).
-define(SDP_ErrorInsufficientResources, 16#0006).


-record(sdpErrorResponse,
	{
	  errorCode,
	  errorInfo
	 }).

-record(sdpServiceSearchRequest,
	{ 
	  serviceSearchPattern,
	  maximumServiceRecordCount
	 }).

-record(sdpServiceSearchResponse,
	{ 
	  totalServiceRecordCount,
	  currentServiceRecordCount,
	  serviceRecordHandleList
	 }).

-record(sdpServiceAttributeRequest,
	{ 
	  serviceRecordHandle,
	  maximumAttributeByteCount,
	  attributeIDList
	 }).

-record(sdpServiceAttributeResponse,
	{ 
	  attributeListByteCount,
	  attributeList
	 }).

-record(sdpServiceSearchAttributeRequest,
	{ 
	  serviceSearchPattern,
	  maximumAttributeByteCount,
	  attributeIDList
	 }).

-record(sdpServiceSearchAttributeResponse,
	{ 	  
	  attributeListByteCount,
	  attributeList
	 }).

-endif.
