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

%%
%% Some none-guard macros
%%
-ifndef(BT_HRL).
-define(BT_HRL, true).

-include("uuid.hrl").

%% Bluetooth base uuid = ?BT_UUID(0)
-define(BT_UUID32(N),
	?UUID(N,16#0000,16#1000,16#8000,16#00805f9b34fb)).

%% Speical UUID match for 16 bit BT uuid
-define(BT_UUID16(N),
     <<16#0000:16,N:16,16#0000:16,16#1000:16,16#8000:16,16#00805f9b34fb:48>>).


-define(is_bt_address(A),
	is_tuple((A)), size((A)) == 6,
	((element(1,(A)) bor element(2,(A)) bor  element(3,(A)) bor
	  element(4,(A)) bor element(5,(A)) bor  element(6,(A))) band
	 -16#100 == 0)).

-define(never, {{0,0,0},{0,0,0}}).

-define(L2CAP_PSM_None,		  16#0000).

%% Range < 0x1000 reserved.
-define(L2CAP_PSM_ReservedStart,    16#0001).
-define(L2CAP_PSM_ReservedEnd,	  16#1000).

-define(L2CAP_PSM_SDP,             16#0001).
-define(L2CAP_PSM_RFCOMM,           16#0003).
-define(L2CAP_PSM_TCS_BIN,          16#0005).  %% Telephony Control Specifictation / TCS Binary

-define(L2CAP_PSM_TCS_BIN_Cordless, 16#0007).  %% Telephony Control Specifictation / TCS Binary
-define(L2CAP_PSM_BNEP,             16#000F).  %% Bluetooth Network Encapsulation Protocol
-define(L2CAP_PSM_HIDControl,       16#0011).  %% HID profile - control interface
-define(L2CAP_PSM_HIDInterrupt,     16#0013).  %% HID profile - interrupt interface
-define(L2CAP_PSM_AVCTP,            16#0017).  %% Audio/Video Control Transport Protocol
-define(L2CAP_PSM_AVDTP,            16#0019).  %% Audio/Video Distribution Transport Protocol
-define(L2CAP_PSM_UID_C_Plane,      16#001D).  %% Unrestricted Digital Information Profile (UDI)
	
%% Range 0x1001-0xFFFF dynamically assigned.
-define(L2CAP_PSM_DynamicStart,     16#1001).
-define(L2CAP_PSM_DynamicEnd,	    16#FFFF).
    
-endif.

	 
