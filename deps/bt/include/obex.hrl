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

-ifndef(OBEX_HRL).
-define(OBEX_HRL, true).

-define(OBEX_VERSION,           16#10).

%% 0x - Request codes
-define(OBEX_CMD_CONNECT,       16#00).
-define(OBEX_CMD_DISCONNECT,    16#01).
-define(OBEX_CMD_PUT,           16#02).
-define(OBEX_CMD_GET,           16#03).
-define(OBEX_CMD_COMMAND,       16#04).
-define(OBEX_CMD_SETPATH,       16#05).

%% 1x
-define(OBEX_RSP_CONTINUE,              16#10).
-define(OBEX_RSP_SWITCH_PRO,            16#11).

%% 2x
-define(OBEX_RSP_SUCCESS,               16#20).
-define(OBEX_RSP_CREATED,               16#21).
-define(OBEX_RSP_ACCEPTED,              16#22).
-define(OBEX_RSP_NON_AUTHORITATIVE,     16#23).
-define(OBEX_RSP_NO_CONTENT,            16#24).
-define(OBEX_RSP_RESET_CONTENT,         16#25).
-define(OBEX_RSP_PARTIAL_CONTENT,       16#26).

%% 3x
-define(OBEX_RSP_MULTIPLE_CHOICES,      16#30).
-define(OBEX_RSP_MOVED_PERMANENTLY,     16#31).
-define(OBEX_RSP_MOVED_TEMPORARILY,     16#32).
-define(OBEX_RSP_SEE_OTHER,             16#33).
-define(OBEX_RSP_NOT_MODIFIED,          16#34).
-define(OBEX_RSP_USE_PROXY,             16#35).

%% 4x
-define(OBEX_RSP_BAD_REQUEST,           16#40).
-define(OBEX_RSP_UNAUTHORIZED,          16#41).
-define(OBEX_RSP_PAYMENT_REQUIRED,      16#42).
-define(OBEX_RSP_FORBIDDEN,             16#43).
-define(OBEX_RSP_NOT_FOUND,             16#44).
-define(OBEX_RSP_METHOD_NOT_ALLOWED,    16#45).
-define(OBEX_RSP_NOT_ACCEPTABLE,        16#46).
-define(OBEX_RSP_PROXY_AUTH,            16#47).
-define(OBEX_RSP_REQUEST_TIMEOUT,       16#48).
-define(OBEX_RSP_CONFLICT,              16#49).
-define(OBEX_RSP_GONE,                  16#4A).
-define(OBEX_RSP_LENGTH_REQUIRED,       16#4B).
-define(OBEX_RSP_PRECONDITION_FAILED,   16#4C).
-define(OBEX_RSP_ENTITY_TOO_LARGE,      16#4D).
-define(OBEX_RSP_URL_TOO_LARGE,         16#4E).
-define(OBEX_RSP_UNSUPPORTED_MEDIA,     16#4F).

%% 5x
-define(OBEX_RSP_INTERNAL_SERVER_ERROR, 16#50).
-define(OBEX_RSP_NOT_IMPLEMENTED,       16#51).
-define(OBEX_RSP_BAD_GATEWAY,           16#52).
-define(OBEX_RSP_SERVICE_UNAVAILABLE,   16#53).
-define(OBEX_RSP_GATEWAY_TIMEOUT,       16#54).
-define(OBEX_RSP_HTTP_VERSION_NOT_SUPPORTED, 16#55).

%% 6x
-define(OBEX_RSP_DATABASE_FULL,         16#60).
-define(OBEX_RSP_DATABASE_LOCKED,       16#61).

%% 7x - command/response
-define(OBEX_CMD_ABORT,                 16#7f).
	
-define(TARGET_FOLDER_BROWSING,
	?UUID(16#F9EC7BC4,16#953C,16#11D2,16#984E,16#525400DC9E09)).
-define(TARGET_SYNCML_DM, "SYNCML-DM").
-define(TARGET_SYNCML_SYNC, "SYNCML-SYNC").

%% NOKIA SYNCML-SYNC ????
-define(UUID_SYNCML_SYNC,
	?UUID(16#00005601,16#0000,16#1000,16#8000,16#0002EE000002)).

%% Min, Max and default transport MTU
-define(OBEX_DEFAULT_MTU,       1024).
-define(OBEX_MINIMUM_MTU,       255).
-define(OBEX_MAXIMUM_MTU,       32768).

-define(OBEX_HDR_MASK, 16#C0).
-define(OBEX_V0,    2#00).
-define(OBEX_Vn,    2#01).
-define(OBEX_I1,    2#10).
-define(OBEX_I4,    2#11).

%% Standard headers

%% null terminated values
-define(OBEX_HDR_NAME,          16#01).  %% Name of the object.
-define(OBEX_HDR_DESCRIPTION,   16#05). %% Description of object 

-define(OBEX_HDR_RESERVED_RANGE_START,    16#10).
-define(OBEX_HDR_RESERVED_RANGE_END,      16#2F).

-define(OBEX_HDR_USER_DEFINED_RANGE_START, 16#30).
-define(OBEX_HDR_USER_DEFINED_RANGE_END,   16#3F).

%% bytes sequence values
-define(OBEX_HDR_TYPE,          16#42). %% Type of the object 
-define(OBEX_HDR_TIME,          16#44). %% Last modification time of (ISO8601) 
-define(OBEX_HDR_TARGET,        16#46). %% Identifies the target for the object 
-define(OBEX_HDR_HTTP,          16#47). %% Data part of the object 
-define(OBEX_HDR_BODY,          16#48). %% Data part of the object 
-define(OBEX_HDR_BODY_END,      16#49). %% Last data part of the object 
-define(OBEX_HDR_WHO,           16#4a). %% Identifies the sender of the object 
-define(OBEX_HDR_APPARAM,       16#4c). %% Application parameters 
-define(OBEX_HDR_AUTHCHAL,      16#4d). %% Authentication challenge 
-define(OBEX_HDR_AUTHRESP,      16#4e). %% Authentication response 
-define(OBEX_HDR_OBJCLASS,      16#4f). %% OBEX Object class of object 

%% 4 bytes values
-define(OBEX_HDR_COUNT,         16#c0). %% Number of objects (used by connect) 
-define(OBEX_HDR_CONNECTION_ID, 16#cb). %% Connection identifier 
-define(OBEX_HDR_LENGTH,        16#c3). %% Total lenght of object 
-define(OBEX_HDR_TIME2,         16#C4). %% Deprecated use HDR_TIME instead 

%% vCard charsets
-define(OBEX_CHARSET_STRING_ISO88591,	"CHARSET=ISO-8859-1").
-define(OBEX_CHARSET_STRING_UTF8,	"UTF-8").

%% vEvent encodings
-define(OBEX_ENCODING_STRING_QUOTED_PRINTABLE, "QUOTED-PRINTABLE").
-define(OBEX_ENCODING_STRING_BASE64, "BASE-64").
-define(OBEX_ENCODING_STRING_8BIT, "8BIT").


-endif.
