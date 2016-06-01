%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

%% Commonly used protocol identifiers across dlink implementations
%%
-define(DLINK_CMD_AUTHORIZE,        <<"au">>).
-define(DLINK_CMD_CRED_EXCHANGE,    <<"cre">>).
-define(DLINK_CMD_SERVICE_ANNOUNCE, <<"sa">>).
-define(DLINK_CMD_RECEIVE,          <<"rcv">>).
-define(DLINK_CMD_FRAG,             <<"frg">>).
-define(DLINK_CMD_PING,             <<"ping">>).

-define(DLINK_ARG_CMD,              <<"cmd">>).
-define(DLINK_ARG_TRANSACTION_ID,   <<"tid">>).
-define(DLINK_ARG_ADDRESS,          <<"addr">>).
-define(DLINK_ARG_NODE_ID,          <<"id">>).
-define(DLINK_ARG_PORT,             <<"port">>).
-define(DLINK_ARG_VERSION,          <<"ver">>).
-define(DLINK_ARG_CREDENTIAL,       <<"cred">>).
-define(DLINK_ARG_CREDENTIALS,      <<"creds">>).
-define(DLINK_ARG_SIGNATURE,        <<"sign">>).
-define(DLINK_ARG_SERVICES,         <<"svcs">>).
-define(DLINK_ARG_MODULE,           <<"mod">>).
-define(DLINK_ARG_AVAILABLE,        <<"av">>).
-define(DLINK_ARG_UNAVAILABLE,      <<"un">>).
-define(DLINK_ARG_STATUS,           <<"stat">>).
-define(DLINK_ARG_DATA,             <<"data">>).
