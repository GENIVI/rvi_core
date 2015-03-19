%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

%% A record defining the modules to use 
%% Used by rvi_common:send_component_request() to
%% figure out how to route an intra-component call

-define(COMP_SPEC_TYPE, list({ module(), gen_server | json_rpc, list()})).

-record(component_spec, {
	  service_edge :: ?COMP_SPEC_TYPE,
	  scheduler :: ?COMP_SPEC_TYPE,
	  service_discovery :: ?COMP_SPEC_TYPE,
	  authorize :: ?COMP_SPEC_TYPE, 
	  data_link :: ?COMP_SPEC_TYPE, 
	  protocol :: ?COMP_SPEC_TYPE 
	 }).

-define(COMP_SPEC_SERVICE_EDGE_DEFAULT,      [ { service_edge_rpc, gen_server, [] } ]).
-define(COMP_SPEC_SCHEDULER_DEFAULT,         [ { scheduler_rpc, gen_server, [] } ]).
-define(COMP_SPEC_SERVICE_DISCOVERY_DEFAULT, [ { service_discovery_rpc, gen_server, [] } ]).
-define(COMP_SPEC_AUTHORIZE_DEFAULT,         [ { authorize_rpc, gen_server, [] }]).
-define(COMP_SPEC_DATA_LINK_DEFAULT,         [ { data_link_bert_rpc, gen_server, [] } ]).
-define(COMP_SPEC_PROTOCOL_DEFAULT,          [ { protocol, gen_server, [] } ]).

