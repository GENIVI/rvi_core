%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(authorize_rpc).

-export([handle_rpc/2]).
-export([init/0]).

-include_lib("lager/include/log.hrl").

init() ->
    ?debug("authorize_rpc:init(): called"),
    case rvi_common:get_component_config(authorize, exo_http_opts) of
	{ ok, ExoHttpOpts } ->
	    exoport_exo_http:instance(authorize_sup, 
				      authorize_rpc,
				      ExoHttpOpts);
	Err -> Err
    end.


%% Retrieve certificate. 
%% Certificate will be passed to exo_json:encode() in order
%% to be translated to JSON.
get_certificate_body(_CallingService, _TargetService) ->
    {struct, 
     [
      %% Topic tree patterns that this node is authorized to
      %% process requests for.
      { "sources", 
	{ array, [ 
		   "jaguarlandrover.com/cloud/media_server" 
		 ]
	}
      },
      %% Services that can be accessed by the source service.
      { destinations, 
	{ array, [
		  "rpc:jaguarlandrover.com/vin/+/services/media_player"
		 ]
	}

      },
      %% Public key for source.
      %% Used to validate signature of requests, etc.
      { public_key, 
	{ struct, [
		   { algorithm, "some_algorithm" },
		   { key, "some_public_key" }
		  ]
	}
      },
      %% Period during which certificate is valid. UTC
      { validity,  
	{ struct, [
		   { start, 1401918299 },
		   { stop, 1402000000 }
		  ]
	}
      },
      %% A system wide unique id for the certificate
      { id, "b674546e-76ae-4204-b551-3f850fbffb4b" },

      %% UTC timestamp of when the certificate was created.
      { create_timestamp, 1403825201 },

      %% Signed by provisioning server.
      %% All nodes have provisioning server's public key.
      %% Signature covers all data in claims element.
      { signature, 
	{ struct, [ 
		    { algorithm, "signature_algorithm" },
		    { signature, "signature" } 
		  ]
	}
      }
     ]
    }.

authorize_local_message(Target, CallingService) ->
    ?debug("authorize_rpc:authorize_local_msg(): target: ~p ~n", [Target]),
    ?debug("authorize_rpc:authorize_local_msg(): calling_service: ~p ~n", [CallingService]),
    {ok, 
     [ 
       { status, rvi_common:json_rpc_status(ok)},
       { signature, "fixme_add_signature" },
%%       { certificate, get_certificate_body(CallingService, Target) }
       { certificate, "certificate"  }
     ]}.

authorize_remote_message(Target, Signature, Certificate) ->
    ?debug("authorize_rpc:authorize_remote_msg(): target: ~p ~n", [Target]),
    ?debug("authorize_rpc:authorize_remote_msg(): signature: ~p ~n", [Signature]),
    ?debug("authorize_rpc:authorize_remote_msg(): certificate: ~p ~n", [Certificate]),
    {ok, 
     [ 
       { status, rvi_common:json_rpc_status(ok) },
       { signature, Signature },
       { certificate, Certificate }
     ]}.

%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc("authorize_local_message", Args) ->
    {ok, Target} = rvi_common:get_json_element(["target"], Args),
    {ok, CallingService} = rvi_common:get_json_element(["calling_service"], Args),
    authorize_local_message(Target, CallingService);

handle_rpc("authorize_remote_message", Args) ->
    {ok, Target} = rvi_common:get_json_element(["target"], Args),
    {ok, Signature} = rvi_common:get_json_element(["signature"], Args),
    {ok, Certificate} = rvi_common:get_json_element(["certificate"], Args),
    authorize_remote_message(Target, Signature, Certificate);

handle_rpc(Other, _Args) ->
    ?debug("authorize_rpc:handle_rpc(~p): unknown~n", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.
