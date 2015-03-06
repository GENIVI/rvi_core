%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(authorize_rpc).
-behaviour(gen_server).

-export([handle_rpc/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([init_rvi_component/0]).
-include_lib("lager/include/log.hrl").

-define(SERVER, ?MODULE). 
-record(st, { 
	  next_transaction_id = 1, %% Sequentially incremented transaction id.
	  services_tid = undefined %% Known services.
	 }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?debug("authorize_rpc:init(): called."),
    {ok, #st {}}.

init_rvi_component() ->
    ?debug("authorize_rpc:init_rvi_component(): called"),
    case rvi_common:get_component_config(authorize, exo_http_opts) of
	{ ok, ExoHttpOpts } ->
	    exoport_exo_http:instance(authorize_sup, 
				      authorize_rpc,
				      ExoHttpOpts),
	    ok;
	
	_ -> 	
	    ?info("authorize_rpc:init_rvi_component(): exo_http_opts not specified. Gen Server only"),
	    ok
    end.


%% Retrieve certificate. 
%% Certificate will be passed to exo_json:encode() in order
%% to be translated to JSON.
get_certificate_body(_ServiceName) ->
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

authorize_local_message(ServiceName) ->
    ?debug("authorize_rpc:authorize_local_msg(): service_name:    ~p ~n", [ServiceName]),
    {ok, 
     [ 
       { status, rvi_common:json_rpc_status(ok)},
       { signature, "fixme_add_signature" },
%%       { certificate, get_certificate_body(ServiceName) }
       { certificate, "certificate"  }
     ]}.

authorize_remote_message(ServiceName, Signature, Certificate) ->
    ?debug("authorize_rpc:authorize_remote_msg(): service_name: ~p ~n", [ServiceName]),
    ?debug("authorize_rpc:authorize_remote_msg(): signature:    ~p ~n", [Signature]),
    ?debug("authorize_rpc:authorize_remote_msg(): certificate:  ~p ~n", [Certificate]),
    {ok, 
     [ 
       { status, rvi_common:json_rpc_status(ok) },
       { signature, Signature },
       { certificate, Certificate }
     ]}.

%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc("authorize_local_message", Args) ->
    {ok, ServiceName} = rvi_common:get_json_element(["service_name"], Args),
    authorize_local_message(ServiceName);

handle_rpc("authorize_remote_message", Args) ->
    {ok, ServiceName} = rvi_common:get_json_element(["service_name"], Args),
    {ok, Signature} = rvi_common:get_json_element(["signature"], Args),
    {ok, Certificate} = rvi_common:get_json_element(["certificate"], Args),
    authorize_remote_message(ServiceName , Signature, Certificate);

handle_rpc(Other, _Args) ->
    ?debug("authorize_rpc:handle_rpc(~p): unknown", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.



%%
%% Genserver implementation
%%
handle_call({rvi_call, authorize_local_message, Args}, _From, State) ->
    {_, ServiceName} = lists:keyfind(service_name, 1, Args),
    ?info("authorize_rpc:authorize_local_message(gen_server):  args:            ~p", [ Args]),
    ?info("authorize_rpc:authorize_local_message(gen_server):  service name:    ~p", [ ServiceName]),
    {reply, authorize_local_message(ServiceName), State};

handle_call({rvi_call, authorize_remote_message, Args}, _From, State) ->
    {_, ServiceName} = lists:keyfind(service_name, 1, Args),
    {_, Signature} = lists:keyfind(signature, 1, Args),
    {_, Certificate} = lists:keyfind(certificate, 1, Args),
    {reply, authorize_remote_message(ServiceName, Signature, Certificate), State};

handle_call(Other, _From, State) ->
    ?warning("authorize_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ]}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

