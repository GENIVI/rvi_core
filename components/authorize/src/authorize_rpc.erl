%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(authorize_rpc).
-behaviour(gen_server).

-export([handle_rpc/2,
	 handle_notification/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_json_server/0]).
-export([get_authorize_jwt/1,
	 get_certificates/1,
	 validate_authorization/4,
	 authorize_local_message/2,
	 authorize_remote_message/4]).

%% for testing & development
-export([sign/1, sign_default_cert/0]).
-export([public_key/0, public_key_json/0,
	 private_key/0]).

-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").

-define(SERVER, ?MODULE). 
-record(st, { 
	  next_transaction_id = 1, %% Sequentially incremented transaction id.
	  services_tid = undefined, %% Known services.
	  cs = #component_spec{},
	  private_key = undefined,
	  public_key = undefined
	 }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?debug("authorize_rpc:init(): called."),
    {Priv, Pub} = authorize_keys:get_key_pair(),
    {ok, #st { cs = rvi_common:get_component_specification(),
	       private_key = Priv,
	       public_key = Pub} }.

start_json_server() ->
    ?debug("authorize_rpc:start_json_server(): called"),
    rvi_common:start_json_rpc_server(authorize, ?MODULE, authorize_sup),
    ok.


%% Retrieve certificate. 
%% Certificate will be passed to exo_json:encode() in order
%% to be translated to JSON.
get_certificate_body(_Service) ->
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

get_authorize_jwt(CompSpec) ->
    ?debug("authorize_rpc:get_authorize_jwt()~n", []),
    rvi_common:request(authorize, ?MODULE, get_authorize_jwt,
		       [], [status, jwt], CompSpec).
    
get_certificates(CompSpec) ->
    ?debug("authorize_rpc:get_certificates()~n", []),
    rvi_common:request(authorize, ?MODULE, get_certificates,
		       [], [status, certs], CompSpec).
    
validate_authorization(CompSpec, JWT, Cert, Conn) ->
    ?debug("authorize_rpc:validate_authorization():"
	   " Conn = ~p~n", [Conn]),
    rvi_common:request(authorize, ?MODULE, validate_authorization,
		       [{jwt, JWT},
			{cert, Cert},
			{conn, Conn}],
		       [status], CompSpec).

authorize_local_message(CompSpec, Service) ->
    ?debug("authorize_rpc:authorize_local_msg(): service:    ~p ~n", [Service]),
    rvi_common:request(authorize, ?MODULE,authorize_local_message, 
		       [{ service, Service }],
		       [status, signature, certificate], CompSpec).
    


authorize_remote_message(CompSpec, Service, Signature, Certificate) ->
    ?debug("authorize_rpc:authorize_remote_msg(): service: ~p ~n", [Service]),
    ?debug("authorize_rpc:authorize_remote_msg(): signature:    ~p ~n", [Signature]),
    ?debug("authorize_rpc:authorize_remote_msg(): certificate:  ~p ~n", [Certificate]),
    rvi_common:request(authorize, ?MODULE,authorize_remote_message, 
		       [{ service, Service},
			{ signature, Signature },
			{ certificate, Certificate }],
		       [status], CompSpec).


%% For testing while developing cert functionality
sign(Term) ->
    %% Use private key of authorize_rpc to make a JWT token
    gen_server:call(?SERVER, {sign, Term}).

sign_default_cert() ->
    gen_server:call(?SERVER, sign_default_cert).

public_key() ->
    gen_server:call(?SERVER, public_key).

public_key_json() ->
    gen_server:call(?SERVER, public_key_json).

private_key() ->
    gen_server:call(?SERVER, private_key).

%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc("get_authorize_jwt", []) ->
    [ Status | Rem ] =
	gen_server:call(?SERVER, { rvi, get_authorize_jwt, [] }),
    {ok, [ rvi_common:json_rpc_status(Status) | Rem ] };
handle_rpc("get_certificates", []) ->
    [ Status | Rem ] =
	gen_server:call(?SERVER, { rvi, get_certificates, [] }),
    {ok, [ rvi_common:json_rpc_status(Status) | Rem ] };
handle_rpc("validate_authorization", Args) ->
    {ok, JWT} = rvi_common:get_json_element(["jwt"], Args),
    {ok, Cert} = rvi_common:get_json_element(["cert"], Args),
    {ok, Conn} = rvi_common:get_json_element(["connection"], Args),
    [ Status | Rem ] =
	gen_server:call(?SERVER, { rvi, validate_authorization,
				   [JWT, Cert, Conn] }),
    {ok, [ rvi_common:json_rpc_status(Status) | Rem] };
handle_rpc("authorize_local_message", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    [ Status | Rem ] = 
	gen_server:call(?SERVER, { rvi, authorize_local_message, 
				   [Service]}),

    { ok, [ rvi_common:json_rpc_status(Status) | Rem] };


handle_rpc("authorize_remote_message", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Signature} = rvi_common:get_json_element(["signature"], Args),
    {ok, Certificate} = rvi_common:get_json_element(["certificate"], Args),
    [ Status ]  = gen_server:call(?SERVER, { rvi, authorize_remote_message, 
					     [Service, Signature, Certificate]}),
    { ok, rvi_common:json_rpc_status(Status)};

handle_rpc(Other, _Args) ->
    ?debug("authorize_rpc:handle_rpc(~p): unknown", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


handle_notification(Other, _Args) ->
    ?debug("authorize_rpc:handle_other(~p): unknown", [ Other ]),
    ok.

%%
%% Genserver implementation
%%
handle_call({rvi, get_authorize_jwt, []}, _From, State) ->
    {reply, [ ok, authorize_keys:authorize_jwt() ], State};

handle_call({rvi, get_certificates, []}, _From, State) ->
    {reply, [ ok, authorize_keys:certificates() ], State};

handle_call({rvi, validate_authorization, [JWT, Cert, Conn] }, _From, State) ->
    %% The authorize JWT contains the public key used to sign the cert
    try authorize_sig:decode_jwt(JWT, authorize_keys:provisioning_key()) of
	{_Header, Keys} ->
	    case authorize_sig:decode_jwt(Cert, Keys) of
		{_CHeader, CertStruct} ->
		    authorize_keys:save_keys(Keys, Conn),
		    authorize_keys:save_cert(CertStruct, Conn),
		    {reply, [ok], State};
		invalid ->
		    ?warning("Invalid certificate from ~p~n", [Conn]),
		    {reply, [not_found], State}
	    end;
	invalid ->
	    ?warning("Invalid auth JWT from ~p~n", [Conn]),
	    {reply, [not_found], State}
    catch
	error:_Err ->
	    ?warning("Auth validation exception: ~p~n", [_Err]),
	    {reply, [not_found], State}
    end;
	
handle_call({rvi, authorize_local_message, [_Service] }, _From, State) ->
    {reply, [ ok, "signature", "certificate" ], State};

handle_call({rvi, authorize_remote_message, 
	     [_Service, _Signature, _Certificate]},
	     _From, State) ->

    %% FIXME: Implement
    {reply, [ ok ], State};

handle_call({sign, Term}, _From, #st{private_key = Key} = State) ->
    {reply, authorize_sig:encode_jwt(Term, Key), State};

handle_call(sign_default_cert, _From, #st{private_key = Key} = State) ->
    {reply, authorize_sig:encode_jwt(get_certificate_body(default), Key), State};

handle_call(public_key, _From, #st{public_key = Key} = State) ->
    {reply, Key, State};

handle_call(public_key_json, _From, #st{public_key = Key} = State) ->
    {reply, authorize_keys:public_key_to_json(Key), State};

handle_call(private_key, _From, #st{private_key = Key} = State) ->
    {reply, Key, State};

handle_call(Other, _From, State) ->
    ?warning("authorize_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, unknown_command, State}.

handle_cast(Other, State) ->
    ?warning("authorize_rpc:handle_cast(~p): unknown", [ Other ]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
