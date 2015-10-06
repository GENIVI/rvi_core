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
	 sign_message/2,
	 validate_message/3,
	 validate_authorization/4,
	 authorize_local_message/3,
	 authorize_remote_message/3]).
-export([filter_by_service/3]).

%% for testing & development
-export([sign/1]).
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
    {Priv, Pub} = KeyPair = authorize_keys:get_key_pair(),
    ?debug("KeyPair = ~p~n", [KeyPair]),
    {ok, #st { cs = rvi_common:get_component_specification(),
	       private_key = Priv,
	       public_key = Pub} }.

start_json_server() ->
    ?debug("authorize_rpc:start_json_server(): called"),
    rvi_common:start_json_rpc_server(authorize, ?MODULE, authorize_sup),
    ok.

sign_message(CompSpec, Message) ->
    ?debug("authorize_rpc:sign_message()~n", []),
    rvi_common:request(authorize, ?MODULE, sign_message,
		       [{message, Message}], [status, jwt], CompSpec).

validate_message(CompSpec, JWT, Conn) ->
    ?debug("authorize_rpc:validate_message()~n", []),
    rvi_common:request(authorize, ?MODULE, validate_message,
		       [{jwt, JWT},
			{conn, Conn}], [status, message], CompSpec).

get_authorize_jwt(CompSpec) ->
    ?debug("authorize_rpc:get_authorize_jwt()~n", []),
    rvi_common:request(authorize, ?MODULE, get_authorize_jwt,
		       [], [status, jwt], CompSpec).

get_certificates(CompSpec) ->
    ?debug("authorize_rpc:get_certificates()~n", []),
    rvi_common:request(authorize, ?MODULE, get_certificates,
		       [], [status, certs], CompSpec).

validate_authorization(CompSpec, JWT, Certs, Conn) ->
    ?debug("authorize_rpc:validate_authorization():"
	   " Conn = ~p~n", [Conn]),
    rvi_common:request(authorize, ?MODULE, validate_authorization,
		       [{jwt, JWT},
			{certs, Certs},
			{conn, Conn}],
		       [status], CompSpec).

authorize_local_message(CompSpec, Service, Params) ->
    ?debug("authorize_rpc:authorize_local_msg(): params:    ~p ~n", [Params]),
    rvi_common:request(authorize, ?MODULE, authorize_local_message,
		       [{service, Service},
			{parameters, Params}],
		       [status, signature], CompSpec).

authorize_remote_message(CompSpec, Service, Params) ->
    ?debug("authorize_rpc:authorize_remote_msg(): service: ~p ~n", [Service]),
    ?debug("authorize_rpc:authorize_remote_msg(): parameters: ~p ~n", [Params]),
    rvi_common:request(authorize, ?MODULE,authorize_remote_message,
		       [{service, Service},
			{parameters, Params}],
		       [status], CompSpec).

filter_by_service(CompSpec, Services, Conn) ->
    ?debug("authorize_rpc:filter_by_service(): services: ~p ~n", [Services]),
    ?debug("authorize_rpc:filter_by_service(): conn: ~p ~n", [Conn]),
    rvi_common:request(authorize, ?MODULE, filter_by_service,
		       [{ services, Services },
			{ conn, Conn }],
		       [status, services], CompSpec).

%% For testing while developing cert functionality
sign(Term) ->
    %% Use private key of authorize_rpc to make a JWT token
    gen_server:call(?SERVER, {sign, Term}).

public_key() ->
    gen_server:call(?SERVER, public_key).

public_key_json() ->
    gen_server:call(?SERVER, public_key_json).

private_key() ->
    gen_server:call(?SERVER, private_key).

%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc("sign_message", Args) ->
    {ok, Message} = rvi_common:get_json_element(["message"], Args),
    [ Status, JWT ] =
	gen_server:call(?SERVER, { rvi, sign_message, [Message] }),
    ?debug("Message signature = ~p~n", [JWT]),
    {ok, [ {status, rvi_common:json_rpc_status(Status)},
	   {jwt, JWT} ]};
handle_rpc("validate_message", Args) ->
    ?debug("validate_message; Args = ~p~n", [Args]),
    {ok, JWT} = rvi_common:get_json_element(["jwt"], Args),
    {ok, Conn} = rvi_common:get_json_element(["conn"], Args),
    [ Status, Msg ] =
	gen_server:call(?SERVER, { rvi, validate_message, [JWT, Conn] }),
    {ok, [ {status, rvi_common:json_rpc_status(Status)},
	   {message, Msg} ]};
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
    {ok, Certs} = rvi_common:get_json_element(["certs"], Args),
    {ok, Conn} = rvi_common:get_json_element(["connection"], Args),
    [ Status | Rem ] =
	gen_server:call(?SERVER, { rvi, validate_authorization,
				   [JWT, Certs, Conn] }),
    {ok, [ rvi_common:json_rpc_status(Status) | Rem] };
handle_rpc("authorize_local_message", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Params} = rvi_common:get_json_element(["parameters"], Args),
    [ Status | Rem ] =
	gen_server:call(?SERVER, { rvi, authorize_local_message,
				   [Service, Params]}),

    { ok, [ rvi_common:json_rpc_status(Status) | Rem] };


handle_rpc("authorize_remote_message", Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Params} = rvi_common:get_json_element(["parameters"], Args),
    [ Status ]  = gen_server:call(?SERVER, { rvi, authorize_remote_message,
					     [Service, Params]}),
    { ok, rvi_common:json_rpc_status(Status)};

handle_rpc("filter_by_service", Args) ->
    ?debug("authorize_rpc:handle_rpc(\"filter_by_service\", ~p)~n", [Args]),
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, Conn} = rvi_common:get_json_element(["conn"], Args),
    [ Status, FilteredServices ] =
	gen_server:call(?SERVER, { rvi, filter_by_service,
				   [Services, Conn] }),
    {ok, [{status, rvi_common:json_rpc_status(Status)},
	  {services, FilteredServices}]};

handle_rpc(Other, _Args) ->
    ?debug("authorize_rpc:handle_rpc(~p): unknown", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


handle_notification(Other, _Args) ->
    ?debug("authorize_rpc:handle_other(~p): unknown", [ Other ]),
    ok.

%%
%% Genserver implementation
%%
handle_call({rvi, sign_message, [Msg]}, _, #st{private_key = Key} = State) ->
    {reply, [ ok, authorize_sig:encode_jwt(Msg, Key) ], State};
handle_call({rvi, validate_message, [JWT, Conn]}, _, State) ->
    try  {reply, [ok, authorize_keys:validate_message(JWT, Conn)], State}
    catch
	error:_Err ->
	 {reply, [not_found], State}
    end;
handle_call({rvi, get_authorize_jwt, []}, _From, State) ->
    {reply, [ ok, authorize_keys:authorize_jwt() ], State};

handle_call({rvi, get_certificates, []}, _From, State) ->
    {reply, [ ok, authorize_keys:get_certificates() ], State};

handle_call({rvi, validate_authorization, [JWT, Certs, Conn] }, _From, State) ->
    %% The authorize JWT contains the public key used to sign the cert
    ?debug(
       "authorize_rpc:handle_call({rvi, validate_authorization, [_,_,_]})~n",
       []),
    try authorize_sig:decode_jwt(JWT, authorize_keys:provisioning_key()) of
	{_Header, Keys} ->
	    store_certs(Certs, Keys, JWT, Conn),
	    {reply, [ok], State};
	invalid ->
	    ?warning("Invalid auth JWT from ~p~n", [Conn]),
	    {reply, [not_found], State}
    catch
	error:_Err ->
	    ?warning("Auth validation exception: ~p~n", [_Err]),
	    {reply, [not_found], State}
    end;

handle_call({rvi, authorize_local_message, [Service, Params] } = R, _From,
	    #st{private_key = Key} = State) ->
    ?debug("authorize_rpc:handle_call(~p)~n", [R]),
    case authorize_keys:find_cert_by_service(Service) of
	{ok, Cert} ->
	    Msg = Params ++ [{<<"certificate">>, Cert}],
	    ?debug("authorize_rpc:authorize_local_message~nMsg = ~p~n", [Msg]),
	    Sig = authorize_sig:encode_jwt(Msg, Key),
	    {reply, [ok, Sig], State};
	_ ->
	    {reply, [ not_found ], State}
    end;

handle_call({rvi, authorize_remote_message, [_Service, Params]},
	    _From, State) ->
    IP = proplists:get_value(remote_ip, Params),
    Port = proplists:get_value(remote_port, Params),
    Timeout = proplists:get_value(timeout, Params),
    SvcName = proplists:get_value(service_name, Params),
    Parameters = proplists:get_value(parameters, Params),
    Signature = proplists:get_value(signature, Params),
    ?debug("authorize_rpc:authorize_remote_message(): remote_ip:     ~p~n", [IP]),
    ?debug("authorize_rpc:authorize_remote_message(): remote_port:   ~p~n", [Port]),
    ?debug("authorize_rpc:authorize_remote_message(): timeout:       ~p~n", [Timeout]),
    ?debug("authorize_rpc:authorize_remote_message(): service_name:  ~p~n", [SvcName]),
    ?debug("authorize_rpc:authorize_remote_message(): parameters:    ~p~n", [Parameters]),
    ?debug("authorize_rpc:authorize_remote_message(): signature:     ~40s~n", [Signature]),
    case authorize_keys:validate_message(
	   iolist_to_binary(Signature), {IP, Port}) of
	invalid ->
	    {reply, [ not_found ], State};
	Msg ->
	    {ok, Timeout1} = rvi_common:get_json_element(["timeout"], Msg),
	    {ok, SvcName1} = rvi_common:get_json_element(["service_name"], Msg),
	    {ok, Params1} = rvi_common:get_json_element(["parameters"], Msg),
	    ?debug("authorize_rpc:authorize_remote_message(): timeout1:      ~p~n", [Timeout1]),
	    ?debug("authorize_rpc:authorize_remote_message(): service_name1: ~p~n", [SvcName1]),
	    ?debug("authorize_rpc:authorize_remote_message(): parameters1:   ~p~n", [Params1]),

	    if Timeout =:= Timeout1,
	       SvcName =:= SvcName1,
	       Parameters =:= Params1 ->
		    ?debug("Remote message authorized.~n", []),
		    {reply, [ ok ], State};
	       true ->
		    ?debug("Remote message NOT authorized.~n", []),
		    {reply, [ not_found ], State}
	    end
    end;

handle_call({rvi, filter_by_service, [Services, Conn]}, _From, State) ->
    Filtered = authorize_keys:filter_by_service(Services, Conn),
    {reply, [ok, Filtered], State};

handle_call({sign, Term}, _From, #st{private_key = Key} = State) ->
    {reply, authorize_sig:encode_jwt(Term, Key), State};

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

store_certs(Certs, Keys, JWT, Conn) ->
    ?debug("Storing ~p certs for conn ~p~n", [length(Certs), Conn]),
    KeyStructs = get_json_element(["keys"], Keys, []),
    authorize_keys:save_keys(KeyStructs, Conn),
    ?debug("KeyStructs = ~p~n", [KeyStructs]),
    lists:foreach(fun(Cert) ->
			  store_cert(Cert, KeyStructs, JWT, Conn)
		  end, Certs).

get_json_element(Path, JSON, Default) ->
    case rvi_common:get_json_element(Path, JSON) of
	{ok, Value} ->
	    Value;
	_ ->
	    Default
    end.

store_cert(Cert, Keys, JWT, Conn) ->
    case authorize_sig:decode_jwt(Cert, authorize_keys:provisioning_key()) of
	{_CHeader, CertStruct} ->
	    authorize_keys:save_keys(Keys, Conn),
	    case authorize_keys:save_cert(CertStruct, JWT, Conn) of
		ok ->
		    ok;
		{error, Reason} ->
		    ?warning(
		       "Couldn't store certificate from ~p: ~p~n",
		       [Conn, Reason]),
		    ok
	    end;
	invalid ->
	    ?warning("Invalid certificate from ~p~n", [Conn]),
	    ok
    end.
