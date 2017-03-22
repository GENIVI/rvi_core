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
-export([get_credentials/1,
	 sign_message/2,
	 validate_message/3,
	 store_creds/3,
	 store_creds/4,
	 remove_connection/2,
	 authorize_local_message/3,
	 authorize_remote_message/3]).
-export([filter_by_service/3]).

%% for service_discovery notifications
-export([service_available/3,
	 service_unavailable/3]).

%% for testing & development
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
    {Priv, Pub} =  authorize_keys:get_device_key(),
    ?debug("KeyPair = {~s, ~s}~n", [authorize_keys:pp_key(Priv),
				    authorize_keys:pp_key(Pub)]),
    CS = rvi_common:get_component_specification(),
    service_discovery_rpc:subscribe(CS, ?MODULE),
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

get_credentials(CompSpec) ->
    ?debug("authorize_rpc:get_credentials()~n", []),
    rvi_common:request(authorize, ?MODULE, get_credentials,
		       [], [status, creds], CompSpec).

remove_connection(CompSpec, Conn) ->
    rvi_common:notification(authorize, ?MODULE, remove_connection,
			    [{conn, Conn}], CompSpec).

store_creds(CompSpec, Creds, Conn) ->
    store_creds(CompSpec, Creds, Conn, undefined).

store_creds(CompSpec, Creds, Conn, PeerCert) ->
    ?debug("API: store_creds(), PeerCert = ~p", [authorize_keys:abbrev(PeerCert)]),
    rvi_common:request(authorize, ?MODULE, store_creds,
		       [{creds, Creds},
			{conn, Conn},
			{peer_cert, PeerCert}],
		       [status], CompSpec).

authorize_local_message(CompSpec, Service, Params) ->
    ?debug("authorize_rpc:authorize_local_msg(): params:    ~p ~n", [Params]),
    rvi_common:request(authorize, ?MODULE, authorize_local_message,
		       [{service, Service},
			{parameters, Params}],
		       [status], CompSpec).

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

service_available(CS, SvcName, _DLMod) ->
    rvi_common:notification(authorize, ?MODULE, service_available,
			    [{service, SvcName}], CS).

service_unavailable(CS, SvcName, _DLMod) ->
    rvi_common:notification(authorize, ?MODULE, service_unavailable,
			    [{service, SvcName}], CS).

public_key() ->
    gen_server:call(?SERVER, public_key).

public_key_json() ->
    gen_server:call(?SERVER, public_key_json).

private_key() ->
    gen_server:call(?SERVER, private_key).

%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc(<<"sign_message">>, Args) ->
    {ok, Message} = rvi_common:get_json_element(["message"], Args),
    LogId = rvi_common:get_json_log_id(Args),
    [ Status, JWT ] =
	gen_server:call(?SERVER, { rvi, sign_message, [Message, LogId] }),
    ?debug("Message signature = ~p~n", [JWT]),
    {ok, [ {status, rvi_common:json_rpc_status(Status)},
	   {jwt, JWT} ]};
handle_rpc(<<"validate_message">>, Args) ->
    ?debug("validate_message; Args = ~p~n", [Args]),
    {ok, JWT} = rvi_common:get_json_element(["jwt"], Args),
    {ok, Conn} = rvi_common:get_json_element(["conn"], Args),
    LogId = rvi_common:get_json_log_id(Args),
    [ Status, Msg ] =
	gen_server:call(?SERVER, { rvi, validate_message, [JWT, Conn, LogId] }),
    {ok, [ {status, rvi_common:json_rpc_status(Status)},
	   {message, Msg} ]};
handle_rpc(<<"get_credentials">>, Args) ->
    LogId = rvi_common:get_json_log_id(Args),
    [ Status | Rem ] =
	gen_server:call(?SERVER, { rvi, get_credentials, [LogId] }),
    {ok, [ rvi_common:json_rpc_status(Status) | Rem ] };
handle_rpc(<<"store_creds">>, Args) ->
    {ok, Creds} = rvi_common:get_json_element(["creds"], Args),
    {ok, Conn} = rvi_common:get_json_element(["conn"], Args),
    {ok, PeerCert} = rvi_common:get_json_element(["peer_cert"], Args),
    LogId = rvi_common:get_json_log_id(Args),
    [ Status | Rem ] =
	gen_server:call(?SERVER, {rvi, store_creds, [Creds, Conn, PeerCert, LogId]}),
    {ok, [ rvi_common:json_rpc_status(Status) | Rem]};
handle_rpc(<<"authorize_local_message">>, Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Params} = rvi_common:get_json_element(["parameters"], Args),
    LogId = rvi_common:get_json_log_id(Args),
    [ Status | Rem ] =
	gen_server:call(?SERVER, { rvi, authorize_local_message,
				   [Service, Params, LogId]}),

    { ok, [ rvi_common:json_rpc_status(Status) | Rem] };


handle_rpc(<<"authorize_remote_message">>, Args) ->
    {ok, Service} = rvi_common:get_json_element(["service"], Args),
    {ok, Params} = rvi_common:get_json_element(["parameters"], Args),
    LogId = rvi_common:get_json_log_id(Args),
    [ Status ]  = gen_server:call(?SERVER, { rvi, authorize_remote_message,
					     [Service, Params, LogId]}),
    { ok, rvi_common:json_rpc_status(Status)};

handle_rpc(<<"filter_by_service">>, Args) ->
    ?debug("authorize_rpc:handle_rpc(\"filter_by_service\", ~p)~n", [Args]),
    {ok, Services} = rvi_common:get_json_element(["services"], Args),
    {ok, Conn} = rvi_common:get_json_element(["conn"], Args),
    LogId = rvi_common:get_json_log_id(Args),
    [ Status, FilteredServices ] =
	gen_server:call(?SERVER, { rvi, filter_by_service,
				   [Services, Conn, LogId] }),
    {ok, [{status, rvi_common:json_rpc_status(Status)},
	  {services, FilteredServices}]};

handle_rpc(Other, _Args) ->
    ?debug("authorize_rpc:handle_rpc(~p): unknown", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


handle_notification(<<"service_available">>, Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    gen_server:cast(?SERVER, {service_available, SvcName}),
    ok;
handle_notification(<<"service_unavailable">>, Args) ->
    {ok, SvcName} = rvi_common:get_json_element(["service"], Args),
    gen_server:cast(?SERVER, {service_unavailable, SvcName}),
    ok;
handle_notification(<<"remove_connection">>, Args) ->
    {ok, Conn} = rvi_common:get_json_element(["conn"], Args),
    gen_server:cast(?SERVER, {remove_connection, Conn}, Args),
    ok;
handle_notification(Other, _Args) ->
    ?debug("authorize_rpc:handle_other(~p): unknown", [ Other ]),
    ok.

%%
%% Gen_server implementation
%%
handle_call({rvi, sign_message, [Msg | LogId]}, _, #st{private_key = Key} = State) ->
    Sign = authorize_sig:encode_jwt(Msg, Key),
    log(LogId, result, "signed", []),
    {reply, [ ok, Sign ], State};
handle_call({rvi, validate_message, [JWT, Conn | LogId]}, _, State) ->
    try  begin Res = authorize_keys:validate_message(JWT, Conn),
	       log(LogId, result, "validated", []),
	       {reply, [ok, Res], State}
	 end
    catch
	error:_Err ->
	    log(LogId, error, "validation FAILED", []),
	    {reply, [not_found], State}
    end;
handle_call({rvi, get_credentials, _Args}, _From, State) ->
    {reply, [ ok, authorize_keys:get_credentials() ], State};

handle_call({rvi, store_creds, [Creds, Conn, PeerCert | LogId]}, _From, State) ->
    do_store_creds(Creds, Conn, PeerCert, LogId, State#st.cs),
    {reply, [ok], State};

handle_call({rvi, authorize_local_message, [Service, _Params | LogId] } = R, _From, State) ->
    ?debug("authorize_rpc:handle_call(~p)~n", [R]),
    case authorize_keys:validate_service_call(Service, local) of
	invalid ->
	    log(LogId, error, "local msg REJECTED", []),
	    {reply, [ not_found ], State};
	{ok, Id} ->
	    log(LogId, result, "local msg allowed: Cred=~s", [Id]),
	    {reply, [ok], State}
    end;

handle_call({rvi, authorize_remote_message, [_Service, Params | LogId]},
	    _From, State) ->
    IP = proplists:get_value(remote_ip, Params),
    Port = proplists:get_value(remote_port, Params),
    Timeout = proplists:get_value(timeout, Params),
    SvcName = proplists:get_value(service_name, Params),
    Parameters = proplists:get_value(parameters, Params),
    ?debug("authorize_rpc:authorize_remote_message(): remote_ip:     ~p~n", [IP]),
    ?debug("authorize_rpc:authorize_remote_message(): remote_port:   ~p~n", [Port]),
    ?debug("authorize_rpc:authorize_remote_message(): timeout:       ~p~n", [Timeout]),
    ?debug("authorize_rpc:authorize_remote_message(): service_name:  ~p~n", [SvcName]),
    ?debug("authorize_rpc:authorize_remote_message(): parameters:    ~p~n", [Parameters]),
    case authorize_keys:validate_service_call(SvcName, {IP, Port}) of
	invalid ->
	    log(LogId, error, "remote msg REJECTED", []),
	    {reply, [ not_found ], State};
	{ok, CredID} ->
	    ?debug("validated Cred ID=~p", [CredID]),
	    log(LogId, result, "remote msg allowed: Cred=~s", [CredID]),
	    {reply, [ok], State}
    end;

handle_call({rvi, filter_by_service, [Services, Conn | _LogId]}, _From, State) ->
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

handle_cast({rvi, service_available, Svc}, State) ->
    authorize_keys:cache_authorizations(Svc),
    {noreply, State};
handle_cast({rvi, service_unavailable, Svc}, State) ->
    authorize_keys:remove_cached_authorizations(Svc),
    {noreply, State};
handle_cast({rvi, remove_connection, Conn}, State) ->
    authorize_keys:remove_connection(Conn),
    {noreply, State};
handle_cast(Other, State) ->
    ?warning("authorize_rpc:handle_cast(~p): unknown", [ Other ]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_store_creds(Creds, Conn, PeerCert, LogId, CS) ->
    ?debug("Storing ~p creds for conn ~p~nPeerCert = ~w",
	   [length(Creds), Conn, authorize_keys:abbrev(PeerCert)]),
    authorize_keys:remove_cached_authorizations_for_conn(Conn),
    authorize_keys:remove_creds_for_conn(Conn),
    lists:foreach(fun(Cred) ->
			  store_cred(Cred, Conn, PeerCert, LogId)
		  end, Creds),
    authorize_keys:update_authorization_cache(Conn, CS).

store_cred(CredJWT, Conn, PeerCert, LogId) ->
    case authorize_sig:decode_jwt(authorize_keys:strip_nl(CredJWT), authorize_keys:provisioning_key()) of
	{_CHeader, CredStruct} ->
	    case authorize_keys:save_cred(CredStruct, CredJWT, Conn, PeerCert, LogId) of
		ok ->
		    ok;
		{error, Reason} ->
		    ?warning(
		       "Couldn't store credential from ~p: ~p~n",
		       [Conn, Reason]),
		    ok
	    end;
	invalid ->
	    log(LogId, warning, "credential INVALID (~p)", [Conn]),
	    ok
    end.

log([ID], Lvl, Fmt, Args) ->
    rvi_log:log(ID, Lvl, <<"authorize">>, rvi_log:format(Fmt, Args));
log(_, _, _, _) ->
    ok.
