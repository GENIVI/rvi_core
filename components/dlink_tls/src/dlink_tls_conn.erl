%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.home>
%%% @copyright (C) 2014, magnus
%%% @doc
%%%
%%% @end
%%% Created : 12 Sep 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(dlink_tls_conn).

-behaviour(gen_server).
-include_lib("lager/include/log.hrl").
-include_lib("public_key/include/public_key.hrl").

%% API

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([setup/7]).
-export([upgrade/2,
         async_upgrade/2]).
-export([send/2]).
-export([send/3]).
-export([send_data/2]).
-export([is_connection_up/1]).
-export([is_connection_up/2]).
-export([terminate_connection/1]).
-export([terminate_connection/2]).
-export([publish_node_id/3]).


-define(SERVER, ?MODULE).
-define(PACKET_MOD, dlink_data_msgpack).

-record(st, {
	  ip = {0,0,0,0},
	  port = 0,
	  sock = undefined,
	  mode = tcp :: tcp | tls,
          packet_mod = ?PACKET_MOD,
          packet_st = [],
          frag_opts = [],
	  mod = undefined,
	  func = undefined,
          cs,
          role = server :: client | server
	 }).

%%%===================================================================
%%% API
%%%===================================================================
%% MFA is to deliver data received on the socket.

setup(Role, IP, Port, Sock, Mod, Fun, CompSpec) when Role==client;
                                                     Role==server ->
    Params = {Role, IP, Port, Sock, Mod, Fun, CompSpec},
    ?debug("setup() IP = ~p; Port = ~p; Mod = ~p; Fun = ~p", [IP, Port, Mod, Fun]),
    ?debug("CompSpec = ~p", [CompSpec]),
    case gen_server:start_link(?MODULE, Params ,[]) of
	{ ok, GenSrvPid } = Res ->
	    gen_tcp:controlling_process(Sock, GenSrvPid),
	    %% gen_server:cast(GenSrvPid, {activate_socket, Sock}),
	    Res;

	Err ->
	    Err
    end.

upgrade(Pid, Role) when Role==client; Role==server ->
    gen_server:call(Pid, {upgrade, Role}).

async_upgrade(Pid, Role) when Role==client;
                              Role==server ->
    gen_server:cast(Pid, {upgrade, Role}).

send(Pid, Data) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Data}).

send(Pid, Data, Opts) when is_pid(Pid) ->
    gen_server:cast(Pid, {send, Data, Opts});
send(IP, Port, Data) ->
    case dlink_tls_connmgr:find_connection_by_address(IP, Port) of
	{ok, Pid} ->
	    gen_server:cast(Pid, {send, Data});

	_Err ->
	    ?info("connection:send(): Connection ~p:~p not found for data: ~p",
		  [ IP, Port, Data]),
	    not_found

    end.

send_data(Pid, Data) ->
    gen_server:cast(Pid, {send_data, Data}).

terminate_connection(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, terminate_connection).

terminate_connection(IP, Port) ->
    case dlink_tls_connmgr:find_connection_by_address(IP, Port) of
	{ok, Pid} ->
	    gen_server:call(Pid, terminate_connection);

	_Err -> not_found
    end.


is_connection_up(Pid) when is_pid(Pid) ->
    is_process_alive(Pid).

is_connection_up(IP, Port) ->
    case dlink_tls_connmgr:find_connection_by_address(IP, Port) of
	{ok, Pid} ->
	    is_connection_up(Pid);

	_Err ->
	    false
    end.

publish_node_id(FromPid, NodeId, Cs) ->
    gen_server:cast(FromPid, {publish_node_id, NodeId, Cs}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
%% MFA used to handle socket closed, socket error and received data
%% When data is received, a separate process is spawned to handle
%% the MFA invocation.
init({Role, IP, Port, Sock, Mod, Fun, CompSpec}) ->
    case IP of
	undefined -> ok;
	_ -> dlink_tls_connmgr:add_connection(IP, Port, self())
    end,
    ?debug("connection:init(): self():   ~p", [self()]),
    ?debug("connection:init(): IP:       ~p", [IP]),
    ?debug("connection:init(): Port:     ~p", [Port]),
    ?debug("connection:init(): Sock:     ~p", [Sock]),
    ?debug("connection:init(): Module:   ~p", [Mod]),
    ?debug("connection:init(): Function: ~p", [Fun]),
    {ok, PktMod} = get_module_config(packet_mod, ?PACKET_MOD, CompSpec),
    ?debug("packet_mod = ~p", [PktMod]),
    PktSt = PktMod:init(CompSpec),
    {ok, FragOpts} = get_module_config(
                       frag_opts, [{packet_mod, {PktMod, PktSt}}], CompSpec),
    {ok, #st{
	    ip = IP,
	    port = Port,
	    sock = Sock,
	    mod = Mod,
            packet_mod = PktMod,
            packet_st = PktSt,
            frag_opts = FragOpts,
	    func = Fun,
            cs = rvi_common:set_value(role, Role, CompSpec)
	   }}.

get_module_config(Key, Default, CS) ->
    ModConf = fun() ->
                      rvi_common:get_module_config(
                        data_link, dlink_tls_rpc, Key, Default, CS)
              end,
    case rvi_common:get_value(tls_opts, undefined, CS) of
        undefined -> ModConf();
        Opts ->
            case lists:keyfind(Key, 1, Opts) of
                false ->
                    ModConf();
                {_, Val} ->
                    Val
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------


handle_call(terminate_connection, _From,  #st{} = St) ->
    ?debug("~p:handle_call(terminate_connection): Terminating: ~p",
	   [ ?MODULE, {St#st.ip, St#st.port}]),

    {stop, Reason, NSt} = handle_info({tcp_closed, St#st.sock}, St),
    {stop, Reason, ok, NSt};
handle_call({upgrade, Role} = Req, _From, #st{cs = CS} = St) ->
    ?debug("~p:handle_call(~p)~n", [?MODULE, Req]),
    %% deliberately crash (for now) if upgrade fails.
    {Reply, #st{} = St1} = handle_upgrade(Role, CS, St),
    {reply, Reply, St1};
handle_call(_Request, _From, #st{} = State) ->
    ?warning("~p:handle_call(): Unknown call: ~p", [ ?MODULE, _Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({upgrade, Role}, #st{cs = CS} = St) ->
    {_, #st{} = St1} = handle_upgrade(Role, CS, St),
    {noreply, St1};
handle_cast({send, Data},  #st{packet_mod = PMod, packet_st = PSt} = St) ->
    ?debug("~p:handle_cast(send): Sending: ~p",
	   [ ?MODULE, abbrev(Data)]),
    {ok, Encoded, PSt1} = PMod:encode(Data, PSt),
    ?debug("Encoded~n~s", [Encoded]),
    case St#st.mode of
	tcp -> gen_tcp:send(St#st.sock, Encoded);
	tls -> ssl:send(St#st.sock, Encoded)
    end,
    {noreply, St#st{packet_st = PSt1}};
handle_cast({send, Data, Opts} = Req, #st{packet_mod = PMod,
                                          packet_st = PSt,
                                          frag_opts = FragOpts} = St) ->
    ?debug("handle_cast(~p, ...), FragOpts = ~p", [Req, FragOpts]),
    {ok, Bin, PSt1} = PMod:encode(Data, PSt),
    St1 = St#st{packet_st = PSt1},
    rvi_frag:send(Bin, Opts ++ FragOpts, ?MODULE, fun() ->
                                                          do_send(Bin, St1)
                                                  end),
    {noreply, St1};
handle_cast({send_data, Data}, #st{} = St) ->
    %% don't encode; just send
    ?debug("send_data, ~w", [authorize_keys:abbrev_bin(Data)]),
    do_send(Data, St),
    {noreply, St};
handle_cast({activate_socket, Sock}, #st{} = State) ->
    Res = inet:setopts(Sock, [{active, once}]),
    ?debug("connection:activate_socket(): ~p", [Res]),
    {noreply, State};
handle_cast({publish_node_id, NodeId, Cs}, #st{} = St) ->
    ?debug("publish_node_id (~p, ~p)", [NodeId]),
    %% Do this from the connection process, so that schedule_rpc can
    %% monitor the connection and unpublish when it goes away.
    schedule_rpc:publish_node_id(Cs, NodeId, dlink_tls_rpc),
    {noreply, St};

handle_cast(_Msg, #st{} = State) ->
    ?warning("~p:handle_cast(): Unknown cast: ~p~nSt=~p", [ ?MODULE, _Msg, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% Fill in peername if empty.
handle_info({tcp, Sock, Data},
	    #st { ip = undefined } = St) ->
    {ok, {IP, Port}} = inet:peername(Sock),
    NSt = St#st { ip = inet_parse:ntoa(IP), port = Port },
    handle_info({tcp, Sock, Data}, NSt);

handle_info({ssl, Sock, Data}, #st{ip = IP, port = Port,
                                   packet_mod = PMod, packet_st = PSt} = State) ->
    ?debug("handle_info(data): Data: ~p", [abbrev(Data)]),
    ?debug("handle_info(data): From: ~p:~p ", [ IP, Port]),
    ?debug("handle_info(data): PMod: ~p", [PMod]),
    case PMod:decode(Data, fun(Elems) ->
                                   handle_elems(Elems, State)
                           end, PSt) of
        {ok, PSt1} ->
            ssl:setopts(Sock, [{active, once}]),
            {noreply, State#st{packet_st = PSt1}};
        {error, Reason} ->
            {stop, Reason, State}
    end;
handle_info({tcp, Sock, Data},
	    #st { ip = IP,
		  port = Port,
                  packet_mod = PMod,
                  packet_st = PSt} = State) ->
    ?debug("handle_info(data): Data: ~p", [Data]),
    ?debug("handle_info(data): From: ~p:~p ", [IP, Port]),

    case PMod:decode(Data, fun(Elems) ->
                                   handle_elems(Elems, State)
                           end, PSt) of
        {ok, PSt1} ->
            inet:setopts(Sock, [{active, once}]),
            {noreply, State#st{packet_st = PSt1}};
        {error, Reason} ->
            ?debug("decode failed, Reason = ~p", [Reason]),
            {stop, Reason, State}
    end;

handle_info({Evt, Sock},
	    #st { ip = IP,
		  port = Port,
		  mod = Mod,
		  func = Fun,
                  cs = CS} = State) when Evt==tcp_closed; Evt==ssl_closed ->
    ?debug("~p:handle_info(~w): Address: ~p:~p ", [ ?MODULE, Evt, IP, Port]),
    Mod:Fun(self(), IP, Port,closed, CS),
    case Evt of
        tcp_closed -> gen_tcp:close(Sock);
        ssl_closed -> ssl:close(Sock)
    end,
    dlink_tls_connmgr:delete_connection_by_pid(self()),
    {stop, normal, State};

handle_info({tcp_error, _Sock},
	    #st { ip = IP,
		  port = Port,
		  mod = Mod,
		  func = Fun,
                  cs = CS} = State) ->

    ?debug("~p:handle_info(tcp_error): Address: ~p:~p ", [ ?MODULE, IP, Port]),
    Mod:Fun(self(), IP, Port, error, CS),
    dlink_tls_connmgr:delete_connection_by_pid(self()),
    {stop, normal, State};


handle_info(_Info, #st{} = State) ->
    ?warning("~p:handle_cast(): Unknown info: ~p", [ ?MODULE, _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ?debug("~p:terminate(): Reason: ~p ", [ ?MODULE, _Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_send(Bin, #st{sock = Sock, mode = tcp}) ->
    gen_tcp:send(Sock, Bin);
do_send(Bin, #st{sock = Sock, mode = tls}) ->
    ssl:send(Sock, Bin).

handle_upgrade(Role, CompSpec, #st{sock = S} = St) ->
    inet:setopts(S, [{active, false}]),
    case do_upgrade(S, Role, CompSpec) of
	{DoVerify, {ok, NewS}} ->
	    ?debug("upgrade to TLS succcessful~n", []),
            ssl:setopts(NewS, [{active, once}]),
            {ok, {IP, Port}} = ssl:peername(NewS),
            PeerCert = get_peercert(DoVerify, NewS),
            ?debug("SSL PeerCert=~w", [abbrev(PeerCert)]),
            NewCS = rvi_common:set_value(
                      dlink_tls_role, Role,
                      rvi_common:set_value(dlink_tls_peer_cert, PeerCert, CompSpec)),
	    {ok, St#st{sock = NewS, mode = tls, role = Role,
                       ip = inet_parse:ntoa(IP), port = Port,
                       cs = NewCS}};
	{_, Error} ->
	    ?error("Cannot upgrade to TLS: ~p~n", [Error]),
            error({cannot_upgrade, Error})
    end.

get_peercert(DoVerify, S) ->
    case ssl:peercert(S) of
        {ok, PeerCert} ->
            PeerCert;
        {error, _} when DoVerify == false ->
            undefined
    end.

do_upgrade(Sock, client, CompSpec) ->
    {DoVerify, Opts} = tls_opts(client, CompSpec),
    ?debug("TLS Opts = ~p", [Opts]),
    {DoVerify, ssl:connect(Sock, Opts)};
do_upgrade(Sock, server, CompSpec) ->
    {DoVerify, Opts} = tls_opts(server, CompSpec),
    ?debug("TLS Opts = ~p", [Opts]),
    {DoVerify, ssl:ssl_accept(Sock, Opts)}.

tls_opts(Role, CompSpec) ->
    {ok, ServerOpts} = get_module_config(server_opts, [], CompSpec),
    TlsOpts0 = proplists:get_value(tls_opts, ServerOpts, []),
    TlsOpts = TlsOpts0 ++
        [{reuse_sessions, false}
         || not lists:keymember(reuse_sessions, 1, TlsOpts0)],
    ?debug("TlsOpts = ~p", [TlsOpts]),
    Opt = fun(K) -> opt(K, TlsOpts,
                        fun() ->
                                ok(setup:get_env(rvi_core, K))
                        end)
          end,
    case VOpt = lists:keyfind(verify, 1, TlsOpts) of
        {verify, false} when Role == server ->
            {false, [
                     {verify, verify_none},
                     {certfile, Opt(device_cert)},
                     {keyfile, Opt(device_key)},
                     {cacertfile, Opt(root_cert)}
                     | other_tls_opts(TlsOpts)]};
        {verify, false} ->
            {false, [
                     {verify, verify_none}
                     | other_tls_opts(TlsOpts)]};
        _ when VOpt==false; VOpt == {verify, true} ->  % {verify,true} default
            {true, [
                    {verify, verify_peer},
                    {certfile, Opt(device_cert)},
                    {keyfile, Opt(device_key)},
                    {cacertfile, Opt(root_cert)},
                    {verify_fun, opt(verify_fun, TlsOpts,
                                     {fun verify_fun/3, public_root_key()})},
                    {partial_chain, opt(partial_chain, TlsOpts,
                                        fun(X) ->
                                                partial_chain(Role, X)
                                        end)}
                    | other_tls_opts(TlsOpts)
                   ]}
    end.

other_tls_opts(Opts) ->
    other_tls_opts([device_cert, device_key,
                    root_cert, verify_fun,
                    partial_chain, verify], Opts).

other_tls_opts(Remove, Opts) ->
    [O || {K,_} = O <- Opts,
          not lists:member(K, Remove)].

opt(Key, Opts, Def) ->
    case lists:keyfind(Key, 1, Opts) of
        false when is_function(Def, 0) -> Def();
        false  -> Def;
        {_, V} -> V
    end.

ok({ok, V}) ->
    V;
ok(Other) ->
    error({badmatch, Other}).

public_root_key() ->
    authorize_keys:provisioning_key().

verify_fun(Cert, What, St) ->
    ?debug("verify_fun(~p, ~p, ~p)", [abbrev(Cert), What, abbrev(St)]),
    verify_fun_(Cert, What, St).

verify_fun_(Cert, {bad_cert, selfsigned_peer}, PubKey) ->
    ?debug("Verify self-signed cert: ~p", [abbrev(Cert)]),
    try verify_cert_sig(Cert, PubKey) of
        true ->
            ?debug("verified!", []),
            {valid, PubKey};
        false ->
            ?debug("verification FAILED", []),
            {bad_cert, invalid_signature}
    catch
        error:Error ->
            ?debug("Caught error:~p~n~p", [Error, erlang:get_stacktrace()]),
            {fail, PubKey}
    end;
verify_fun_(_, {bad_cert, Reason}, St) ->
    ?debug("Bad cert: ~p", [Reason]),
    {fail, St};
verify_fun_(_, {extension, _}, St) ->
    {unknown, St};
verify_fun_(_, valid, St) ->
    {valid, St};
verify_fun_(_, valid_peer, St) ->
    {valid_peer, St}.

partial_chain(_, Certs) ->
    ?debug("partial_chain() invoked, length(Certs) = ~w", [length(Certs)]),
    Decoded = (catch [public_key:der_decode('Certificate', C)
                      || C <- Certs]),
    ?debug("partial_chain: ~p", [[lager:pr(Dec) || Dec <- Decoded]]),
    {trusted_ca, hd(Certs)}.

handle_elems(Elements, #st{frag_opts = FragOpts} = St) ->
    MaybeF = rvi_frag:maybe_fragment(Elements, ?MODULE, FragOpts),
    ?debug("maybe_fragment(~p) -> ~p", [Elements, MaybeF]),
    case MaybeF of
        true ->
            %% It was a fragment, but not a complete message yet
            St;
        {true, Msg} ->
            #st{packet_mod = PMod, packet_st = PSt} = St,
            PMod:decode(Msg, fun(Elems) ->
                                     got_msg(Elems, St)
                             end, PSt);
        false ->
            got_msg(Elements, St)
    end.

got_msg(Elements, #st{ip = IP, port = Port, mod = Mod, func = Fun, cs = CS} = St) ->
    ?debug("handle_info(data complete): Processed: ~p", [abbrev(Elements)]),
    Mod:Fun(self(), IP, Port, data, Elements, CS),
    St.

verify_cert_sig(#'OTPCertificate'{tbsCertificate = TBS,
				  signature = Sig}, PubKey) ->
    DER = public_key:pkix_encode('OTPTBSCertificate', TBS, otp),
    {SignType, _} = signature_algorithm(TBS),
    public_key:verify(DER, SignType, Sig, PubKey).

signature_algorithm(#'OTPCertificate'{tbsCertificate = TBS}) ->
    signature_algorithm(TBS);
signature_algorithm(#'OTPTBSCertificate'{
		       signature = #'SignatureAlgorithm'{
				      algorithm = Algo}}) ->
    public_key:pkix_sign_types(Algo).


abbrev(T) ->
    authorize_keys:abbrev(T).
