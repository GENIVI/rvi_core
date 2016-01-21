%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%% Copyright (C) 2015, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(proto_msgpack_rpc).
-behaviour(gen_server).

-export([handle_rpc/2,
         handle_notification/2]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").

-define(SERVER, ?MODULE).
-export([start_json_server/0]).
-export([send_message/8,
         receive_message/3]).

-record(st, {
          %% Component specification
          queue = [],
          cs = #component_spec{},
	  pack_opts = [{allow_atom, pack},
		       {enable_str, true},
		       jsx]
          }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?debug("init(): called."),
    {ok, #st{ cs = rvi_common:get_component_specification() }}.

start_json_server() ->
    rvi_common:start_json_rpc_server(protocol, ?MODULE, proto_msgpack_sup).



send_message(CompSpec,
             TID,
             ServiceName,
             Timeout,
             ProtoOpts,
             DataLinkMod,
             DataLinkOpts,
             Parameters) ->
    rvi_common:request(protocol, ?MODULE, send_message,
                       [{ transaction_id, TID },
                        { service, ServiceName },
                        { timeout, Timeout },
                        { protocol_opts, ProtoOpts },
                        { data_link_mod, DataLinkMod },
                        { data_link_opts, DataLinkOpts },
                        { parameters, Parameters }],
                       [ status ], CompSpec).

receive_message(CompSpec, {IP, Port}, Data) ->
    rvi_common:notification(protocol, ?MODULE, receive_message,
                            [ {data, Data },
                              {remote_ip, IP},
                              {remote_port, Port} ],
                            CompSpec).

%% JSON-RPC entry point

%% CAlled by local exo http server
handle_rpc("send_message", Args) ->
    LogId = rvi_common:get_json_log_id(Args),
    {ok, TID} = rvi_common:get_json_element(["transaction_id"], Args),
    {ok, ServiceName} = rvi_common:get_json_element(["service_name"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, ProtoOpts} = rvi_common:get_json_element(["protocol_opts"], Args),
    {ok, DataLinkMod} = rvi_common:get_json_element(["data_link_mod"], Args),
    {ok, DataLinkOpts} = rvi_common:get_json_element(["data_link_opts"], Args),
    {ok, Parameters} = rvi_common:get_json_element(["parameters"], Args),
    [ ok ] = gen_server:call(?SERVER, { rvi, send_message,
                                        [TID,
                                         ServiceName,
                                         Timeout,
                                         ProtoOpts,
                                         DataLinkMod,
                                         DataLinkOpts,
                                         Parameters,
                                         LogId]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};


handle_rpc(Other, _Args) ->
    ?warning("proto_msgpack_rpc:handle_rpc(~p): Unknown~n", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


handle_notification("receive_message", Args) ->
    LogId = rvi_common:get_json_log_id(Args),
    {ok, Data} = rvi_common:get_json_element(["data"], Args),
    {ok, RemoteIP} = rvi_common:get_json_element(["remote_ip"], Args),
    {ok, RemotePort} = rvi_common:get_json_element(["remote_port"], Args),
    gen_server:cast(?SERVER, { rvi, receive_message, [Data,
                                                      RemoteIP,
                                                      RemotePort,
                                                      LogId]}),
    ok;

handle_notification(Other, _Args) ->
    ?debug("handle_notification(Other=~p): unknown", [ Other ]),
    ok.


handle_call({rvi, send_message,
             [TID,
              ServiceName,
              Timeout,
              ProtoOpts,
              DataLinkMod,
              DataLinkOpts,
              Parameters
              | LogId]}, _From, St) ->
    ?debug("    protocol:send(): transaction id:  ~p~n", [TID]),
    ?debug("    protocol:send(): service name:    ~p~n", [ServiceName]),
    ?debug("    protocol:send(): timeout:         ~p~n", [Timeout]),
    ?debug("    protocol:send(): opts:            ~p~n", [ProtoOpts]),
    ?debug("    protocol:send(): data_link_mod:   ~p~n", [DataLinkMod]),
    ?debug("    protocol:send(): data_link_opts:  ~p~n", [DataLinkOpts]),
    ?debug("    protocol:send(): parameters:      ~p~n", [Parameters]),
    Data = msgpack:pack([ { <<"tid">>, TID },
			  { <<"service">>, ServiceName },
			  { <<"timeout">>, Timeout },
			  { <<"parameters">>, Parameters } ], St#st.pack_opts),
    RviOpts = rvi_common:rvi_options(Parameters),
    Res = DataLinkMod:send_data(
	    St#st.cs, ?MODULE, ServiceName, RviOpts ++ DataLinkOpts, Data),
    {reply, Res, St};

handle_call(Other, _From, St) ->
    ?warning("proto_msgpack_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, [ invalid_command ], St}.


%% Convert list-based data to binary.
handle_cast({rvi, receive_message, [Payload, IP, Port | LogId]} = Msg, St) ->
    ?debug("~p:handle_cast(~p)", [?MODULE, Msg]),
    {ok, Elems} = msgpack:unpack(Payload, St#st.pack_opts),

    [ ServiceName, Timeout, Parameters ] =
	opts([<<"service">>, <<"timeout">>, <<"parameters">>],
	     Elems, undefined),

            ?debug("    protocol:rcv(): service name:    ~p~n", [ServiceName]),
            ?debug("    protocol:rcv(): timeout:         ~p~n", [Timeout]),
            ?debug("    protocol:rcv(): remote IP/Port:  ~p~n", [{IP, Port}]),

    service_edge_rpc:handle_remote_message(St#st.cs,
					   {IP, Port},
					   ServiceName,
					   Timeout,
					   Parameters),
    {noreply, St};

handle_cast(Other, St) ->
    ?warning("proto_msgpack_rpc:handle_cast(~p): unknown", [ Other ]),
    {noreply, St}.

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

opt(K, L, Def) ->
    case lists:keyfind(K, 1, L) of
        {_, V} -> V;
        false  -> Def
    end.

opts(Keys, Elems, Def) ->
    [ opt(K, Elems, Def) || K <- Keys].
