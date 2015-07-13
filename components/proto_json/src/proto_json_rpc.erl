%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(proto_json_rpc).
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
-export([send_message/9,
	 receive_message/2]).

-record(st, { 
	  %% Component specification
	  cs = #component_spec{}
	  }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?debug("proto_json_rpc:init(): called."),
    {ok, #st { cs = rvi_common:get_component_specification() } }.

start_json_server() ->
    rvi_common:start_json_rpc_server(protocol, ?MODULE, proto_json_sup).



send_message(CompSpec, 
	     ServiceName, 
	     Timeout, 
	     ProtoOpts,
	     DataLinkMod,
	     DataLinkOpts,
	     Parameters, 
	     Signature, 
	     Certificate) ->
    rvi_common:request(protocol, ?MODULE, send_message,
		       [ { service, ServiceName },
			 { timeout, Timeout },
			 { protocol_opts, ProtoOpts },
			 { data_link_mod, DataLinkMod },
			 { data_link_opts, DataLinkOpts },
			 { parameters, Parameters },
			 { signature, Signature },
			 { certificate, Certificate }],
		       [ status ], CompSpec).

receive_message(CompSpec, Data) ->
    rvi_common:notification(protocol, ?MODULE, receive_message, 
			    [ {data, Data } ],
			    CompSpec).

%% JSON-RPC entry point

%% CAlled by local exo http server
handle_rpc("send_message", Args) ->
    {ok, ServiceName} = rvi_common:get_json_element(["service_name"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, ProtoOpts} = rvi_common:get_json_element(["protocol_opts"], Args),
    {ok, DataLinkMod} = rvi_common:get_json_element(["data_link_mod"], Args),
    {ok, DataLinkOpts} = rvi_common:get_json_element(["data_link_opts"], Args),
    {ok, Parameters} = rvi_common:get_json_element(["parameters"], Args),
    {ok, Signature} = rvi_common:get_json_element(["signature"], Args),
    {ok, Certificate} = rvi_common:get_json_element(["certificate"], Args),
    [ ok ] = gen_server:call(?SERVER, { rvi, send_message, 
					[ServiceName,
					 Timeout,
					 ProtoOpts,
					 DataLinkMod,
					 DataLinkOpts,
					 Parameters,
					 Signature, 
					 Certificate]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};
				 


handle_rpc(Other, _Args) ->
    ?warning("proto_json_rpc:handle_rpc(~p): Unknown~n", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.


handle_notification("receive_message", Args) ->
    {ok, Data} = rvi_common:get_json_element(["data"], Args),

    gen_server:cast(?SERVER, { rvi, receive_message, [Data]}),
    ok;

handle_notification(Other, _Args) ->
    ?debug("proto_json_rpc:handle_other(~p): unknown", [ Other ]),
    ok.


handle_call({rvi, send_message, 
	     [ServiceName,
	      Timeout,
	      ProtoOpts,
	      DataLinkMod,
	      DataLinkOpts,
	      Parameters,
	      Signature,
	      Certificate]}, _From, St) ->
    ?debug("    protocol:send(): service name:    ~p~n", [ServiceName]),
    ?debug("    protocol:send(): timeout:         ~p~n", [Timeout]),
    ?debug("    protocol:send(): opts:            ~p~n", [ProtoOpts]),
    ?debug("    protocol:send(): data_link_mod:   ~p~n", [DataLinkMod]),
    ?debug("    protocol:send(): data_link_opts:  ~p~n", [DataLinkOpts]),
    ?debug("    protocol:send(): parameters:      ~p~n", [Parameters]),
    ?debug("    protocol:send(): signature:       ~p~n", [Signature]),
    ?debug("    protocol:send(): certificate:     ~p~n", [Certificate]),

    
    Data = term_to_json({ struct, 
			  [
			   { "service", ServiceName },
			   { "timeout", Timeout }, 
			   { "parameters", Parameters },
			   { "signature", Signature },
			   { "certificate", Certificate }
			  ]
			}),

    Res = DataLinkMod:send_data(St#st.cs, ?MODULE, ServiceName, DataLinkOpts, Data),

    { reply, Res, St };


handle_call(Other, _From, St) ->
    ?warning("proto_json_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, [ invalid_command ], St}.

%% Convert list-based data to binary.
handle_cast({rvi, receive_message, [Data]}, St) when is_list(Data)->
    handle_cast({ rvi, receive_message, [ list_to_binary(Data) ] }, St);

handle_cast({rvi, receive_message, [Payload]}, St) ->
    {ok, {struct, Elems}} = exo_json:decode_string(binary_to_list(Payload)),

    [ ServiceName, Timeout, Parameters, Signature, Certificate ] = 
	opts(["service", "timeout", "parameters", 
	      "signature", "certificate"], Elems, undefined),

    ?debug("    protocol:rcv(): service name:    ~p~n", [ServiceName]),
    ?debug("    protocol:rcv(): timeout:         ~p~n", [Timeout]),
%%    ?debug("    protocol:rcv(): parameters:      ~p~n", [Parameters]),
    ?debug("    protocol:rcv(): signature:       ~p~n", [Signature]),
    ?debug("    protocol:rcv(): certificate:     ~p~n", [Certificate]),

    service_edge_rpc:handle_remote_message(St#st.cs, 
					   ServiceName,
					   Timeout,
					   Parameters,
					   Signature,
					   Certificate),
    {noreply, St};


handle_cast(Other, St) ->
    ?warning("proto_json_rpc:handle_cast(~p): unknown", [ Other ]),
    {noreply, St}.

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

term_to_json(Term) ->
    binary_to_list(iolist_to_binary(exo_json:encode(Term))).

opt(K, L, Def) ->
    case lists:keyfind(K, 1, L) of
	{_, V} -> V;
	false  -> Def
    end.

opts(Keys, Elems, Def) ->
    [ opt(K, Elems, Def) || K <- Keys].
