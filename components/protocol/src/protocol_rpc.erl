%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(protocol_rpc).
-behaviour(gen_server).

-export([handle_rpc/2]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-include_lib("lager/include/log.hrl").
-include_lib("rvi_common/include/rvi_common.hrl").

-define(SERVER, ?MODULE). 
-export([start_json_server/0]).
-export([send_message/7,
	 receive_message/2]).

-record(st, { 
	  %% Component specification
	  cs = #component_spec{}
	  }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?debug("protocol_rpc:init(): called."),
    {ok, #st { cs = rvi_common:get_component_specification() } }.

start_json_server() ->
    rvi_common:start_json_rpc_server(protocol, ?MODULE, protocol_sup).



send_message(CompSpec, 
	     ServiceName, 
	     Timeout, 
	     NetworkAddress, 
	     Parameters, 
	     Signature, 
	     Certificate) ->
    rvi_common:request(protocol, ?MODULE, send_message,
		       [ ServiceName,
			 Timeout,
			 NetworkAddress, 
			 Parameters, 
			 Signature,
			 Certificate],
		       [ service,
			 timeout,
			 network_address,
			 signature,
			 certificate],
		       [ status ], CompSpec).

receive_message(CompSpec, Data) ->
    rvi_common:request(protocol, ?MODULE, receive_message, 
		       [ Data ], [ data ], 
		       [status], CompSpec).

%% JSON-RPC entry point

%% CAlled by local exo http server
handle_rpc("send_message", Args) ->
    {ok, ServiceName} = rvi_common:get_json_element(["service_name"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    {ok, Parameters} = rvi_common:get_json_element(["parameters"], Args),
    {ok, Signature} = rvi_common:get_json_element(["signature"], Args),
    {ok, Certificate} = rvi_common:get_json_element(["certificate"], Args),
    [ ok ] = gen_server:call(?SERVER, { rvi_call, send_message, 
					[ServiceName,
					 Timeout,
					 NetworkAddress,
					 Parameters,
					 Signature, 
					 Certificate]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};
				 

handle_rpc("receive_message", Args) ->
    {ok, Data} = rvi_common:get_json_element(["data"], Args),

    [ ok ] = gen_server:call(?SERVER, { rvi_call, receive_message, [Data]}),
    {ok, [ {status, rvi_common:json_rpc_status(ok)} ]};


handle_rpc(Other, _Args) ->
    ?debug("    protocol_rpc:handle_rpc(~p): Unknown~n", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.

handle_call({rvi_call, send_message, 
	     [ServiceName,
	      Timeout,
	      NetworkAddress,
	      Parameters,
	      Signature,
	      Certificate]}, _From, State) ->
    ?debug("    protocol:send(): service name:    ~p~n", [ServiceName]),
    ?debug("    protocol:send(): timeout:         ~p~n", [Timeout]),
    ?debug("    protocol:send(): network_address: ~p~n", [NetworkAddress]),
%%    ?debug("    protocol:send(): parameters:      ~p~n", [Parameters]),
    ?debug("    protocol:send(): signature:       ~p~n", [Signature]),
    ?debug("    protocol:send(): certificate:     ~p~n", [Certificate]),

    
    Data = term_to_binary({ ServiceName, Timeout, NetworkAddress, 
			    Parameters, Signature, Certificate }),

    Res = rvi_common:request(data_link, data_link_bert_rpc, send_data,
			     [ NetworkAddress, Data ],
			     [ network_address, data ],
			     [ status], 
			     State#st.cs),
    { reply, Res, State };


%% Convert list-based data to binary.
handle_call({rvi_call, receive_message, [Data]}, From, State) when is_list(Data)->
    handle_call({ rvi_call, receive_message, 
		  [ list_to_binary(Data) ] }, From, State);

handle_call({rvi_call, receive_message, [Data]}, _From, State) ->
    { ServiceName, 
      Timeout, 
      NetworkAddress, 
      Parameters, 
      Signature, 
      Certificate } = binary_to_term(Data),
    ?debug("    protocol:rcv(): service name:    ~p~n", [ServiceName]),
    ?debug("    protocol:rcv(): timeout:         ~p~n", [Timeout]),
    ?debug("    protocol:rcv(): network_address: ~p~n", [NetworkAddress]),
%%    ?debug("    protocol:rcv(): parameters:      ~p~n", [Parameters]),
    ?debug("    protocol:rcv(): signature:       ~p~n", [Signature]),
    ?debug("    protocol:rcv(): certificate:     ~p~n", [Certificate]),
    Res = rvi_common:request(service_edge, service_edge_rpc, 
			     handle_remote_message, 
			     [ ServiceName,
			       Timeout,
			       NetworkAddress,
			       Parameters,
			       Signature,
			       Certificate ],
			     [ service_name,
			       timeout,
			       network_address,
			       parameters,
			       signature,
			       certificate ],
			     [ status ],
			    State#st.cs),
    {reply, Res , State};

handle_call(Other, _From, State) ->
    ?warning("protocol_rpc:handle_call(~p): unknown", [ Other ]),
    { reply, { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ]}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
