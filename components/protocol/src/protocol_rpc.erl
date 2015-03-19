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

-define(SERVER, ?MODULE). 
-export([start_json_server/0]).


-record(st, { }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?debug("protocol_rpc:init(): called."),
    {ok, #st {}}.

start_json_server() ->
    rvi_common:start_json_rpc_server(protocol, ?MODULE, protocol_sup).


send_message(ServiceName, Timeout, NetworkAddress, Parameters, Signature, Certificate) ->
    ?debug("    protocol:send(): service name:    ~p~n", [ServiceName]),
    ?debug("    protocol:send(): timeout:         ~p~n", [Timeout]),
    ?debug("    protocol:send(): network_address: ~p~n", [NetworkAddress]),
%%    ?debug("    protocol:send(): parameters:      ~p~n", [Parameters]),
    ?debug("    protocol:send(): signature:       ~p~n", [Signature]),
    ?debug("    protocol:send(): certificate:     ~p~n", [Certificate]),
    Data = term_to_binary({ ServiceName, Timeout, NetworkAddress, 
			    Parameters, Signature, Certificate }),

    case rvi_common:send_component_request(data_link, send_data,  
				      [
				       {network_address, NetworkAddress}, 
				       {data, Data}
				      ]) of 
	{ ok, _JSONStatus } -> 
	    {ok, [ { status, rvi_common:json_rpc_status(ok)}]};
	
	Err -> 
	    ?debug("    protocol_rpc:send() Failed at data_link:transmit_data(): ~p~n", 
		      [ Err ]),
	    Err
    end.

receive_message(Data) when is_list(Data) ->
    receive_message(list_to_binary(Data));

receive_message(Data) when is_binary(Data) ->
    { ServiceName, Timeout, NetworkAddress, Parameters, Signature, Certificate } = 
	binary_to_term(Data),
    ?debug("    protocol:rcv(): service name:    ~p~n", [ServiceName]),
    ?debug("    protocol:rcv(): timeout:         ~p~n", [Timeout]),
    ?debug("    protocol:rcv(): network_address: ~p~n", [NetworkAddress]),
%%    ?debug("    protocol:rcv(): parameters:      ~p~n", [Parameters]),
    ?debug("    protocol:rcv(): signature:       ~p~n", [Signature]),
    ?debug("    protocol:rcv(): certificate:     ~p~n", [Certificate]),
    case 
	rvi_common:send_component_request(service_edge, handle_remote_message, 
					  [
					   { service_name, ServiceName },
					   { timeout, Timeout},
					   { network_address, NetworkAddress},
					   { parameters, Parameters},
					   { signature, Signature},
					   { certificate, Certificate }
					  ]) of
	{ ok, _JSONStatus} -> 
	    {ok, [ { status, rvi_common:json_rpc_status(ok)}]};
	
	Err -> 
	    ?debug("    protocol_rpc:rcv() service_edge:handle_remote_message() call failed with: ~p~n", 
		      [ Err ]),
	    Err
    end.

%% JSON-RPC entry point

%% CAlled by local exo http server
handle_rpc("send_message", Args) ->
    {ok, ServiceName} = rvi_common:get_json_element(["service_name"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    {ok, Parameters} = rvi_common:get_json_element(["parameters"], Args),
    {ok, Signature} = rvi_common:get_json_element(["signature"], Args),
    {ok, Certificate} = rvi_common:get_json_element(["certificate"], Args),
    send_message(ServiceName,
		 Timeout,
		 NetworkAddress,
		 Parameters,
		 Signature, 
		 Certificate);

handle_rpc("receive_message", Args) ->
    {ok, Data} = rvi_common:get_json_element(["data"], Args),
    receive_message(Data);

handle_rpc(Other, _Args) ->
    ?debug("    protocol_rpc:handle_rpc(~p): Unknown~n", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.

handle_call({rvi_call, send_message, Args}, _From, State) ->
    {_, ServiceName} = lists:keyfind(service_name, 1, Args),
    {_, Timeout} = lists:keyfind(timeout, 1, Args),
    {_, NetworkAddress} = lists:keyfind(network_address, 1, Args),
    {_, Parameters} = lists:keyfind(parameters, 1, Args),
    {_, Signature} = lists:keyfind(signature, 1, Args),
    {_, Certificate} = lists:keyfind(certificate, 1, Args),
    { reply, send_message(ServiceName,
			  Timeout,
			  NetworkAddress,
			  Parameters,
			  Signature, 
			  Certificate), State };


handle_call({rvi_call, receive_message, Args}, _From, State) ->
    {_, Data} = lists:keyfind(data, 1, Args),
    {reply, receive_message(Data), State};

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
