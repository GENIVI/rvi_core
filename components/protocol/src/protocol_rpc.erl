%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(protocol_rpc).


-export([handle_rpc/2]).
-export([init/0]).

-include_lib("lager/include/log.hrl").

init() ->
    case rvi_common:get_component_config(protocol, exo_http_opts) of
	{ ok, ExoHttpOpts } ->
	    exoport_exo_http:instance(protocol_sup, 
				      protocol_rpc,
				      ExoHttpOpts);
	Err -> Err
    end,

    ok.


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
	{ ok, _JSONStatus, _JSON} -> 
	    {ok, [ { status, rvi_common:json_rpc_status(ok)}]};
	
	Err -> 
	    ?debug("    protocol_rpc:send() Failed at data_link:transmit_data(): ~p~n", 
		      [ Err ]),
	    Err
    end.

receive_message(Data) ->
    { ServiceName, Timeout, NetworkAddress, Parameters, Signature, Certificate } = 
	binary_to_term(list_to_binary(Data)),
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
	{ ok, _JSONStatus, _JSON} -> 
	    {ok, [ { status, rvi_common:json_rpc_status(ok)}]};
	
	Err -> 
	    ?debug("    protocol_rpc:rcv() Failed at service_edge:process_remote_message(): ~p~n", 
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

