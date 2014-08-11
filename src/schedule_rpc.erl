%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%




-module(schedule_rpc).


-export([handle_rpc/2]).
-export([init/0]).

-include_lib("lager/include/log.hrl").

init() ->
    case rvi_common:get_component_config(schedule, exo_http_opts) of
	{ ok, ExoHttpOpts } ->
	    exoport_exo_http:instance(schedule_sup, 
				      schedule_rpc,
				      ExoHttpOpts);
	Err -> Err
    end,
    ok.

schedule_message(Target, Timeout, NetworkAddress, Parameters, Signature, Certificate) ->
    ?debug("    schedule_rpc:schedule_request(): target:          ~p", [ Target]),
    ?debug("    schedule_rpc:schedule_request(): timeout:         ~p", [ Timeout]),
    ?debug("    schedule_rpc:schedule_request(): network_address: ~p", [ NetworkAddress]),
    ?debug("    schedule_rpc:schedule_request(): parameters:      ~p", [Parameters]),
    ?debug("    schedule_rpc:schedule_request(): signature:       ~p", [Signature]),
    ?debug("    schedule_rpc:schedule_request(): certificate:     ~p", [Certificate]),
    schedule:schedule_message(Target, 
			      Timeout, 
			      NetworkAddress, 
			      Parameters,
			      Signature,
			      Certificate),
    {ok, [ { status, rvi_common:json_rpc_status(ok)}]}.

data_link_up(NetworkAddress, AvailableServices) ->
    ?debug("    schedule_rpc:data_link_up(): network_address: ~p", [ NetworkAddress]),
    ?debug("    schedule_rpc:data_link_up(): services:        ~p", [ AvailableServices]),
    schedule:data_link_up(NetworkAddress, AvailableServices),
    {ok, [ { status, rvi_common:json_rpc_status(ok)}]}.


data_link_down(NetworkAddress, DiscountinuedServices) ->
    ?debug("    schedule_rpc:data_link_down(): network_address: ~p", [ NetworkAddress]),
    ?debug("    schedule_rpc:data_link_down(): services         ~p", [ DiscountinuedServices]),
    schedule:data_link_down(NetworkAddress, DiscountinuedServices),
    {ok, [ { status, rvi_common:json_rpc_status(ok)}]}.


%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc("schedule_message", Args) ->
    {ok, Target} = rvi_common:get_json_element(["target"], Args),
    {ok, Timeout} = rvi_common:get_json_element(["timeout"], Args),
    {ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    {ok,  Parameters} = rvi_common:get_json_element(["parameters"], Args),
    {ok, Signature} = rvi_common:get_json_element(["signature"], Args),
    {ok, Certificate} = rvi_common:get_json_element(["certificate"], Args),
    schedule_message(Target, 
		     Timeout, 
		     NetworkAddress,
		     Parameters,
		     Signature,
		     Certificate);

handle_rpc("data_link_up", Args) ->
    {ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    {ok,  AvailableServices} = rvi_common:get_json_element(["services"], Args),
    data_link_up(NetworkAddress, AvailableServices);

handle_rpc("data_link_down", Args) ->
    {ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    {ok,  DiscountinuedServices} = rvi_common:get_json_element(["services"], Args),
    data_link_down(NetworkAddress, DiscountinuedServices);

handle_rpc(Other, _Args) ->
    ?debug("    schedule_rpc:handle_rpc(~p): unknown", [ Other ]),
    {ok, [ {status, rvi_common:json_rpc_status(invalid_command)}]}.

