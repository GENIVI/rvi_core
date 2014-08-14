%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(data_link_bert_rpc_rpc).

-export([handle_rpc/2]).
-export([authorize/6]).
-export([receive_data/1]).
-export([service_announce/5]).
-export([ping/2]).
-export([init/0]).

-include_lib("lager/include/log.hrl").

init() ->
    ?debug("    data_link_bert_rpc_rpc:init(): Called"),
    %% Setup the bert-rpc server
    case rvi_common:get_component_config(data_link, bert_rpc_server) of
	{ok, TmpBertOpts } ->
	    %% Prepend access rights and redirects that we will need.
	    %% to route incoming calls to the correct module.
	    BertOpts = 
		[ 
		  %% {exo, [ {reuse_mode, client}]},  %% Can't get reuse to work.
		  {access, 

		  [
		   %% Redirect incoming RPCs targeting data_link to
		   %% the data_link_bert_rpc_rpc module
		   {redirect, [ { data_link, data_link_bert_rpc_rpc }]},
		   
		   %% Accept incoming RPCs targeting data_link module.
		   {accept, data_link_bert_rpc_rpc}
		  ]} | TmpBertOpts],
	    ?debug("    data_link_bert_rpc_rpc:init(): Starting BERT-RPC with ~p", [ BertOpts ]),
	    case supervisor:start_child(data_link_bert_rpc_sup, 
					{ data_link_bert_rpc_rpc, 
					  { bert_rpc_exec, start_link, [ BertOpts ] },
					  permanent, 5000, worker, [bert_rpc_exec] }) of

		{ ok, _ } ->
		    ?notice("---- RVI Node External Address: ~s", 
			    [ application:get_env(rvi, node_address, undefined)]),

		    case rvi_common:get_component_config(data_link, exo_http_opts) of
			{ ok, ExoHttpOpts } ->
			    exoport_exo_http:instance(data_link_bert_rpc_sup, 
						      data_link_bert_rpc_rpc,
						      ExoHttpOpts),
			    ok;

			Err -> 
			    ?error("    data_link_bert_rpc_rpc:init(): Failed to setup exo http server: ~p", 
				   [ Err ]),
			    Err
		    end;
		Err -> 
		    ?error("    data_link_bert_rpc_rpc:init(): Failed to launch bert server: ~p", [ Err ]),
		    Err
	    end;

	_ ->
	    ?error("    data_link_bert_rpc_rpc:init(): No { rvi, { bert_rpc_server, [...]}} app env found."),
	    {error, { missing_env, bert_rpc_server}}
    end.



setup_data_link(RemoteAddress, RemotePort, Service) ->
    { LocalAddress, LocalPort} = rvi_common:node_address_tuple(),
    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): Remote Address:   ~p", [ RemoteAddress]),
    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): Remote Port:      ~p", [ RemotePort]),
    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): Local Address:    ~p", [ LocalAddress]),
    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): Local Port:       ~p", [ LocalPort]),
    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): service:          ~p", [ Service]),

    case bert_rpc_exec:get_session(RemoteAddress, RemotePort, [tcp],
				   [{auto_connect, false},
				    binary,
				    {packet,4},
				    {active,once},
				    {send_timeout,30},
				    {send_timeout_close,true}], 100) of
	{ok, _} -> 
	    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): Already connected!"),
	    {ok, [ { status, rvi_common:json_rpc_status(ok)}]};
	Other ->
	    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): Not connected: ~p", [Other]),


	    %% Ping the server to tell it that we are available.
	    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): ---------------"),
	    ?debug("    data_link_bert_rpc_rpc:setup_data_link():    Sending ping()"),
	    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): ---------------"),
	    nice_bert_rpc:cast_host(RemoteAddress, RemotePort, 
				    [tcp], data_link, ping, 
				    [{LocalAddress, LocalPort}, 
				     {RemoteAddress, RemotePort}]),

	    %% Follow up with an authorize.
	    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): ---------------"),
	    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): Sending authorize()"),
	    ?debug("    data_link_bert_rpc_rpc:setup_data_link(): ---------------"),
	    nice_bert_rpc:cast_host(RemoteAddress, RemotePort, 
				    [tcp], data_link, authorize, 
				    [1, LocalAddress, LocalPort, bert_rpc_rpc, 
				     {certificate, {}}, { signature, {}} ]),

	    {ok, [ { status, rvi_common:json_rpc_status(ok)}]}
    end.





disconnect_data_link(RemoteAddress, RemotePort) ->
    ?debug("    data_link_bert_rpc_rpc:disconnect_data_link(): Remote Address: ~p", [ RemoteAddress]),
    ?debug("    data_link_bert_rpc_rpc:disconnect_data_link(): Remote Port:    ~p", [ RemotePort]),
    {ok, [ { status, rvi_common:json_rpc_status(ok)}]}.



send_data(RemoteAddress, RemotePort, Data) ->
    ?debug("    data_link_bert_rpc_rpc:send_data(): Remote Address: ~p", [ RemoteAddress]),
    ?debug("    data_link_bert_rpc_rpc:send_data(): Remote Port:    ~p", [ RemotePort]),
    ?debug("    data_link_bert_rpc_rpc:send_data(): Data:           ~p", [ Data]),
    case  nice_bert_rpc:cast_host(RemoteAddress, RemotePort, 
				  [tcp], 
				  data_link, receive_data, [Data]) of
	Res ->
	    ?debug ("    data_link_bert_rpc_rpc:send_data(): bert-rpc result: ~p", [ Res ]),
	    {ok, [ { status, rvi_common:json_rpc_status(ok)}]}
    end.



%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc("setup_data_link", Args) ->
    { ok, NetworkAddress } = rvi_common:get_json_element(["network_address"], Args),
    [ RemoteAddress, RemotePort] =  string:tokens(NetworkAddress, ":"),
    { ok,  Service } = rvi_common:get_json_element(["service"], Args),

    setup_data_link(RemoteAddress, list_to_integer(RemotePort), Service);


handle_rpc("disconenct_data_link", Args) ->
    { ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    [ RemoteAddress, RemotePort] =  string:tokens(NetworkAddress, ":"),

    disconnect_data_link(RemoteAddress, list_to_integer(RemotePort));    

handle_rpc("send_data", Args) ->
    {ok, NetworkAddress} = rvi_common:get_json_element(["network_address"], Args),
    [ RemoteAddress, RemotePort] =  string:tokens(NetworkAddress, ":"),
    { ok,  Data} = rvi_common:get_json_element(["data"], Args),

    send_data(RemoteAddress, list_to_integer(RemotePort), Data);
    
handle_rpc(Other, _Args) ->
    ?debug("    data_link_bert_rpc_rpc:handle_rpc(~p)", [ Other ]),
    { ok, [ { status, rvi_common:json_rpc_status(invalid_command)} ] }.



%% Functions called by BERT-RPC from remote data_link entries.
%%
ping({RemoteAddress, RemotePort}, {LocalAddress, LocalPort}) ->
    ?debug("    data_link_bert_rpc_rpc:ping(): Remoteddress:  {~p, ~p}",
		   [ RemoteAddress, RemotePort ]),
    ?debug("    data_link_bert_rpc_rpc:ping(): LocalAddress:  {~p, ~p}", 
	   [ LocalAddress, LocalPort ]),
    { Address, Port } = rvi_common:node_address_tuple(),
    ?debug("    data_link_bert_rpc_rpc:ping(): CfgAddress:    {~p, ~p}", [ Address, Port ]),

    %% Come back with with an authorize.
    %% Follow up with an authorize.
    ?debug("    data_link_bert_rpc_rpc:ping(): ----------------"),
    ?debug("    data_link_bert_rpc_rpc:ping(): Sending authorize()"),
    ?debug("    data_link_bert_rpc_rpc:ping(): ----------------"),
    nice_bert_rpc:cast_host(RemoteAddress, RemotePort, 
			    [tcp], data_link, authorize, 
			    [1, Address, Port, bert_rpc_rpc, 
			     {certificate, {}}, { signature, {}} ]),
    ok.

    
authorize(TransactionID, RemoteAddress, RemotePort, Protocol, Certificate, Signature) ->
    ?debug("    data_link_bert_rpc_rpc:authorize(): TransactionID:  ~p", [ TransactionID ]),
    ?debug("    data_link_bert_rpc_rpc:authorize(): Remote Address: ~p", [ RemoteAddress ]),
    ?debug("    data_link_bert_rpc_rpc:authorize(): Remote Port:    ~p", [ RemotePort ]),
    ?debug("    data_link_bert_rpc_rpc:authorize(): Protocol:       ~p", [ Protocol ]),
    ?debug("    data_link_bert_rpc_rpc:authorize(): Certificate:    ~p", [ Certificate ]),
    ?debug("    data_link_bert_rpc_rpc:authorize(): Signature:      ~p", [ Signature ]),
	
    { LocalAddress, LocalPort } = rvi_common:node_address_tuple(),

    %% Send our own servide announcement to the backend server
    %% First grab all our services.
    case rvi_common:send_component_request(service_discovery, get_services, [], 
					   [ services ]) of
	{ ok, _, [ JSONSvc], _JSON} -> 
	    %% Covnert to JSON structured typles.
	    LocalServices = 
		lists:foldl(fun({struct, JSONElem}, Acc) -> 
				    [ proplists:get_value("service", JSONElem, undefined) | Acc]
			    end, 
			    [], JSONSvc),

	    ?debug("    data_link_bert_rpc_rpc:authorize(): LocalSvc:       ~p", 
		   [ LocalServices ]),

	    %% Grab our local address.
	    { LocalAddress, LocalPort } = rvi_common:node_address_tuple(),

	    %% Send an authorize back to the remote node
	    ?debug("    data_link_bert_rpc_rpc:authorize(): -------------------"),
	    ?debug("    data_link_bert_rpc_rpc:authorize(): Sending announce()"),
	    ?debug("    data_link_bert_rpc_rpc:authorize(): -------------------"),
	    nice_bert_rpc:cast_host(RemoteAddress, RemotePort, 
				    [tcp], 
				    data_link, service_announce, 
				    [2, LocalAddress, LocalPort, 
				     LocalServices, { signature, {}} ]),

	    ok;

	Err -> 
	    ?warning("    data_link_bert_rpc_rpc:authorize() Failed at authorize: ~p", 
		   [ Err ]),
	   ok
    end,
    ok.
    

service_announce(TransactionID, RemoteAddress, RemotePort, Services, Signature) ->
    ?debug("    data_link_bert_rpc_rpc:service_announce(): TransactionID:  ~p", [ TransactionID ]),
    ?debug("    data_link_bert_rpc_rpc:service_announce(): Remote Address: ~p", [ RemoteAddress ]),
    ?debug("    data_link_bert_rpc_rpc:service_announce(): Remote Port:    ~p", [ RemotePort ]),
    ?debug("    data_link_bert_rpc_rpc:service_announce(): Signature:      ~p", [ Signature ]),
    ?debug("    data_link_bert_rpc_rpc:service_announce(): Services:       ~p", [ Services ]),

    %% Register the received services.
    RemoteNetworkAddress = RemoteAddress  ++ ":" ++ integer_to_list(RemotePort),
    register_announced_services(RemoteNetworkAddress, Services),

    %% Report that the data link is up

    rvi_common:send_component_request(service_discovery, data_link_up, 
					  [
					   {services, Services}, 
					   { network_address,	RemoteNetworkAddress }
					  ]),

    rvi_common:send_component_request(schedule, data_link_up, 
					  [
					   {services, Services}, 
					   { network_address,	RemoteNetworkAddress }
					  ]),

    rvi_common:send_component_request(service_edge, data_link_up, 
    					  [
     					   {services, Services}, 
     					   { network_address,	RemoteNetworkAddress }
     					  ]),
    ok.


register_announced_services(_NetworkAddress, []) ->
    ok;

register_announced_services(NetworkAddress, [ Service | T ]) ->
    ?debug("    data_link_bert_rpc_rpc:register_announced_services():Registering service: ~p | addr: ~p",
	   [Service, NetworkAddress]),
    case 
	rvi_common:send_component_request(service_discovery, register_remote_service, 
					  [
					   {service, Service}, 
					   {network_address, NetworkAddress}
					  ]) of
	{ ok, _, _JSON} -> 
	    register_announced_services(NetworkAddress, T);

	Err -> 
	    ?debug("    data_link_bert_rpc_rpc:register_announced_services(): Failed at data_link_up: ~p", 
		      [ Err ]),
	    Err
    end.



receive_data(Data) ->
    ?debug("    data_link_bert_rpc_rpc:receive_data(): ~p", [ Data ]),
    case 
	rvi_common:send_component_request(protocol, receive_message, 
					  [
					   { data, Data }
					  ]) of
	{ ok, _, _JSON} -> 
	    ok;
	Err -> 
	    ?debug("schedule:schedule_message() Failed at data_link_up: ~p", 
		      [ Err ])
    end,
    ok.

