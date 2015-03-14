%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%


-module(rvi_common).

%%-include_lib("lhttpc/include/lhttpc.hrl").
-include_lib("lager/include/log.hrl").

-export([send_http_request/3]).
-export([send_component_request/3]).
-export([send_component_request/4]).
-export([get_component_url/1]).
-export([get_component_process/1]).
-export([json_rpc_status/1]).
-export([get_json_element/2]).
-export([sanitize_service_string/1]).
-export([local_service_to_string/1]).
-export([local_service_to_string/2]).
-export([remote_service_to_string/1]).
-export([remote_service_to_string/2]).
-export([local_service_prefix/0]).
-export([get_static_node/1]).
-export([static_nodes/0]).
-export([node_address_string/0]).
-export([node_address_tuple/0]).
-export([get_request_result/1]).
-export([get_component_config/1]).
-export([get_component_config/2]).
-export([get_component_config/3]).


-define(NODE_SERVICE_PREFIX, node_service_prefix).
-define(NODE_ADDRESS, node_address).
-define(STATIC_NODES, static_nodes).


json_rpc_status(0) ->
    ok;

json_rpc_status("0") ->
    ok;

json_rpc_status(1) ->
    invalid_command;

json_rpc_status("1") ->
    invalid_command;

json_rpc_status(2) ->
    not_found;

json_rpc_status("2") ->
    not_found;

json_rpc_status(3) ->
    not_available;

json_rpc_status("3") ->
    not_available;

json_rpc_status(4) ->
    internal;

json_rpc_status("4") ->
    internal;

json_rpc_status(5) ->
    already_connected;

json_rpc_status("5") ->
    already_connected;


json_rpc_status(Unknown) when is_integer(Unknown)->
    undefined;

json_rpc_status(Unknown) when is_list(Unknown)->
    undefined;

json_rpc_status(ok) ->
    0;

json_rpc_status(invalid_command) ->
    1;

json_rpc_status(not_found) ->
    2;

json_rpc_status(not_available) ->
    3;

json_rpc_status(internal) ->
    4;

json_rpc_status(already_connected) ->
    5;

json_rpc_status(_) ->
    999.

get_request_result({ok, {http_response, {_V1, _V2}, 200, _Text, _Hdr}, JSONBody}) ->
    case get_json_element(["result", "status"], JSONBody) of 
	{ok, Value} ->
	    { ok, json_rpc_status(Value), JSONBody };

	{ error, undefined} ->
	    {ok, undefined }
    end;

get_request_result({ok, {http_response, {_V1, _V2}, Status, Reason, _Hdr}, _JSONBody}) ->
    {error, {http, Status, Reason}};

get_request_result({error, Reason})->
    { error, Reason};

get_request_result(ok)->
    { ok, ok, "{}"};

get_request_result(Other)->
    ?error("get_request_result(): Unhandled result: ~p", [Other]),    
    { error, format}.

			
%% Send a request to another component (service_edge, authorize, etc).
send_component_request(Component, Service, ArgList) ->
    case send_component_request(Component, Service, ArgList, []) of
	{ ok, Status, _} ->
	    { ok, Status };

	Err -> Err
    end.

%% FIXME:
%% This function shooudl clean up the values in ReturnParams
%% so that they are regular tuples. 
%% Today ReturnParams contains { struct, { "some_element", { array, [...]}}}.
%% Callers to send_component_request() Need to be fixed as well.

send_component_request(Component, Service, ArgList, ReturnParams) ->

    %% ?debug("send_component_request(): Component:      ~p", [ Component]),
    %% ?debug("send_component_request(): Address:        ~p", [ Address ]),
    %% ?debug("send_component_request(): Service:        ~p", [ Service]),
    %% ?debug("send_component_request(): ArgList:        ~p", [ ArgList]),
    %% ?debug("send_component_request(): ReturnParams:   ~p", [ ReturnParams]),
	    

    %%
    %% Send to gen_server, if configured, else URL
    %%
    case { rvi_common:get_component_process(Component), 
	   rvi_common:get_component_url(Component) }  of

	%% We have a gen_server process to send to. Let the gen_server take priority
	%% over JSON-RPC URL
	{undefined, undefined} -> 
	    { error, {unknown_component, Component} };

	{ undefined, Address } ->
	    case get_request_result(
		   send_http_request(Address, atom_to_list(Service),  ArgList)
		  ) of
		{ok, Status, JSONBody} ->
		    ReturnVal = retrieve_json_reply_elements(ReturnParams, JSONBody),
		    { ok, Status, ReturnVal };

		Err -> Err
	    end;

	{ Proc, _ } ->
	    ?debug("Sending ~p:~p to ~p", [Component, Service, Proc]),
	    { Reply, ReplyArg} = gen_server:call(Proc, { rvi_call, Service, ArgList }),
	    %% Retrieve the status from the reply
	    [ Status | ReturnValues ] = retrieve_reply_elements([ status | ReturnParams], ReplyArg),
	    %% Return
	    { Reply, json_rpc_status(Status), ReturnValues }
	
    end.

send_http_request(Url,Method, Args) ->

    Req = binary_to_list(
	    iolist_to_binary(
	      exo_json:encode({struct, [{"jsonrpc", "2.0"},
					{"id", 1},
					{"method",  Method},
					{"params", {struct, Args}}
				       ]
			      }))),

    Hdrs = [{'Content-Type', "application/json"} ],
    ?debug("rvi_common:send_http_request() Sending:      ~p", [Req]),
    try
        exo_http:wpost(Url, {1,1}, Hdrs, Req, 1000)
    catch
        Type:Reason ->
            ?error("rvi_common:send_http_request() CRASHED: URL:      ~p", [Url]),
            ?error("rvi_common:send_http_request() CRASHED: Hdrs:     ~p", [Hdrs]),
            ?error("rvi_common:send_http_request() CRASHED: Body:     ~p", [Req]),
            ?error("rvi_common:send_http_request() CRASHED: Type:     ~p", [Type]),
            ?error("rvi_common:send_http_request() CRASHED: Reason:   ~p", [Reason]),
            ?error("rvi_common:send_http_request() CRASHED: Stack:    ~p", [ erlang:get_stacktrace()]),
            {error, internal}
    end.
	

get_component_url(Component) when is_atom(Component) ->
    %% Locate the correct service address for the given component
    case get_component_config(Component, url) of
	{ok, URL } -> URL;

	_ -> undefined
    end.


get_component_process(Component) when is_atom(Component) ->
    %% Locate the correct service address for the given component
    case get_component_config(Component, gen_server) of
	{ok, GenServer } -> GenServer;
	_ -> undefined
    end.


%% If Path is just a single element, convert to list and try again.
get_json_element(ElemPath, JSON) when is_atom(ElemPath) ->
    get_json_element([ElemPath], JSON);

get_json_element(ElemPath, JSON) when is_binary(JSON) ->
    get_json_element(ElemPath, binary_to_list(JSON));

get_json_element(ElemPath, JSON) when is_tuple(JSON) ->
    get_json_element_(ElemPath, JSON);

get_json_element(ElemPath, JSON) when is_list(JSON) ->
    case  exo_json:decode_string(JSON) of
	{ok,  Data } ->
	    get_json_element_(ElemPath, Data);

	Err -> 
	    Err
    end;

get_json_element(P, J) ->
    ?warning("get_json_element(): Unknown call structure; Path: ~p | JSON: ~p",
	      [P, J]),
    {error, call, {P, J}}.

get_json_element_(_, undefined) ->
    { error, undefined };

get_json_element_([], {array, JSON}) ->
    {ok, JSON};

get_json_element_([], {struct, JSON}) ->
    {ok, JSON};

get_json_element_([], JSON) ->
    {ok, JSON};


%% All proplist keys in JSON are strings. 
%% Convert atomically provided path elements to strings
get_json_element_([Elem | T], JSON ) when is_atom(Elem) ->
    get_json_element_([atom_to_list(Elem) | T], JSON);

get_json_element_([Elem | T], {struct, JSON} ) ->
    Res = get_json_element_(T, proplists:get_value(Elem, JSON, undefined)),
    Res;

get_json_element_([Elem | T], {array, JSON} ) ->
    Res = get_json_element_(T, proplists:get_value(Elem, JSON, undefined)),
    Res;

get_json_element_(Path,JSON) ->
    ?warning("get_json_element_(): Unhandled: Path: ~p | JSON: ~p",
	     [Path, JSON]),
    { error, undefined }.

retrieve_reply_elements(ElemList, Reply) ->
    retrieve_reply_elements(ElemList, Reply, []).

retrieve_reply_elements([], _, Acc) ->
    lists:reverse(Acc);


retrieve_reply_elements([Elem | T], Reply, Acc) ->
    case lists:keyfind(Elem, 1, Reply) of 
	{ _, Value } ->
	    retrieve_reply_elements(T, Reply, [ Value | Acc ]);
	false ->
	    retrieve_reply_elements(T, Reply, [ undefined | Acc ])
    end.
    

retrieve_json_reply_elements(Elem, JSON) ->
    retrieve_json_reply_elements(Elem, JSON, []).

retrieve_json_reply_elements([], _, Acc) ->
    lists:reverse(Acc);

retrieve_json_reply_elements([Elem | T], JSON, Acc) when is_atom(Elem) ->
    retrieve_json_reply_elements([[Elem] | T], JSON, Acc);
    

retrieve_json_reply_elements([Elem | T], JSON, Acc) when is_list(Elem) ->
    %% prefix with result since that is where all reply elements are stored.
    case get_json_element([ result | Elem ], JSON) of 
	{ ok, Value } ->
	    retrieve_json_reply_elements(T, JSON, [ Value | Acc ]);
	{ error, undefined } ->
	    retrieve_json_reply_elements(T, JSON, [ undefined | Acc ])
    end.


sanitize_service_string(Service) when is_binary(Service) ->
    sanitize_service_string(binary_to_list(Service));

sanitize_service_string(Service) when is_list(Service) ->
    %% Check if message type is specced.
    %% rpc:/jaguarlandrover.com/1234/hvac",
    %% If so, drop message type.
    case string:tokens(Service, ":") of
	[ Res ] -> 
	    Res;

	[_Type, Res ] ->
	    Res
    end.



local_service_to_string(Type, Svc) ->
    Type ++ ":" ++ local_service_to_string(Svc).


%% Make sure we don't get two slashes between the prefix and the service name
local_service_to_string([ $/ | Service]) ->
    local_service_prefix() ++ Service;

%% Make sure we don't get two slashes
local_service_to_string(Svc) ->
    local_service_prefix() ++ Svc.

remote_service_to_string(Service) ->
    Service.

remote_service_to_string(Type, Service) ->
    Type ++ ":" ++ Service.

local_service_prefix() ->
    Prefix = 
	case application:get_env(rvi, ?NODE_SERVICE_PREFIX) of
	    {ok, P} when is_atom(P) -> atom_to_list(P);
	    {ok, P} when is_list(P) -> P;
	    undefined -> 
		?debug("WARNING: Please set application rvi environment ~p", 
			  [?NODE_SERVICE_PREFIX]),
		error({missing_env, ?NODE_SERVICE_PREFIX})
	end,

    %% Tag on a trailing slash if not there already.
    case lists:last(Prefix) of
	$/ -> Prefix;

	_ -> Prefix ++ "/"
    end.

static_nodes() ->
    case application:get_env(rvi, ?STATIC_NODES) of

	{ok, NodeList} -> 
	    NodeList;

	undefined -> 
	    not_found
    end.


%% Locate the statically configured node whose service(s) prefix-
%% matches the provided service.
%% FIXME: Longest prefix match.
get_static_node(Service) ->
	case application:get_env(rvi, ?STATIC_NODES) of

	    {ok, NodeList} when is_list(NodeList) -> 
		get_static_node(Service, NodeList);

	    undefined -> 
		?debug("No ~p configured under rvi.", [?STATIC_NODES]),
		not_found
	end.

get_static_node(_Service, []) ->
    not_found;

%% Validate that argumenst are all lists.
get_static_node(Service, [{ SvcPrefix, NetworkAddress} | T ]) when 
      not is_list(Service); not is_list(SvcPrefix); not is_list(NetworkAddress) ->
    ?warning("rvi_common:get_static_node(): Could not resolve ~p against {~p, ~p}:"
	     "One or more elements not strings.",  [ Service, SvcPrefix, NetworkAddress]),
    get_static_node(Service, T );
    

%% If the service we are trying to resolve has a shorter name than
%% the prefix we are comparing with, ignore.
get_static_node(Service, [{ SvcPrefix, _NetworkAddress } | T ]) when 
      length(Service) < length(SvcPrefix) ->
    ?debug("rvi_common:get_static_node(): Service: ~p is shorter than prefix ~p. Ignore.",
	   [ Service, SvcPrefix]),
    get_static_node(Service, T );

get_static_node(Service, [{ SvcPrefix, NetworkAddress} | T] ) ->
    case string:str(Service, SvcPrefix) of
	1 ->
	    ?debug("rvi_common:get_static_node(): Service: ~p -> { ~p, ~p}.",
		   [ Service, SvcPrefix, NetworkAddress]),
	    NetworkAddress;
	_ ->
	    ?debug("rvi_common:get_static_node(): Service: ~p != { ~p, ~p}.",
		   [ Service, SvcPrefix, NetworkAddress]),
	    get_static_node(Service, T )
    end.


node_address_string() ->
    case application:get_env(rvi, ?NODE_ADDRESS) of
	{ok, P} when is_atom(P) -> atom_to_list(P);
	{ok, P} when is_list(P) -> P;
	undefined -> 
	    ?warning("WARNING: Please set application rvi environment ~p", 
		      [?NODE_ADDRESS]),
	    error({missing_env, ?NODE_ADDRESS})
    end.

node_address_tuple() ->
    case node_address_string() of
	{missing_env, _} = Err -> Err;
	Addr ->
	    [ Address, Port ] = string:tokens(Addr, ":"),	
	    { Address, list_to_integer(Port) }
    end.

get_component_config(Component) ->
    case application:get_env(rvi, components, undefined) of
	undefined -> 
	    {error, {missing_env, {rvi, { component, [ ]}}}};

	CompList ->
	    case proplists:get_value(Component, CompList, undefined) of
		undefined ->
		    Err = {missing_env, {rvi, { component, [ { Component, {}} ]}}},
		    ?debug("get_component_config(): Missing app environment: ~p",
			   [Err]),
		     {error, Err};
	
		CompConf ->
		    {ok, CompConf}
	    end
    end.
	
get_component_config(Component, Key) ->
    case get_component_config(Component) of
	{ok, PropList } ->
	    
	    case proplists:get_value(Key, PropList, undefined ) of

		undefined ->
		    Err = {missing_env, {rvi, { component, [ { Component, { Key, {}}} ]}}},
		    ?debug("get_component_config(): Missing app environment: ~p", [Err]),
		    {error, Err};

		Config-> 
		     {ok, Config }
	    end;
	Err -> Err
    end.

get_component_config(Component, Key, Default) ->
    case get_component_config(Component) of
	{ok, PropList } ->
	    {ok, proplists:get_value(Key, PropList, Default)};

	Err -> Err
    end.
