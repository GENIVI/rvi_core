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

-include_lib("rvi_common/include/rvi_common.hrl").

-export([send_http_request/3]).
-export([request/7]).
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
-export([get_component_specification/0,
	 get_component_modules/1,
	 get_component_modules/2,
	 get_module_specification/3,
	 get_module_config/4,
	 get_module_config/5,
	 get_module_json_rpc_address/3,
	 get_module_json_rpc_url/3,
	 get_module_genserver_pid/3
	]).

-export([start_json_rpc_server/3]).

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
	    ?debug("get_request_result(~s)", [JSONBody]),
	    { json_rpc_status(Value), JSONBody };

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
    { error, format }.


json_argument([], [], Acc) ->
    Acc;

json_argument([Arg | AT], [Spec | ST], Acc) ->
    json_argument(AT, ST, [ { Spec, Arg } | Acc]).

%% Convert a list of unnamed arguments to a proplist
%% understood by json encode
json_argument(ArgList, SpecList) ->
    { struct, json_argument(ArgList, SpecList, []) }.

request(Component, 
	Module, 
	Function, 
	InArg,
	InArgSpec,
	OutArgSpec,
	CompSpec) ->

    %% Figure out how we are to invoke this MFA.
    case get_module_type(Component, Module, CompSpec) of
	%% We have a gen_server
	{ ok, gen_server } ->
	    ?debug("Sending ~p - ~p:~p(~p)", [Component, Module, Function, InArg]),	
	    gen_server:call(Module, { rvi_call, Function, InArg});

	%% We have a JSON-RPC server
	{ ok,  json_rpc } ->
	    URL = get_module_json_rpc_url(Component, Module, CompSpec),
	    ?debug("Sending ~p:~p(~p) -> ~p.", [Module, Function, InArg, URL]),	
	    JSONArg = json_argument(InArg, InArgSpec),
	    ?debug("Sending ~p:~p(~p) -> ~p.", [Module, Function, InArg, JSONArg]),	

	    case get_request_result(
		   send_http_request(URL, atom_to_list(Function),  JSONArg)
		  ) of

		{ ok, JSONBody} ->
		    json_reply(OutArgSpec, JSONBody);

		Err -> Err
	    end;

	Err1 -> Err1
    end.

send_http_request(Url,Method, Args) ->

    Req = binary_to_list(
	    iolist_to_binary(
	      exo_json:encode({struct, [{"jsonrpc", "2.0"},
					{"id", 1},
					{"method",  Method},
					{"params", Args}
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

    
json_reply(ArgList, JSON) ->
    retrieve_json_reply_elements(ArgList, JSON, []).

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

get_component_config_(Component, Default, CompList) ->
    case proplists:get_value(Component, CompList, undefined) of
	undefined ->
	    ?debug("get_component_config(~p): Default: ~p", 
		   [Component, Default]),
	     Default;
	
	ModList ->
	    ?debug("get_component_config(~p) -> ~p", 
		   [Component, ModList]),
	    ModList
    end.

get_component_specification() ->
    case application:get_env(rvi, components, undefined) of
	undefined -> 
	    #component_spec { 
	       service_edge = ?COMP_SPEC_SERVICE_EDGE_DEFAULT,
	       scheduler = ?COMP_SPEC_SCHEDULER_DEFAULT,
	       service_discovery = ?COMP_SPEC_SERVICE_DISCOVERY_DEFAULT,
	       authorize = ?COMP_SPEC_AUTHORIZE_DEFAULT,
	       data_link = ?COMP_SPEC_DATA_LINK_DEFAULT,
	       protocol = ?COMP_SPEC_PROTOCOL_DEFAULT
	      };

	CompList ->
	    #component_spec { 
	       service_edge = get_component_config_(service_edge, 
						    ?COMP_SPEC_SERVICE_EDGE_DEFAULT,
						    CompList), 
	       scheduler = get_component_config_(scheduler,
						 ?COMP_SPEC_SCHEDULER_DEFAULT,
						 CompList),
	       service_discovery = get_component_config_(service_discovery, 
							 ?COMP_SPEC_SERVICE_DISCOVERY_DEFAULT,
							 CompList),
	       authorize = get_component_config_(authorize, 
						 ?COMP_SPEC_AUTHORIZE_DEFAULT,
						 CompList),
	       data_link = get_component_config_(data_link, 
						 ?COMP_SPEC_DATA_LINK_DEFAULT,
						 CompList),
	       protocol =  get_component_config_(protocol, 
						 ?COMP_SPEC_PROTOCOL_DEFAULT,
						 CompList)
	      }
    end.


get_component_modules(Component) ->
    get_component_modules(Component, get_component_specification()).

get_component_modules(service_edge, CompSpec) ->
    CompSpec#component_spec.service_edge;

get_component_modules(scheduler, CompSpec) ->
    CompSpec#component_spec.scheduler;

get_component_modules(service_discovery, CompSpec) ->
    CompSpec#component_spec.service_discovery;

get_component_modules(authorize, CompSpec) ->
    CompSpec#component_spec.authorize;

get_component_modules(data_link, CompSpec) ->
    CompSpec#component_spec.data_link;

get_component_modules(protocol, CompSpec) ->
    CompSpec#component_spec.protocol;
    
get_component_modules(_, _) ->
    undefined.

	
%% Get the spec for a specific module (protocol_bert_rpc) within
%% a component (protocol).
get_module_specification(Component, Module, CompSpec) ->
    case get_component_modules(Component, CompSpec) of
	undefined ->
	    undefined;

	Modules ->
	    case lists:keyfind(Module, 1, Modules ) of
		false ->
		    ?debug("get_component_module_specification(): Missing component spec: "
			   "rvi:component:~p:~p:{...}", [Component, Module]),
		    {error, {not_found, Module}};

		{ Module, Type, ModConf } -> 
		    ?debug("get_component_module_specification(): ~p:~p -> ~p ",
			   [Component, Module, { Module, Type, ModConf}]),
		    {ok, Module, Type, ModConf };

		IllegalFormat ->
		    ?warning("get_component_module_specification(): Illegal format: ~p: ~p", 
			     [Module, IllegalFormat]),
		    {error, {illegal_format,{ Module, IllegalFormat } } }
	    end
    end.

%% Get a specific option (bert_rpc_port) for a specific module
%% (protocol_bert_rpc) within a component (protocol).
get_module_config(Component, Module, Key, CompSpec) ->
    case get_module_specification(Component, Module, CompSpec) of
	{ok, _Module, _Type, ModConf } ->
	    case proplists:get_value(Key, ModConf, undefined ) of
		undefined ->
		    ?debug("get_component_config(): Missing component spec: "
			   "rvi:component:~p:~p:~p{...}", [Component, Module, Key]),
		    {error, {not_found, Component, Module, Key}};


		Config -> 
		    ?debug("get_component_config(): ~p:~p:~p -> ~p: ",
			   [Component, Module, Key, Config]),
		    {ok, Config }
	    end;
	Err -> Err
    end.

%% Get a specific option (bert_rpc_port) for a specific module
%% (protocol_bert_rpc) within a component (protocol), with
%% a default value.
get_module_config(Component, Module, Key, Default, CompSpec) ->
    case get_module_config(Component, Module, Key, CompSpec) of
	{ok, Config } ->
	    {ok, Config };

	_ -> {ok, Default }
    end.


get_module_type(Component, Module, CompSpec) ->
    case get_module_specification(Component, Module, CompSpec) of
	{ok, _Module, Type, _ModConf } ->
	    {ok, Type} ;

	Err -> Err
    end.

get_module_json_rpc_address(Component, Module, CompSpec) ->
    %% Dig out the JSON RPC address
    case get_module_config(Component,
			   Module, 
			   json_rpc_address,
			   undefined, 
			   CompSpec) of
	undefined ->
	    ?debug("get_component_config(): Missing component spec: "
		   "rvi:component:~p:~p:json_rpc_addr, {...}", [Component, Module]),
	    {error, {not_found, Component, Module, json_rpc_addr}};

	{ok, { IP, Port }} -> 
	    ?debug("get_module_json_rpc_address(~p, ~p) -> ~p:~p", 
		   [ Component, Module, IP, Port]),
	    {ok,  IP, Port };

	{ok, Port } -> 
	    ?debug("get_module_json_rpc_address(~p, ~p) -> 127.0.0.1:~p", 
		   [ Component, Module, Port]),
	    {ok,   "127.0.0.1", Port}
    end.


get_module_json_rpc_url(Component, Module, CompSpec) ->
    case get_module_json_rpc_address(Component, Module, CompSpec) of 
	{ ok, IP, Port } when is_integer(Port)->
	    Res = "http://" ++ IP ++ ":" ++ integer_to_list(Port),
	    ?debug("get_module_json_rpc_url(~p, ~p) ->~p", [ Component, Module, Res ]),
	    Res;


	{ ok, IP, Port } when is_list(Port)->
	    Res = "http://" ++ IP ++ ":" ++ Port,
	    ?debug("get_module_json_rpc_url(~p, ~p) ->~p", [ Component, Module, Res ]),
	    Res;
	Err -> 
	    ?debug("get_module_json_rpc_url(~p, ~p) Failed: ~p", [ Component, Module, Err ]),
	    Err
    end.
	  


get_module_genserver_pid(Component, Module, CompSpec) ->
    %% Check that this is a JSON RPC module
    case get_module_type(Component, Module, CompSpec) of
	{ ok, gen_server} ->
	    %% For now, we'll just use Module
	    { ok, Module };

	{ok, json_rpc } ->
	    { error, { is_json_rpc, Module } };

	{ok, Unknown } ->
	    { error, { unknown_type, Unknown } };

	Err -> Err
    end.


start_json_rpc_server(Component, Module, Supervisor) ->
    Addr = get_module_json_rpc_address(Component, 
				       Module,
				       get_component_specification()),

    case Addr of 
	{ ok, IP, Port } ->
	    ExoHttpOpts = [ { ip, IP }, { port, Port } ],

	    exoport_exo_http:instance(Supervisor, 
				      Module,
				      ExoHttpOpts);
	Err -> 
	    ?info("rvi_common:start_json_rpc_server(~p:~p): "
		  "No JSON-RPC address setup. skip",
		  [ Component, Module ]),
	    Err
    end.
	    
