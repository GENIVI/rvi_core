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

-export([send_json_request/3]).
-export([send_json_notification/3]).
-export([request/6]).
-export([notification/5]).
-export([json_rpc_status/1]).
-export([get_json_element/2]).
-export([sanitize_service_string/1]).
-export([local_service_to_string/1]).
-export([local_service_to_string/2]).
-export([remote_service_to_string/1]).
-export([remote_service_to_string/2]).
-export([local_service_prefix/0]).
-export([node_address_string/0]).
-export([node_address_tuple/0]).
-export([node_msisdn/0]).
-export([get_request_result/1]).
-export([get_component_specification/0,
	 get_component_modules/1,
	 get_component_modules/2,
	 get_module_specification/3,
	 get_module_config/3,
	 get_module_config/4,
	 get_module_config/5,
	 get_module_json_rpc_address/3,
	 get_module_json_rpc_url/3,
	 get_module_genserver_pid/3,
	 get_value/3
	]).
-export([set_value/3]).
-export([get_log_id/1,            %% (CompSpec)
	 get_json_log_id/1,       %% (JSONArgs)
	 log_id_json_tail/1,      %% (CompSpec)
	 pick_up_json_log_id/2,   %% (JSONArgs, CompSpec)
	 pass_log_id/2]).         %% (PropList, CompSpec)
-export([utc_timestamp/0,
	 utc_timestamp/1]).
-export([bin/1,
	 take/2]).
-export([start_json_rpc_server/3,
	 start_json_rpc_server/4]).
-export([start_msgpack_rpc/2,
	 start_msgpack_rpc/3]).
-export([extract_json/2,
	 normalize_json/1,
	 term_to_json/1]).
-export([rvi_options/1]).
-export([save_source_address/3,
	 get_source_address/1]).
-export([announce/1]).

-define(NODE_SERVICE_PREFIX, node_service_prefix).
-define(NODE_ADDRESS, node_address).
-define(NODE_MSISDN, node_msisdn).

-record(pst, {
	  buffer = [],
	  balance = start,
	  in_string = false,
	  escaped = false
	 }).

json_rpc_status([I] = Str) when I >= $0, I =< $9 ->
    try json_rpc_status(list_to_integer(Str))
    catch error:_ -> undefined
    end;
json_rpc_status(I) when is_integer(I)->
    case lists:keyfind(I, 1, status_values()) of
	{_, St} -> St;
	false   -> undefined
    end;
json_rpc_status(A) when is_atom(A) ->
    case lists:keyfind(A, 2, status_values()) of
	{I, _} -> I;
	false  -> 999
    end;
json_rpc_status(L) when is_list(L) ->
    undefined.

status_values() ->
    [{0, ok},
     {1, invalid_command},
     {2, not_found},
     {3, not_available},
     {4, internal},
     {5, already_connected},
     {6, no_route},
     {7, unauthorized}].

get_request_result({ok, {http_response, {_V1, _V2}, 200, _Text, _Hdr}, JSONBody}) ->
    ?debug("JSONBody = ~p", [JSONBody]),
    case get_json_element(["result", "status"], JSONBody) of
	{ok, Value} ->
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
    lists:reverse(Acc);

json_argument([Arg | AT], [Spec | ST], Acc) when is_atom(Arg)->
    json_argument(AT, ST, [ { Spec, atom_to_binary(Arg, latin1) } | Acc]);

json_argument([Arg | AT], [Spec | ST], Acc) ->
    json_argument(AT, ST, [ { Spec, Arg } | Acc]).

%% Convert a list of unnamed arguments to a proplist
%% understood by json encode
json_argument(ArgList, SpecList) ->
    json_argument(ArgList, SpecList, []).

request(Component,
	Module,
	Function,
	InArgPropList0,
	OutArgSpec,
	CompSpec) ->

    %% Split [ { network_address, "127.0.0.1:888" } , { timeout, 34 } ] to
    %% [ "127.0.0.1:888", 34] [ network_address, timeout ]
    InArgPropList = pass_log_id(InArgPropList0, CompSpec),
    InArg = [ Val || { _Key, Val } <- InArgPropList ],
    InArgSpec = [ Key || { Key, _Val } <- InArgPropList ],
    %% Figure out how we are to invoke this MFA.
    case get_module_type(Component, Module, CompSpec) of
	%% We have a gen_server
	{ ok, gen_server } ->
	    ?debug("Sending ~p - ~p:~p(~p)", [Component, Module, Function,
					      authorize_keys:abbrev_payload(InArg)]),
	    gen_server:call(Module, { rvi, Function, InArg});

	%% We have a JSON-RPC server
	{ ok,  json_rpc } ->
	    URL = get_module_json_rpc_url(Component, Module, CompSpec),
	    ?debug("Sending ~p:~p(~p) -> ~p.", [Module, Function, InArg, URL]),
	    JSONArg = json_argument(InArg, InArgSpec),
	    ?debug("Sending ~p:~p(~p) -> ~p.", [Module, Function, InArg, JSONArg]),

	    case get_request_result(
		   send_json_request(URL, atom_to_binary(Function, latin1),  JSONArg)
		  ) of

		{ ok, JSONBody} ->
		    json_reply(OutArgSpec, JSONBody);

		Err -> Err
	    end;

	Err1 -> Err1
    end.

notification(Component,
	     Module,
	     Function,
	     InArgPropList0,
	     CompSpec) ->

    %% Split [ { network_address, "127.0.0.1:888" } , { timeout, 34 } ] to
    %% [ "127.0.0.1:888", 34] [ network_address, timeout ]
    InArgPropList = pass_log_id(InArgPropList0, CompSpec),
    InArg = [ Val || { _Key, Val } <- InArgPropList ],
    InArgSpec = [ Key || { Key, _Val } <- InArgPropList ],
    %% Figure out how we are to invoke this MFA.
    ?debug("notify [~w]~w:~w - ~p", [Component, Module, Function,
				     InArgPropList]),

    case get_module_type(Component, Module, CompSpec) of
	%% We have a gen_server
	{ ok, gen_server } ->
	    ?debug("via gen_server (~p)", [Module]),
	    gen_server:cast(Module, { rvi, Function, InArg}),
	    ok;

	%% We have a JSON-RPC server
	{ ok,  json_rpc } ->
	    URL = get_module_json_rpc_url(Component, Module, CompSpec),
	    ?debug("Sending via URL=~p", [URL]),
	    JSONArg = json_argument(InArg, InArgSpec),
	    send_json_notification(URL, atom_to_binary(Function, latin1),  JSONArg),
	    ok;
	{ error, _ } = Error ->
	    ?warning("get_module_type(~p,~p,~p) -> ~p",
		     [Component, Module, CompSpec, Error]),
	    %% ignore
	    ok
    end.

pass_log_id(PList, CompSpec) ->
    case get_value(rvi_log_id, undefined, CompSpec) of
	undefined ->
	    PList;
	ID ->
	    lists:keystore(rvi_log_id, 1, PList, {rvi_log_id, ID})
    end.

send_json_request(Url,Method, Args) ->

    Req = jsx_encode([{<<"jsonrpc">>, <<"2.0">>},
		      {<<"id">>, 1},
		      {<<"method">>,  Method},
		      {<<"params">>, normalize_json(Args)}
		     ]),

    Hdrs = [{'Content-Type', "application/json"} ],
    ?debug("rvi_common:send_json_request() Sending:      ~p", [Req]),
    try
        exo_http:wpost(Url, {1,1}, Hdrs, Req, 1000)
    catch
        Type:Reason ->
            ?error("rvi_common:send_json_request() CRASHED: URL:      ~p", [Url]),
            ?error("rvi_common:send_json_request() CRASHED: Hdrs:     ~p", [Hdrs]),
            ?error("rvi_common:send_json_request() CRASHED: Body:     ~p", [Req]),
            ?error("rvi_common:send_json_request() CRASHED: Type:     ~p", [Type]),
            ?error("rvi_common:send_json_request() CRASHED: Reason:   ~p", [Reason]),
            ?error("rvi_common:send_json_request() CRASHED: Stack:    ~p", [ erlang:get_stacktrace()]),
            {error, internal}
    end.

jsx_encode(J) ->
    jsx:encode(normalize_json(J)).

send_json_notification(Url,Method, Args) ->

    Req = jsx_encode([{<<"jsonrpc">>, <<"2.0">>},
		      {<<"method">>,  Method},
		      {<<"params">>, normalize_json(Args)}
		     ]),

    Hdrs = [{'Content-Type', "application/json"} ],
    %%?debug("rvi_common:send_json_notification() Sending:      ~p", [Req]),
    try
        exo_http:wpost(Url, {1,1}, Hdrs, Req, 1000)
    catch
        Type:Reason ->
            ?error("rvi_common:send_json_notification() CRASHED: URL:      ~p", [Url]),
            ?error("rvi_common:send_json_notification() CRASHED: Hdrs:     ~p", [Hdrs]),
            ?error("rvi_common:send_json_notification() CRASHED: Body:     ~p", [Req]),
            ?error("rvi_common:send_json_notification() CRASHED: Type:     ~p", [Type]),
            ?error("rvi_common:send_json_notification() CRASHED: Reason:   ~p", [Reason]),
            ?error("rvi_common:send_json_notification() CRASHED: Stack:    ~p", [ erlang:get_stacktrace()]),
            {error, internal}
    end.

rvi_options(Opts) when is_list(Opts) ->
    [{K,V} || {K,V} <- Opts,
	      is_rvi_opt(K)].

is_rvi_opt(K) ->
    case re:run(K, <<"^rvi\\.">>, []) of
	{match, _} ->
	    true;
	nomatch ->
	    false
    end.

term_to_json(Term) ->
    jsx:encode(normalize_json(Term)).

normalize_json(J) ->
    {ok, JSX} = msgpack:unpack(
		  msgpack:pack(unstruct(J), [jsx,
					     {allow_atom,pack}]), [jsx]),
    JSX.

unstruct({struct, Elems}) -> unstruct(Elems);
unstruct({array, Elems})  -> unstruct(Elems);
unstruct([_|_] = Elems)   -> [unstruct(X) || X <- Elems];
unstruct(X) ->
    X.


%% If Path is just a single element, convert to list and try again.
get_json_element(_, []) ->
    {error, undefined};
get_json_element(ElemPath, JSON) when is_atom(ElemPath) ->
    get_json_element([ElemPath], JSON);

get_json_element(ElemPath, JSON) when is_binary(JSON) ->
    get_json_element(ElemPath, binary_to_list(JSON));

get_json_element(ElemPath, JSON) when is_tuple(JSON) ->
    get_json_element_(ElemPath, JSON);

get_json_element(ElemPath, [T|_] = JSON) when is_tuple(T) ->
    get_json_element_(ElemPath, JSON);
get_json_element(ElemPath, [H|_] = JSON) when is_integer(H) ->
    case  exo_json:decode_string(JSON) of
	{ok,  Data } ->
	    get_json_element_(ElemPath, Data);

	Err ->
	    Err
    end;

get_json_element(P, J) ->
    ?warning("get_json_element(): Unknown call structure; Path: ~p | JSON: ~p",
	      [P, J]),
    {error, {call, {P, J}}}.

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
    get_json_element_([atom_to_binary(Elem, latin1) | T], JSON);

get_json_element_(Path, {Type, JSON}) when Type==array;
					   Type==struct ->
    get_json_element_(Path, JSON);
get_json_element_([Elem | T], JSON) ->
    case Elem of
	{'OR', Alts} ->
	    get_json_element_(T, get_json_element_alt(Alts, JSON));
	_ ->
	    get_json_element_(T, get_json_value(Elem, JSON, undefined))
    end;

get_json_element_(Path,JSON) ->
    ?warning("get_json_element_(): Unhandled: Path: ~p | JSON: ~p",
	     [Path, JSON]),
    { error, undefined }.

get_json_element_alt(Alts, [{K, V}|T]) ->
    case member(K, Alts) of
	true  -> V;
	false -> get_json_element_alt(Alts, T)
    end;
get_json_element_alt(_, []) ->
    undefined.

member(K, [H|T]) ->
    case comp(K, H) of
	true -> true;
	false -> member(K, T)
    end;
member(_, []) ->
    false.

get_json_value(K, [{K1,V}|T], Def) ->
    case comp(K, K1) of
	true -> V;
	false -> get_json_value(K, T, Def)
    end;
get_json_value(K, [_|T], Def) ->
    get_json_value(K, T, Def);
get_json_value(_, [], Def) ->
    Def.

comp(A, A) -> true;
comp(A, B) when is_list(A), is_binary(B) ->
    list_to_binary(A) =:= B;
comp(A, B) when is_binary(A), is_list(B) ->
    A =:= list_to_binary(B);
comp(_, _) ->
    false.

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

bin(L) ->
    iolist_to_binary(L).

local_service_to_string(Type, Svc) ->
    bin([Type, ":", local_service_to_string(Svc)]).


%% Make sure we don't get two slashes between the prefix and the service name
local_service_to_string(<<"/", Service/binary>>) ->
    bin([local_service_prefix(), Service]);
local_service_to_string([ $/ | Service]) ->
    bin([local_service_prefix(), Service]);

%% Make sure we don't get two slashes
local_service_to_string(Svc) ->
    bin([local_service_prefix(), Svc]).

remote_service_to_string(Service) ->
    Service.

remote_service_to_string(Type, Service) ->
    bin([Type, ":", Service]).

local_service_prefix() ->
    Prefix =
	case application:get_env(rvi_core, ?NODE_SERVICE_PREFIX) of
	    {ok, P} when is_atom(P) -> atom_to_binary(P, latin1);
	    {ok, P} when is_list(P) -> iolist_to_binary(P);
	    {ok, P} when is_binary(P) -> P;
	    undefined ->
		?debug("WARNING: Please set application rvi environment ~p",
			  [?NODE_SERVICE_PREFIX]),
		error({missing_env, ?NODE_SERVICE_PREFIX})
	end,

    %% Tag on a trailing slash if not there already.
    case last(Prefix) of
	$/ -> bin(Prefix);

	_ -> bin([Prefix, "/"])
    end.

last(L) when is_list(L) ->
    lists:last(L);
last(B) when is_binary(B) ->
    Sz = byte_size(B)-1,
    <<_:Sz/binary, Last>> = B,
    Last.

node_address_string() ->
    case application:get_env(rvi_core, ?NODE_ADDRESS) of
	{ok, P} when is_atom(P) -> atom_to_binary(P, latin1);
	{ok, P} when is_list(P) -> list_to_binary(P);
	{ok, P} when is_binary(P) -> P;
	undefined ->
	    ?warning("WARNING: Please set application rvi environment ~p",
		      [?NODE_ADDRESS]),
	    error({missing_env, ?NODE_ADDRESS})
    end.

node_address_tuple() ->
    case node_address_string() of
	{missing_env, _} = Err -> Err;
	Addr ->
	    [ Address, Port ] = re:split(Addr,":",[{return,binary}]),
	    { Address, binary_to_integer(Port) }
    end.

node_msisdn() ->
    case application:get_env(rvi_core, ?NODE_MSISDN) of
	{ok, M} when is_list(M) -> M;
	undefined ->
	    ?warning("WARNING: Please set application rvi environment ~p",
		     [?NODE_MSISDN]),
	    error({missing_env, ?NODE_MSISDN})
    end.

get_component_config_(Component, Default, CompList) ->
    case proplists:get_value(Component, CompList, undefined) of
	undefined ->
	    %% ?debug("get_component_config(~p): Default: ~p",
	    %% 	   [Component, Default]),
	     Default;

	ModList ->
	    %% ?debug("get_component_config(~p) -> ~p",
	    %% 	   [Component, ModList]),
	    ModList
    end.

get_component_specification() ->
    CS = get_component_specification_(),
    %% lager:debug("CompSpec = ~p", [CS]),
    CS.

get_component_specification_() ->
    case application:get_env(rvi_core, components, undefined) of
	undefined ->
	    #component_spec {
	       service_edge = ?COMP_SPEC_SERVICE_EDGE_DEFAULT,
	       schedule = ?COMP_SPEC_SCHEDULE_DEFAULT,
	       service_discovery = ?COMP_SPEC_SERVICE_DISCOVERY_DEFAULT,
	       authorize = ?COMP_SPEC_AUTHORIZE_DEFAULT,
	       data_link = ?COMP_SPEC_DATA_LINK_DEFAULT,
	       protocol = ?COMP_SPEC_PROTOCOL_DEFAULT,
	       rvi_common = ?COMP_SPEC_RVI_COMMON_DEFAULT
	      };

	CompList ->
	    #component_spec {
	       service_edge = get_component_config_(service_edge,
						    ?COMP_SPEC_SERVICE_EDGE_DEFAULT,
						    CompList),
	       schedule = get_component_config_(schedule,
						 ?COMP_SPEC_SCHEDULE_DEFAULT,
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
						 CompList),
	       rvi_common = get_component_config_(rvi_common,
						  ?COMP_SPEC_RVI_COMMON_DEFAULT,
						  CompList)
	      }
    end.


get_component_modules(Component) ->
    get_component_modules(Component, get_component_specification()).

get_component_modules(service_edge, CompSpec) ->
    CompSpec#component_spec.service_edge;

get_component_modules(schedule, CompSpec) ->
    CompSpec#component_spec.schedule;

get_component_modules(service_discovery, CompSpec) ->
    CompSpec#component_spec.service_discovery;

get_component_modules(authorize, CompSpec) ->
    CompSpec#component_spec.authorize;

get_component_modules(data_link, CompSpec) ->
    CompSpec#component_spec.data_link;

get_component_modules(protocol, CompSpec) ->
    CompSpec#component_spec.protocol;

get_component_modules(rvi_common, CompSpec) ->
    CompSpec#component_spec.rvi_common;

get_component_modules(_, _) ->
    undefined.

%% Get the spec for a specific module (protocol_bert_rpc) within
%% a component (protocol).
get_module_specification(Component, Module, CompSpec) ->
    case get_component_modules(Component, CompSpec) of
	undefined ->
	    ?debug("get_module_specification(): Missing: rvi_core:component: ~p~nCS = ~p",
		   [Component, CompSpec]),
	    undefined;

	Modules ->
	    case lists:keyfind(Module, 1, Modules ) of
		false ->
		    ?debug("get_module_specification(): Missing component spec: "
			   "rvi_core:component:~p:~p:{...}: ~p", [Component, Module, Modules]),
		    {error, {not_found, Module}};

		{ Module, Type, ModConf } ->
		    %% ?debug("get_module_specification(): ~p:~p -> ~p ",
		    %% 	   [Component, Module, { Module, Type, ModConf}]),
		    {ok, Module, Type, ModConf };

		IllegalFormat ->
		    ?warning("get_module_specification(): Illegal format: ~p: ~p",
			     [Module, IllegalFormat]),
		    {error, {illegal_format,{ Module, IllegalFormat } } }
	    end
    end.

get_module_config(Component, Module, Key) ->
    get_module_config(Component, Module, Key, get_component_specification()).

%% Get a specific option (bert_rpc_port) for a specific module
%% (protocol_bert_rpc) within a component (protocol).
get_module_config(Component, Module, Key, CompSpec) ->
    case get_module_specification(Component, Module, CompSpec) of
	{ok, _Module, _Type, ModConf } ->
	    case proplists:get_value(Key, ModConf, undefined ) of
		undefined ->
		    ?debug("get_module_config(): Missing component spec: "
			   "~p:~p:~p{...}: ~p",
			   [Component, Module, Key, ModConf]),
		    {error, {not_found, Component, Module, Key}};


		Config ->
		    ?debug("get_module_config(): ~p:~p:~p -> ~p: ",
			   [Component, Module, Key, Config]),
		    {ok, Config }
	    end;
	Err ->
	    ?debug("get_module_config(): ~p:~p:~p: Failed: ~p ",
		   [Component, Module, Key, Err]),

	    Err
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

set_value(Key, Value, #component_spec{values = Keys} = CompSpec) ->
    CompSpec#component_spec{values = lists:keystore(Key, 1, Keys, {Key, Value})}.

get_value(Key, Default, #component_spec{values = Values}) ->
    case lists:keyfind(Key, 1, Values) of
	{_, Value} ->
	    Value;
	false ->
	    Default
    end.

get_log_id(CS) ->
    get_value(rvi_log_id, <<"null">>, CS).

log_id_json_tail(CS) ->
    case get_value(rvi_log_id, undefined, CS) of
	undefined ->
	    [];
	Id ->
	    [{<<"rvi_log_id">>, Id}]
    end.

get_json_log_id(Args) ->
    case get_json_element([<<"rvi_log_id">>], Args) of
	{ok, ID} ->
	    ID;
	{error, _} ->
	    <<"null">>
    end.

pick_up_json_log_id(Args, CS) ->
    case get_json_element([<<"rvi_log_id">>], Args) of
	{ok, ID} ->
	    set_value(rvi_log_id, ID, CS);
	{error, _} ->
	    CS
    end.

get_module_type(Component, Module, CompSpec) ->
    case get_module_specification(Component, Module, CompSpec) of
	{ok, _Module, Type, _ModConf } ->
	    {ok, Type} ;

	Err -> Err
    end.

get_module_json_rpc_address(Component, Module, CompSpec) ->
    %% Dig out the JSON RPC address
    get_module_rpc_address(json, Component, Module, CompSpec).
    %% case get_module_config(Component,
    %% 			   Module,
    %% 			   json_rpc_address,
    %% 			   undefined,
    %% 			   CompSpec) of
    %% 	{ok, undefined } ->
    %% 	    ?debug("get_module_json_rpc_address(): Missing component spec: "
    %% 		   "rvi_core:component:~p:~p:json_rpc_address, {...}", [Component, Module]),
    %% 	    {error, {not_found, Component, Module, json_rpc_address}};

    %% 	{ok, { IP, Port }} ->
    %% 	    ?debug("get_module_json_rpc_address(~p, ~p) -> ~p:~p",
    %% 		   [ Component, Module, IP, Port]),
    %% 	    {ok,  bin(IP), Port };

    %% 	{ok, Port } ->
    %% 	    ?debug("get_module_json_rpc_address(~p, ~p) -> 127.0.0.1:~p",
    %% 		   [ Component, Module, Port]),
    %% 	    {ok,   <<"127.0.0.1">>, Port}
    %% end.


get_module_rpc_address(Type, Component, Module, CompSpec)
  when Type == json; Type == msgpack ->
    %% Dig out the JSON/MsgPack RPC address
    Key = case Type of
	      json    -> json_rpc_address;
	      msgpack -> msgpack_rpc_address
	  end,
    case get_module_config(Component,
			   Module,
			   Key,
			   undefined,
			   CompSpec) of
	{ok, undefined } ->
	    ?debug("get_module_rpc_address(): Missing component spec: "
		   "rvi_core:components:~p:~p:~s, {...}",
		   [Component, Module, Key]),
	    {error, {not_found, Component, Module, Key}};

	{ok, { IP, Port }} ->
	    ?debug("get_module_rpc_address(~p, ~p, ~p) -> ~p:~p",
		   [Type, Component, Module, IP, Port]),
	    {ok,  bin(IP), Port };

	{ok, Port } ->
	    ?debug("get_module_rpc_address(~p, ~p, ~p) -> 127.0.0.1:~p",
		   [Type, Component, Module, Port]),
	    {ok,   <<"127.0.0.1">>, Port}
    end.


get_module_json_rpc_url(Component, Module, CompSpec) ->
    get_module_rpc_url(json, Component, Module, CompSpec).

get_module_rpc_url(Type, Component, Module, CompSpec)
  when Type == json; Type == msgpack ->
    case get_module_rpc_address(Type, Component, Module, CompSpec) of
	{ ok, IP, Port } when is_integer(Port)->
	    Res = bin(["http://", IP, ":", integer_to_binary(Port)]),
	    ?debug("get_module_rpc_url(~p, ~p, ~p) ->~p", [Type, Component, Module, Res ]),
	    Res;
	{ ok, IP, Port } when is_list(Port)->
	    Res = bin(["http://", IP, ":", Port]),
	    ?debug("get_module_rpc_url(~p, ~p, ~p) ->~p", [Type, Component, Module, Res ]),
	    Res;
	Err ->
	    ?debug("get_module_rpc_url(~p, ~p, ~p) Failed: ~p", [Type, Component, Module, Err ]),
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
    start_json_rpc_server(Component, Module, Supervisor, []).

start_json_rpc_server(Component, Module, Supervisor, XOpts) ->
    Addr = get_module_json_rpc_address(Component,
				       Module,
				       get_component_specification()),

    case Addr of
	{ ok, IP, Port } ->
	    ExoHttpOpts = [ { ip, IP }, { port, Port } | XOpts ],

	    exoport_exo_http:instance(Supervisor,
				      Module,
				      ExoHttpOpts);
	Err ->
	    ?debug("start_json_rpc_server(~p:~p): "
		   "No JSON-RPC address setup. skip",
		   [ Component, Module ]),
	    Err
    end.

start_msgpack_rpc(Component, Module) ->
    start_msgpack_rpc(Component, Module, []).

start_msgpack_rpc(Component, Module, XOpts) ->
    ?debug("start_msgpack_rpc(~w, ~w, ~p)", [Component, Module, XOpts]),
    case get_module_rpc_address(msgpack, Component, Module, get_component_specification()) of
	{ok, {client, Opts}} ->
	    ?debug("starting msgpack_rpc client: ~p", [Opts]),
	    start_msgpack_rpc_client(Component, Module, Opts, XOpts);
	{ok, {server, Opts}} ->
	    ?debug("starting msgpack_rpc server: ~p", [Opts]),
	    start_msgpack_rpc_server(Component, Module, Opts, XOpts);
	{ok, {IP, Port}} ->
	    start_msgpack_rpc_server(Component, Module, [{ip, IP}, {port, Port}], XOpts);
	Error ->
	    ?debug("no recognized msgpack config for ~w:~w (~p)",
		   [Component, Module, Error])
    end.

start_msgpack_rpc_client(Component, Module, Opts, XOpts) ->
    Name = {msgpack_rpc_client, Component, Module},
    rvi_msgpack_rpc:start_link([{gproc, {n,l,Name}}|XOpts] ++ Opts).

start_msgpack_rpc_server(Component, Module, Opts, XOpts) ->
    Name = {msgpack_rpc_server, Component, Module},
    [Callback, Rest] = take([{callback, fun() -> msgpack_rpc_cb(Module) end}],
			    XOpts ++ Opts),
    rvi_msgpack_rpc_server:start_link([{callback, Callback} | Rest]).

msgpack_rpc_cb(Module) ->
    binary_to_existing_atom(
      <<(atom_to_binary(Module, latin1))/binary, "_msgpack">>, latin1).

utc_timestamp() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:universal_time()) - seconds_jan_1970().

utc_timestamp({_,_,_} = TS) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(TS)) - seconds_jan_1970().

seconds_jan_1970() ->
    %% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
    62167219200.



count_brackets([],
	       #pst {
		  buffer = [],
		  balance = start } = PSt)  ->
    { incomplete, PSt#pst {}};

count_brackets([],
	       #pst {
		  buffer = Buffer,
		  balance = start } = PSt)  ->
    count_brackets(Buffer,
		   PSt#pst {
		     buffer = [],
		     balance = start } );
count_brackets([${ | Rem],
	       #pst {
		  buffer = Buffer,
		  balance = start } = PSt)  ->
    count_brackets(Rem,
		   PSt#pst{
		     buffer = [ ${ | Buffer ],
		     balance = 1});

%% Drop any initial characters prior to opening bracket
count_brackets([_ | Rem],
	       #pst { balance = start } = PSt)  ->
    count_brackets(Rem, PSt );

%% If balance is back to zero, we have completed a JSON
%% element.
count_brackets(Rem,
	       #pst {
		  buffer = Buffer,
		  balance = 0 } = PSt) ->

    { complete, lists:reverse(Buffer),
      PSt#pst {
	buffer = Rem,
	balance = start
       }
    };

%% If we still have balance, but no more input
%% we have an incomplete element.x
count_brackets([], PSt) ->
    { incomplete,  PSt };


%% We have a string start or end, and we are not esacped
%% Flip our in-string state
count_brackets([$" | Rem],
	       #pst {
		  buffer = Buffer,
		  in_string = InString,
		  escaped = false} = PSt) ->

    count_brackets(Rem, PSt#pst {
			  buffer = [ $" | Buffer ],
			  in_string = not InString });


%% We have an escape character, and we are in a string. Turn on our escape state
count_brackets([$\\ | Rem],
	       #pst {
		  buffer = Buffer,
		  in_string = true,
		  escaped = false } = PSt) ->

    count_brackets(Rem, PSt#pst {
			  buffer = [ $\\ | Buffer ],
			  escaped = true});

%% We have an opening bracket and we are not in a string
count_brackets([${ | Rem],
	       #pst {
		  buffer = Buffer,
		  balance = Balance,
		  in_string = false } = PSt) ->

    count_brackets(Rem,
		   PSt#pst {
		     buffer = [ ${ | Buffer ],
		     balance = Balance + 1});

%% We have an closing bracket and we are not in a string
count_brackets([$} | Rem],
	       #pst {
		  buffer = Buffer,
		  balance = Balance,
		  in_string = false } = PSt) ->

    count_brackets(Rem,
		   PSt#pst {
		     buffer = [ $} | Buffer ],
		     balance = Balance - 1});

%% We have just regular data to feed over.
%% Make sure to clear the escape state.
count_brackets([C | Rem],
	       #pst { buffer = Buffer } = PSt) ->

    count_brackets(Rem, PSt#pst {
			  buffer = [ C | Buffer ],
			  escaped = false
			 } ).

extract_json(Buf, PST, Acc) ->
    case count_brackets(Buf, PST) of
	{ complete, Processed, NPST} ->
	    extract_json([], NPST, [ Processed | Acc]);


	{ incomplete, NPST} ->
	    { Acc, NPST }
    end.

extract_json(Buf, undefined) ->
    extract_json(Buf, #pst {},[]);

extract_json(Buf, PST) ->
    extract_json(Buf, PST,[]).

announce(Name) ->
    ?debug("Announce ~p~n", [Name]),
    gproc:reg(Name),
    ok.

%% inet_ip(IP) when is_binary(IP) ->
%%     inet_ip(binary_to_list(IP));
%% inet_ip(IP) ->
%%     {ok, Addr} = inet:ip(IP),
%%     Addr.


%% take([ Key::atom() | {Key::atom(), Default} ], Opts) -> [Value | Rest]
take([H|T], Opts) when is_atom(H) ->
    case lists:keytake(H, 1, Opts) of
        {value, {_, Value}, Rest} ->
            [Value | take(T, Rest)];
        false ->
            error({required, H})
    end;
take([{H,Default}|T], Opts) ->
    case lists:keytake(H, 1, Opts) of
        {value, {_, Value}, Rest} ->
            [Value | take(T, Rest)];
	false when is_function(Default, 0) ->
	    [Default() | take(T, Opts)];
        false ->
            [Default | take(T, Opts)]
    end;
take([], Opts) ->
    [Opts].

save_source_address(client, Socket, CS) ->
    {ok, {_, _} = Addr} = inet:peername(Socket),
    set_value(source_address, Addr, CS);
save_source_address(server, Socket, CS) ->
    {ok, {_, _} = Addr} = inet:sockname(Socket),
    set_value(source_address, Addr, CS).

get_source_address(CS) ->
    get_value(source_address, undefined, CS).
