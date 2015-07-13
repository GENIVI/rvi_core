%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2006 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% File    : bt_sdp.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : SDP functions
%%% Created : 28 May 2006 by Tony Rogvall <tony@PBook.local>

-module(bt_sdp).

-export([attribute_to_string/1, attribute_to_string/2,  attribute_to_string/3]).
-export([string_to_attribute/1, string_to_attribute/2]).
-export([value_to_string/1]).
-export([uuid_to_string/1, string_to_uuid/1]).

-export([is_value/1, encode_value/1, decode_value/1, decode_value/2]).
-export([fold_value/3]).
-export([encode/2, decode/1]).
-export([uuid_128/1]).
-export([decode_sdp_value/1]).

%% Native Client SDP/L2CAP API
-export([service_search/2,
	 service_attribute/3,
	 service_search_attribute/3
	]).
-export([encode_pdu/2,encode_pdu/3,
	 decode_pdu/1]).

-import(lists, [reverse/1, map/2]).

-include("../include/bt.hrl").
-include("../include/sdp.hrl").


-define(SDP_NIL,         0).
-define(SDP_UNSIGNED,    1).
-define(SDP_SIGNED,      2).
-define(SDP_UUID,        3).
-define(SDP_TEXT,        4).
-define(SDP_BOOLEAN,     5).
-define(SDP_SEQUENCE,    6).
-define(SDP_ALTERNATIVE, 7).
-define(SDP_URL,         8).


%% L2CAP implementation of SDP query (will be)
service_search(Address, UUIDList) ->
    SearchPattern = cvt_uuid_list(UUIDList),
    PDU = #sdpServiceSearchRequest { serviceSearchPattern=SearchPattern,
				     maximumServiceRecordCount=16#ffff
				   },
    sdp_request(Address, PDU).

service_attribute(Address, Handle, AttributeList) ->
    AttributeIDList= cvt_attribute_id_list(AttributeList),
    PDU = #sdpServiceAttributeRequest { serviceRecordHandle=Handle,
					maximumAttributeByteCount=16#ffff,
					attributeIDList=AttributeIDList },
    sdp_request(Address, PDU).
    
service_search_attribute(Address, UUIDList, AttributeList) ->
    SearchPattern = cvt_uuid_list(UUIDList),
    AttributeIDList= cvt_attribute_id_list(AttributeList),
    PDU=#sdpServiceSearchAttributeRequest { serviceSearchPattern=SearchPattern,
					    maximumAttributeByteCount=16#ffff,
					    attributeIDList=AttributeIDList },
    sdp_request(Address, PDU).
    

sdp_request(Address, PDU) ->
    case l2cap:open(Address, ?L2CAP_PSM_SDP) of
	{ok,L2CAP} ->
	    Result = sdp_l2cap_request(L2CAP,<<>>,[],PDU),
	    l2cap:close(L2CAP),
	    Result;
	Error -> Error
    end.

sdp_l2cap_request(L2CAP,Continuation,Acc,Request) ->
    Transaction = next_transaction_id(),
    io:format("Transaction ~w\n", [Transaction]),
    Bin = encode_pdu(Transaction,Continuation,Request),
    l2cap:send(L2CAP, Bin),
    receive
	{l2cap, L2CAP, {data,Data}} ->
	    case decode_pdu(Data,Acc) of
		{ok,{Transaction,<<>>,_Acc1,
		     #sdpErrorResponse { errorCode=ErrorCode,
					 errorInfo=ErrorInfo }}} ->
		    {error,{ErrorCode,ErrorInfo}};
		{ok,{Transaction,<<>>,_Acc1,Response}} ->
		    {ok,Response};
		{ok,{Transaction,Continuation1,Acc1,_Response}} ->
		    io:format("get more\n",[]),
		    sdp_l2cap_request(L2CAP,Continuation1,Acc1,Request);
		Error ->
		    Error
	    end;
	{l2cap, L2CAP, closed} ->
	    {error, closed}
    end.
	    
%%
%% Encode PDU
%%
encode_pdu(Transaction,PDU) ->
    encode_pdu(Transaction,<<>>,PDU).

encode_pdu(Transaction,Continuation,
	   #sdpServiceSearchRequest
	   { serviceSearchPattern=ServiceSearchPattern,
	     maximumServiceRecordCount=MaximumServiceRecordCount
	    })  ->
    EServiceSearchPattern = encode_value(ServiceSearchPattern),
    ParameterLen = size(EServiceSearchPattern)+2+1+size(Continuation),
    <<?SDP_ServiceSearchRequest,
     Transaction:16/unsigned-integer,
     ParameterLen:16/unsigned-integer,
     EServiceSearchPattern/binary,
     MaximumServiceRecordCount:16/unsigned-integer,
     (size(Continuation)):8/unsigned-integer, Continuation/binary>>;
encode_pdu(Transaction,Continuation,
	   #sdpServiceSearchResponse
	   { totalServiceRecordCount=TotalServiceRecordCount,
	     currentServiceRecordCount=CurrentServiceRecordCount,
	     serviceRecordHandleList=Handles
	    }) ->
    ParameterLen = 2+2+4*length(Handles),
    ServiceRecordHandleList = 
	list_to_binary(map(fun(H) -> <<H:32/unsigned-integer>> end, Handles)),
    <<?SDP_ServiceSearchResponse,
     Transaction:16/unsigned-integer,
     ParameterLen:16/unsigned-integer,
     TotalServiceRecordCount:16/unsigned-integer,
     CurrentServiceRecordCount:16/unsigned-integer,
     ServiceRecordHandleList/binary,
     (size(Continuation)):8, Continuation/binary>>;
encode_pdu(Transaction,Continuation,
	   #sdpServiceAttributeRequest
	   { serviceRecordHandle=Handle,
	     maximumAttributeByteCount=MaximumAttributeByteCount,
	     attributeIDList=AttributeIDList
	    }) ->
    EAttributeIDList = encode_value(AttributeIDList),
    ParameterLen = 4+2+size(EAttributeIDList)+1+size(Continuation),
    <<?SDP_ServiceAttributeRequest,
     Transaction:16/unsigned-integer,
     ParameterLen:16/unsigned-integer,
     Handle:32/unsigned-integer,
     MaximumAttributeByteCount:16/unsigned-integer,     
     EAttributeIDList/binary,
     (size(Continuation)):8/unsigned-integer, Continuation/binary>>;
encode_pdu(Transaction,Continuation,
	   #sdpServiceAttributeResponse 
	   { attributeListByteCount=AttributeListByteCount,
	     attributeList=AttributeList }) ->
    ParameterLen = 2 + size(AttributeList)+1+size(Continuation),
    <<?SDP_ServiceAttributeResponse,
     Transaction:16/unsigned-integer,
     ParameterLen:16/unsigned-integer,
     AttributeListByteCount:16/unsigned-integer,
     AttributeList/binary,
     (size(Continuation)):8, Continuation/binary>>;
encode_pdu(Transaction,Continuation,
	   #sdpServiceSearchAttributeRequest
	   { serviceSearchPattern=ServiceSearchPattern,
	     maximumAttributeByteCount=MaximumAttributeByteCount,
	     attributeIDList=AttributeIDList
	    }) ->
    EServiceSearchPattern = encode_value(ServiceSearchPattern),
    EAttributeIDList = encode_value(AttributeIDList),    
    ParameterLen = size(EServiceSearchPattern)+2+
	size(EAttributeIDList)+1+size(Continuation),
    <<?SDP_ServiceSearchAttributeRequest,
     Transaction:16/unsigned-integer,
     ParameterLen:16/unsigned-integer,
     EServiceSearchPattern/binary,
     MaximumAttributeByteCount:16/unsigned-integer,
     EAttributeIDList/binary,
     (size(Continuation)):8/unsigned-integer, Continuation/binary>>;
encode_pdu(Transaction,Continuation,
	   #sdpServiceSearchAttributeResponse 
	   { attributeListByteCount=AttributeListByteCount,
	     attributeList=AttributeList }) ->
    ParameterLen = 2+size(AttributeList)+1+size(Continuation),
    <<?SDP_ServiceSearchAttributeResponse,
     Transaction:16/unsigned-integer,
     ParameterLen:16/unsigned-integer,
     AttributeListByteCount:16/unsigned-integer,
     AttributeList/binary,
     (size(Continuation)):8, Continuation/binary>>;
encode_pdu(Transaction,_Continuation,
	   #sdpErrorResponse { errorCode=ECode,
			       errorInfo=ErrorInfo }) ->
    ErrorCode = 
	case ECode of
	    invalid_version -> ?SDP_ErrorInvalidVersion;
	    invalid_handle -> ?SDP_ErrorInvalidHandle;
	    invalid_syntax -> ?SDP_ErrorInvalidSyntax;
	    invalid_pdu_size -> ?SDP_ErrorInvaludPduSize;
	    invalid_continuation -> ?SDP_ErrorInvalidContinuation;
	    insufficient_resources ->?SDP_ErrorInsufficientResources;
	    _ when is_integer(ECode) -> ECode
	end,
    ParameterLen = 2+size(ErrorInfo),
    <<?SDP_ErrorResponse,
     Transaction:16/unsigned-integer,
     ParameterLen:16/unsigned-integer,
     ErrorCode:16/unsigned-integer,
     ErrorInfo/binary>>;

encode_pdu(_Transaction,_Continuation,PDU) ->
    %% add response pdus.
    erlang:error({bad_pdu, PDU}).

decode_pdu(Bin) ->
    decode_pdu(Bin, []).

decode_pdu(<<ID,
	    Transaction:16/unsigned-integer,
	    ParameterLen:16/unsigned-integer,
	    Parameters:ParameterLen/binary>>, Acc) ->
    case catch decode_pdu(ID,Transaction,Parameters,Acc) of
	{'EXIT',_} ->
	    {error,invalid_request_syntax};
	Result -> Result
    end;
decode_pdu(PDU, _Acc) ->
    erlang:error({bad_pdu,PDU}).

%% decode pdu return {Transaction,Continuation,Pdu}
decode_pdu(?SDP_ServiceSearchRequest,Transaction,Parameters,_Acc) ->
    PatternSize = sizeof(Parameters,0),

    <<Patterns:PatternSize/binary,
     MaximumServiceRecordCount:16/unsigned-integer,
     CSize:8, Continuation:CSize/binary>> = Parameters,

    {ok,{Transaction,Continuation
	 #sdpServiceSearchRequest 
	 { serviceSearchPattern=decode(Patterns),
	   maximumServiceRecordCount=MaximumServiceRecordCount}}};

decode_pdu(?SDP_ServiceSearchResponse,Transaction,
	   <<TotalServiceRecordCount:16/unsigned-integer,
	    CurrentServiceRecordCount:16/unsigned-integer,
	    ServiceRecordHandleList:CurrentServiceRecordCount/binary-unit:32,
	    CSize:8, Continuation:CSize/binary>>, Acc) ->
    HandleList = uint32_list(ServiceRecordHandleList),
    if Continuation == <<>> ->
	    {ok,{Transaction,Continuation,[],
		 #sdpServiceSearchResponse 
		 { totalServiceRecordCount=TotalServiceRecordCount,
		   currentServiceRecordCount=CurrentServiceRecordCount,
		   serviceRecordHandleList=Acc++HandleList}}};
       true ->
	    {ok,{Transaction,Continuation,Acc++HandleList,
		 #sdpServiceSearchResponse 
		 { totalServiceRecordCount=TotalServiceRecordCount,
		   currentServiceRecordCount=CurrentServiceRecordCount,
		   serviceRecordHandleList=HandleList}}}
    end;

decode_pdu(?SDP_ServiceAttributeRequest,Transaction,Parameters,_Acc) ->
    AttributeIDSize = sizeof(Parameters,6),
    <<Handle:32/unsigned-integer,
     MaximumAttributeByteCount:16/unsigned-integer,     
     AttributeIDList:AttributeIDSize/binary,
     CSize:8, Continuation:CSize/binary>> = Parameters,
    {ok,{Transaction,Continuation,[],
	 #sdpServiceAttributeRequest 
	 { serviceRecordHandle=Handle,
	   maximumAttributeByteCount=MaximumAttributeByteCount,
	   attributeIDList = decode(AttributeIDList) }}};

decode_pdu(?SDP_ServiceAttributeResponse,Transaction,
	   <<AttributeListByteCount:16/unsigned-integer,
	    AttributeList:AttributeListByteCount/binary,
	    CSize:8, Continuation:CSize/binary>>, Acc) ->
    if Continuation == <<>> ->
	    TotalAttributeList = list_to_binary([Acc,AttributeList]),
	    AList = decode_value(TotalAttributeList),
	    IAttributeList=attribute_id_list(AList),
	    {ok,{Transaction,Continuation,[],
		 #sdpServiceAttributeResponse 
		 { attributeListByteCount=size(TotalAttributeList),
		   attributeList=IAttributeList}}};
       true ->
	    {ok,{Transaction,Continuation,Acc++[AttributeList],
		 #sdpServiceAttributeResponse 
		 { attributeListByteCount=AttributeListByteCount,
		   attributeList=AttributeList}}}
    end;
decode_pdu(?SDP_ServiceSearchAttributeRequest,Transaction, Parameters, _Acc) ->
    PatternSize = sizeof(Parameters,0),
    AttributeIDSize = sizeof(Parameters,PatternSize+2),
    <<Pattern:PatternSize/binary,
     MaximumAttributeByteCount:16/unsigned-integer,
     AttributeIDList:AttributeIDSize/binary,
     CSize:8, Continuation:CSize/binary>> = Parameters,

    {ok,{Transaction,Continuation,[],
	 #sdpServiceSearchAttributeRequest 
	 { serviceSearchPattern=decode(Pattern),
	   maximumAttributeByteCount=MaximumAttributeByteCount,
	   attributeIDList = decode(AttributeIDList) }}};

decode_pdu(?SDP_ServiceSearchAttributeResponse,Transaction,
	   <<AttributeListByteCount:16/unsigned-integer,
	    AttributeList:AttributeListByteCount/binary,
	    CSize:8, Continuation:CSize/binary>> , Acc) ->
    if Continuation == <<>> ->
	    TotalAttributeList = list_to_binary([Acc,AttributeList]),
	    AList = decode_value(TotalAttributeList),
	    RAttributeList = service_id_list(AList),
	    {ok,{Transaction,Continuation,[],
		 #sdpServiceSearchAttributeResponse 
		 { attributeListByteCount=size(TotalAttributeList),
		   attributeList=RAttributeList}}};
       true ->
	    {ok,{Transaction,Continuation,Acc++[AttributeList],
		 #sdpServiceSearchAttributeResponse 
		 { attributeListByteCount=AttributeListByteCount,
		   attributeList=AttributeList}}}
    end;
decode_pdu(?SDP_ErrorResponse,Transaction,
	   <<ErrorCode:16/unsigned-integer,
	    ErrorInfo/binary>>, _Acc) ->
    ECode = 
	case ErrorCode of
	    ?SDP_ErrorInvalidVersion -> invalid_version;
	    ?SDP_ErrorInvalidHandle  -> invalid_handle;
	    ?SDP_ErrorInvalidSyntax  -> invalid_syntax;
	    ?SDP_ErrorInvaludPduSize -> invalid_pdu_size;
	    ?SDP_ErrorInvalidContinuation -> invalid_continuation;
	    ?SDP_ErrorInsufficientResources -> insufficient_resources;
	    _ -> ErrorCode
	end,
    {ok,{Transaction,<<>>,[],
	 #sdpErrorResponse { errorCode=ECode,
			     errorInfo=ErrorInfo }}};
decode_pdu(_ID, _Transaction, _Parameters, _Acc) ->
    {error, invalid_pdu}.




uint32_list(<<X:32/unsigned-integer, T/binary>>) ->
    [X | uint32_list(T)];
uint32_list(<<>>) ->
    [].

%% convert search attribute response into attribute list
service_id_list({sequence,List}) ->
    map(fun(H) -> attribute_id_list(H) end, List);
service_id_list(_) ->
    [].
		
    
%% convert attribute response into attribute list
attribute_id_list({sequence,List}) ->
    attribute_id_list1(List);
attribute_id_list(_) -> [].

attribute_id_list1([{uint16,ID},Elem|T]) ->
    [{ID,Elem} | attribute_id_list1(T)];
attribute_id_list1([{uint16,ID}]) ->
    [{ID,nil}];
attribute_id_list1([]) ->
    [];
attribute_id_list1(_) ->
    [].

    

cvt_uuid_list(UUIDList) ->
    {sequence,
     map(
       fun(UUID) when is_binary(UUID) -> 
	       {uuid,UUID};
	  (UUID) when is_list(UUID); is_atom(UUID) ->
	       {uuid,string_to_uuid(UUID)}
       end,
       UUIDList)}.

cvt_attribute_id_list(AttributeList) ->
	{sequence,
	 map(
	   fun({A1,A2}) ->
		   A11 = if is_list(A1); is_atom(A1) ->
				 string_to_attribute(A1);
			    is_integer(A1) ->
				 A1 band 16#ffff
			 end,
		   A22 = if is_list(A2); is_atom(A2) ->
				 string_to_attribute(A2);
			    is_integer(A2) ->
				 A2 band 16#ffff
			 end,
		   {uint32, (A11 bsl 16) + A22};
	      (A) when is_list(A); is_atom(A) ->
		   {uint16, string_to_attribute(A)};
	      (A) when is_integer(A) ->
		   {uint16, A band 16#ffff}
	   end, AttributeList)}.
    

next_transaction_id() ->    
    N = case get('$sdp_transaction_id') of
	    undefined -> 1;
	    T -> T
	end,
    put('$sdp_transaction_id', N+1),
    N.

	

encode(Attribute, Value) ->
    <<?SDP_UNSIGNED:5, 1:3, Attribute:16, (encode_value(Value))/binary>>.

decode(Bin = <<?SDP_UNSIGNED:5,1:3,Attribute:16,_/binary>>) ->
    {Attribute, decode_value(Bin, 3)}.


%%
%% return size of tag & value (including length bytes if present)
%%
sizeof(Bin, Offs) ->
    case Bin of
	<<_:Offs/binary, ?SDP_NIL:5, 0:3, _/binary>> -> 1;
	<<_:Offs/binary, _:5, 0:3, _/binary>> -> 2;
	<<_:Offs/binary, _:5, 1:3, _/binary>> -> 3;
	<<_:Offs/binary, _:5, 2:3, _/binary>> -> 5;
	<<_:Offs/binary, _:5, 3:3, _/binary>> -> 9;
	<<_:Offs/binary, _:5, 4:3, _/binary>> -> 17;
	<<_:Offs/binary, _:5, 5:3, Len:8, _/binary>> -> Len+2;
	<<_:Offs/binary, _:5, 6:3, Len:16, _/binary>> -> Len+3;
	<<_:Offs/binary, _:5, 7:3, Len:32, _/binary>> -> Len+5
    end.

decode_value(Bin) ->
    decode_value(Bin, 0).

decode_value(Bin, Offs) ->
    case Bin of
	<<_:Offs/binary,?SDP_NIL:5, 0:3,_/binary>> -> nil;
	%% type=1 unsigned integer
	<<_:Offs/binary,?SDP_UNSIGNED:5, 0:3, X:8/unsigned,_/binary>>   -> 
	    {uint8,X};
	<<_:Offs/binary,?SDP_UNSIGNED:5, 1:3, X:16/unsigned,_/binary>>  -> 
	    {uint16,X};
	<<_:Offs/binary,?SDP_UNSIGNED:5, 2:3, X:32/unsigned,_/binary>>  -> 
	    {uint32,X};
	<<_:Offs/binary,?SDP_UNSIGNED:5, 3:3, X:64/unsigned,_/binary>>  -> 
	    {uint64,X};
	<<_:Offs/binary,?SDP_UNSIGNED:5, 4:3, X:128/unsigned,_/binary>> -> 
	    {uint128,X};
	%% type=2 signed integer
	<<_:Offs/binary,?SDP_SIGNED:5, 0:3, X:8/signed,_/binary>>   ->
	    {int8,X};
	<<_:Offs/binary,?SDP_SIGNED:5, 1:3, X:16/signed,_/binary>>  -> 
	    {int16,X};
	<<_:Offs/binary,?SDP_SIGNED:5, 2:3, X:32/signed,_/binary>>  -> 
	    {int32,X};
	<<_:Offs/binary,?SDP_SIGNED:5, 3:3, X:64/signed,_/binary>>  -> 
	    {int64,X};
	<<_:Offs/binary,?SDP_SIGNED:5, 4:3, X:128/signed,_/binary>> -> 
	    {int128,X};
	%% type=3 UUID
	<<_:Offs/binary,?SDP_UUID:5, 1:3, X:2/binary,_/binary>>   -> 
	    {uuid,X};
	<<_:Offs/binary,?SDP_UUID:5, 2:3, X:4/binary,_/binary>>   -> 
	    {uuid,X};
	<<_:Offs/binary,?SDP_UUID:5, 4:3, X:16/binary,_/binary>>  -> 
	    {uuid,X};
	%% type=4 Text string
	<<_:Offs/binary,?SDP_TEXT:5, 5:3, Len:8, X:Len/binary,_/binary>> ->
	    {text,binary_to_list(X)};
	<<_:Offs/binary,?SDP_TEXT:5, 6:3, Len:16, X:Len/binary,_/binary>> ->
	    {text,binary_to_list(X)};
	<<_:Offs/binary,?SDP_TEXT:5, 7:3, Len:32, X:Len/binary,_/binary>> ->
	    {text,binary_to_list(X)};
	%% type=5 Boolean
	<<_:Offs/binary,?SDP_BOOLEAN:5, 0:3, 0,_/binary>> -> 
	    {boolean,false};
	<<_:Offs/binary,?SDP_BOOLEAN:5, 0:3, _,_/binary>> -> 
	    {boolean,true};
	%% type=6 Sequence
	<<_:Offs/binary,?SDP_SEQUENCE:5, 5:3, Len:8, _/binary>> ->
	    {sequence, decode_sequence(Bin,Offs+2, Len, [])};
	<<_:Offs/binary,?SDP_SEQUENCE:5, 6:3, Len:16, _/binary>> ->
	    {sequence, decode_sequence(Bin,Offs+3, Len, [])};
	<<_:Offs/binary,?SDP_SEQUENCE:5, 7:3, Len:32, _/binary>> ->
	    {sequence, decode_sequence(Bin,Offs+5, Len, [])};
	%% type=7 Alternative
	<<_:Offs/binary,?SDP_ALTERNATIVE:5, 5:3, Len:8, _/binary>> ->
	    {alternative, decode_sequence(Bin,Offs+2, Len, [])};
	<<_:Offs/binary,?SDP_ALTERNATIVE:5, 6:3, Len:16, _/binary>> ->
	    {alternative, decode_sequence(Bin,Offs+3, Len, [])};
	<<_:Offs/binary,?SDP_ALTERNATIVE:5, 7:3, Len:32, _/binary>> ->
	    {alternative, decode_sequence(Bin,Offs+5, Len, [])};
	%% type=8 URL
	<<_:Offs/binary,?SDP_URL:5, 5:3, Len:8, X:Len/binary,_/binary>> ->
	    {url,binary_to_list(X)};
	<<_:Offs/binary,?SDP_URL:5, 6:3, Len:16, X:Len/binary,_/binary>> ->
	    {url,binary_to_list(X)};
	<<_:Offs/binary,?SDP_URL:5, 7:3, Len:32, X:Len/binary,_/binary>> ->
	    {url,binary_to_list(X)};
	%% 9-31 reserved
	<<_:Offs/binary,Code:5, _:3, _/binary>> when Code >= 9 ->
	    Sz = sizeof(Bin, Offs),
	    <<_:Offs/binary,_,Reserved:Sz/binary,_/binary>> = Bin,
	    %% The reserved includes the tag byte
	    {reserved, Code, Reserved}
    end.

decode_sequence(Bin,Offset,Len,Acc) when Len > 0 ->
    Elem = decode_value(Bin, Offset),
    Sz = sizeof(Bin, Offset),
    decode_sequence(Bin,Offset+Sz,Len-Sz,[Elem|Acc]);
decode_sequence(_Bin,_Offset,0,Acc) ->
    reverse(Acc).

encode_value(nil) ->   <<?SDP_NIL:5, 0:3>>;

encode_value({uint8,X}) ->   <<?SDP_UNSIGNED:5, 0:3, X:8>>;
encode_value({uint16,X}) ->  <<?SDP_UNSIGNED:5, 1:3, X:16>>;
encode_value({uint32,X}) ->  <<?SDP_UNSIGNED:5, 2:3, X:32>>;
encode_value({uint64,X}) ->  <<?SDP_UNSIGNED:5, 3:3, X:64>>;
encode_value({uint128,X}) -> <<?SDP_UNSIGNED:5, 4:3, X:128>>;
encode_value({uint,X}) when X>=0 ->
    if X<16#100 -> <<?SDP_UNSIGNED:5, 0:3, X:8>>;
       X<16#10000 -> <<?SDP_UNSIGNED:5, 1:3, X:16>>;
       X<16#100000000 -> <<?SDP_UNSIGNED:5, 2:3, X:32>>;
       X<16#10000000000000000 -> <<?SDP_UNSIGNED:5, 3:3, X:64>>;
       true -> (<<?SDP_UNSIGNED:5, 4:3, X:128>>)
    end;
encode_value({int8,X}) ->   <<?SDP_SIGNED:5, 0:3, X:8>>;
encode_value({int16,X}) ->  <<?SDP_SIGNED:5, 1:3, X:16>>;
encode_value({int32,X}) ->  <<?SDP_SIGNED:5, 2:3, X:32>>;
encode_value({int64,X}) ->  <<?SDP_SIGNED:5, 3:3, X:64>>;
encode_value({int128,X}) -> <<?SDP_SIGNED:5, 4:3, X:128>>;
encode_value({int,X}) ->
    if X >= -16#100, X<16#100 -> 
	    <<?SDP_SIGNED:5, 0:3, X:8>>;
       X >= -16#10000, X<16#10000 -> 
	    <<?SDP_SIGNED:5, 1:3, X:16>>;
       X >= -16#100000000, X<16#100000000 -> 
	    <<?SDP_SIGNED:5, 2:3, X:32>>;
       X >= -16#10000000000000000, X<16#10000000000000000 ->
	    <<?SDP_SIGNED:5,3:3,X:64>>;
       true -> (<<?SDP_SIGNED:5, 4:3, X:128>>)
    end;
encode_value({uuid,UUID}) when is_binary(UUID) ->
    case size(UUID) of
	2  -> <<?SDP_UUID:5, 1:3, UUID/binary>>;
	4  -> <<?SDP_UUID:5, 2:3, UUID/binary>>;
	16 -> (<<?SDP_UUID:5, 4:3, UUID/binary>>)
    end;
encode_value({text,Text}) when is_list(Text) ->
    Bin = list_to_binary(Text),
    Len = size(Bin),
    if Len =< 16#100 ->
	    <<?SDP_TEXT:5, 5:3, Len:8, Bin/binary>>;
       Len =< 16#10000 ->
	    <<?SDP_TEXT:5, 6:3, Len:16, Bin/binary>>;
       true ->
	    (<<?SDP_TEXT:5, 6:3, Len:32, Bin/binary>>)
    end;
encode_value({boolean,Bool}) ->
    case Bool of
	true  -> <<?SDP_BOOLEAN:5, 0:3, 1>>;
	false -> (<<?SDP_BOOLEAN:5, 0:3, 0>>)
    end;
encode_value({url,URL}) when is_list(URL) ->
    Bin = list_to_binary(URL),
    Len = size(Bin),
    if Len =< 16#100 ->
	    <<?SDP_URL:5, 5:3, Len:8, Bin/binary>>;
       Len =< 16#10000 ->
	    <<?SDP_URL:5, 6:3, Len:16, Bin/binary>>;
       true ->
	    (<<?SDP_URL:5, 6:3, Len:32, Bin/binary>>)
    end;
encode_value({sequence, List}) ->
    Bin = list_to_binary(map(fun(E) -> encode_value(E) end, List)),
    Len = size(Bin),
    if Len =< 16#100 ->
	    <<?SDP_SEQUENCE:5, 5:3, Len:8, Bin/binary>>;
       Len =< 16#10000 ->
	    <<?SDP_SEQUENCE:5, 6:3, Len:16, Bin/binary>>;
       true ->
	    (<<?SDP_SEQUENCE:5, 6:3, Len:32, Bin/binary>>)
    end;    
encode_value({alternative, List}) ->
    Bin = list_to_binary(map(fun(E) -> encode_value(E) end, List)),
    Len = size(Bin),
    if Len =< 16#100 ->
	    <<?SDP_ALTERNATIVE:5, 5:3, Len:8, Bin/binary>>;
       Len =< 16#10000 ->
	    <<?SDP_ALTERNATIVE:5, 6:3, Len:16, Bin/binary>>;
       true ->
	    (<<?SDP_ALTERNATIVE:5, 6:3, Len:32, Bin/binary>>)
    end;
encode_value({reserved,_Code,Bin}) when is_binary(Bin) ->
    Bin.

fold_value(Fun, Acc, Value={alternative,List}) ->
    fold_value(Fun, Fun(Value,Acc), List);
fold_value(Fun, Acc, Value={sequence,List}) ->
    fold_value(Fun, Fun(Value,Acc), List);
fold_value(Fun, Acc,[H|T]) ->
    fold_value(Fun, fold_value(H, Fun, Acc), T);
fold_value(_Fun, Acc, []) ->
    Acc;
fold_value(Fun, Acc, Value) ->
    Fun(Value, Acc).



%% make a "decode" of binary sdp value
decode_sdp_value({uuid,Bin}) when is_binary(Bin) ->  uuid_to_string(Bin);
decode_sdp_value({uint8,Int}) when is_integer(Int) ->   Int;
decode_sdp_value({uint16,Int}) when is_integer(Int) ->  Int;
decode_sdp_value({uint32,Int}) when is_integer(Int) ->  Int;
decode_sdp_value({uint64,Int}) when is_integer(Int) ->  Int;
decode_sdp_value({uint128,Int}) when is_integer(Int) -> Int;
decode_sdp_value({int8,Int}) when is_integer(Int) ->    Int;
decode_sdp_value({int16,Int}) when is_integer(Int) ->   Int;
decode_sdp_value({int32,Int}) when is_integer(Int) ->   Int;
decode_sdp_value({int64,Int}) when is_integer(Int) ->   Int;
decode_sdp_value({int128,Int}) when is_integer(Int) ->  Int;
decode_sdp_value({boolean,Bool}) -> Bool;
decode_sdp_value({text,Text}) -> Text;
decode_sdp_value({url,Url}) -> Url;
decode_sdp_value({sequence,Seq}) ->
    {sequence,map(fun decode_sdp_value/1, Seq)};
decode_sdp_value({alternative,Alt}) ->
    {alternative,map(fun decode_sdp_value/1, Alt)}.

is_value(nil) -> true;
is_value({uint8,X}) when is_integer(X), X>=0 -> true;
is_value({uint16,X}) when is_integer(X), X>=0 -> true;
is_value({uint32,X}) when is_integer(X), X>=0 -> true;
is_value({uint64,X}) when is_integer(X), X>=0 -> true;
is_value({uint128,X}) when is_integer(X), X>=0 -> true;
is_value({uint,X}) when is_integer(X), X>=0 -> true;
is_value({int8,X}) when is_integer(X) -> true;
is_value({int16,X}) when is_integer(X) -> true;
is_value({int32,X}) when is_integer(X) -> true;
is_value({int64,X}) when is_integer(X) -> true;
is_value({int128,X}) when is_integer(X) -> true;
is_value({int,X}) when is_integer(X) -> true;
is_value({uuid,UUID}) when is_binary(UUID) ->
    case size(UUID) of
	2  -> true;
	4  -> true;
	16 -> true;
	_ -> false
    end;
is_value({text,Text}) when is_list(Text) -> 
    lists:all(fun(X) -> is_integer(X) end, Text);
is_value({boolean,true}) -> true;
is_value({boolean,false}) -> true;
is_value({url,URL}) when is_list(URL) ->
    case catch list_to_binary(URL) of
	{'EXIT', _} -> false;
	_ -> true
    end;
is_value({sequence, List}) ->
    lists:all(fun is_value/1, List);
is_value({alternative, List}) ->
    lists:all(fun is_value/1, List);
is_value({reserved,_Code,Bin}) when is_binary(Bin) ->
    Bin;
is_value(_) -> false.




value_to_string({uuid,Bin}) -> uuid_to_string(Bin);
value_to_string({uint8,Int}) -> integer_to_list(Int);
value_to_string({uint16,Int}) -> integer_to_list(Int);
value_to_string({uint32,Int}) -> integer_to_list(Int);
value_to_string({uint64,Int}) -> integer_to_list(Int);
value_to_string({uint128,Int}) -> integer_to_list(Int);
value_to_string({int8,Int}) -> integer_to_list(Int);
value_to_string({int16,Int}) -> integer_to_list(Int);
value_to_string({int32,Int}) -> integer_to_list(Int);
value_to_string({int64,Int}) -> integer_to_list(Int);
value_to_string({int128,Int}) -> integer_to_list(Int);
value_to_string({boolean,Bool}) -> atom_to_list(Bool);
value_to_string({sequence,Seq}) ->
    map_fmt(fun value_to_string/1, ",", Seq);
value_to_string({alternative,Alt}) ->
    map_fmt(fun value_to_string/1, "|", Alt);
value_to_string({url,URL}) -> URL;
value_to_string({text,Text}) -> [$\",Text, $\"].

map_fmt(Fun, _Sep, [H]) ->  Fun(H);
map_fmt(Fun, Sep, [H|T]) -> [Fun(H),Sep | map_fmt(Fun, Sep, T)];
map_fmt(_Fun, _Sep, []) ->  [].

%% convert to full uuid
uuid_128(<<UUID:16>>) -> ?BT_UUID16(UUID);
uuid_128(<<UUID:32>>) -> ?BT_UUID32(UUID);
uuid_128(<<>>) -> <<>>;
uuid_128(UUID) when ?is_uuid(UUID) ->  UUID.

%% Try convert UUID into symboic name
uuid_to_string(UUID) ->
    case uuid_128(UUID) of
	UUID128 = ?BT_UUID16(UUID16) ->
	    case <<UUID16:16>> of
		%% PROTOCOLS
		?UUID_SDP -> "SDP";
		?UUID_UDP -> "UDP";
		?UUID_RFCOMM -> "RFCOMM";
		?UUID_TCP -> "TCP";
		?UUID_TCS_BIN -> "TCS_BIN";
		?UUID_TCS_AT -> "TCS_AT";
		?UUID_OBEX -> "OBEX";
		?UUID_IP -> "IP";
		?UUID_FTP -> "FTP";
		?UUID_HTTP -> "HTTP";
		?UUID_WSP -> "WSP";
		?UUID_BNEP -> "BNEP";
		?UUID_UPNP -> "UPNP";
		?UUID_HIDP -> "HIDP";
		?UUID_HCRP_CTRL -> "HCRP_CTRL";
		?UUID_HCRP_DATA -> "HCRP_DATA";
		?UUID_HCRP_NOTE -> "HRCP_NOTE";
		?UUID_AVCTP -> "AVCTP";
		?UUID_AVDTP -> "AVDTP";
		?UUID_CMPT -> "CMPT";
		?UUID_UDI -> "UDI";
		?UUID_MCAP_CTRL -> "MCAP_CTRL";
		?UUID_MCAP_DATA -> "MCAP_DATA";
		?UUID_L2CAP -> "L2CAP";

		%% SERVICE Classes
		?UUID_ServiceDiscoveryServer -> "ServiceDiscoveryServer";
		?UUID_BrowseGroupDescriptor -> "BrowseGroupDescriptor";
		?UUID_PublicBrowseGroup -> "PublicBrowseGroup";
		?UUID_SerialPort -> "SerialPort";
		?UUID_LANAccessUsingPPP -> "LANAccessUsingPPP";
		?UUID_DialupNetworking -> "DialupNetworking";
		?UUID_IrMCSync -> "IrMCSync";
		?UUID_OBEXObjectPush -> "OBEXObjectPush";
		?UUID_OBEXFileTransfer -> "OBEXFileTransfer";
		?UUID_IrMCSyncCommand -> "IrMCSyncCommand";
		?UUID_Headset -> "Headset";
		?UUID_CordlessTelephony -> "CordlessTelephony";
		?UUID_AudioSource -> "AudioSource";
		?UUID_AudioSink -> "AudioSink";
		?UUID_AVRemoteControlTarget -> "AVRemoteControlTarget";
		?UUID_AdvancedAudioDistribution -> "AdvancedAudioDistribution";
		?UUID_AVRemoteControl -> "AVRemoteControl";
		?UUID_VideoConferencing -> "VideoConferencing";
		?UUID_Intercom -> "Intercom";
		?UUID_Fax -> "Fax";
		?UUID_HeadsetAudioGateway -> "HeadsetAudioGateway";
		?UUID_WAP -> "WAP";
		?UUID_WAPClient -> "WAPClient";
		?UUID_PANU -> "PANU";
		?UUID_NAP -> "NAP";
		?UUID_GN -> "GN";
		?UUID_DirectPrinting -> "DirectPrinting";
		?UUID_ReferencePrinting -> "ReferencePrinting";
		?UUID_Imaging -> "Imaging";
		?UUID_ImagingResponder -> "ImagingResponder";
		?UUID_ImagingAutomaticArchive -> "ImagingAutomaticArchive";
		?UUID_ImagingReferencedObjects -> "ImagingReferencedObjects";
		?UUID_Handsfree -> "Handsfree";
		?UUID_HandsfreeAudioGateway -> "HandsfreeAudioGateway";
		?UUID_DirectPrintingReferenceObjectsService -> "DirectPrintingReferenceObjectsService";
		?UUID_ReflectedUI -> "ReflectedUI";
		?UUID_BasicPrinting -> "BasicPrinting";
		?UUID_PrintingStatus -> "PrintingStatus";
		?UUID_HumanInterfaceDeviceService -> "HumanInterfaceDeviceService";
		?UUID_HardcopyCableReplacement -> "HardcopyCableReplacement";
		?UUID_HCR_Print -> "HCR_Print";
		?UUID_HCR_Scan -> "HCR_Scan";
		?UUID_CommonISDNAccess -> "CommonISDNAccess";
		?UUID_VideoConferencingGW -> "VideoConferencingGW";
		?UUID_UDI_MT -> "UDI_MT";
		?UUID_UDI_TA -> "UDI_TA";
		?UUID_Audio_Video -> "Audio/Video";
		?UUID_SIM_Access -> "SIM_Access";
		?UUID_PhonebookAccess_PCE -> "PhonebookAccess-PCE";
		?UUID_PhonebookAccess_PSE -> "PhonebookAccess-PSE";

		?UUID_PhonebookAccess -> "PhonebookAccessProfile";
		?UUID_Headset_HS -> "Headset Profile";
		?UUID_Message_Access_Server -> "MessageAccessProfile";
		?UUID_Message_Notification_Server -> "MessageNotificationServer";
		?UUID_Message_Access_Profile -> "MessageAccessProfile";
		?UUID_GNSS -> "GNSSProfile";
		?UUID_GNSS_Server -> "GNSSSever";
		?UUID_3D_Display -> "3D-Display";
		?UUID_3D_Glasses -> "3D-Glasses";
		?UUID_3D_Synchronization -> "3D-Synchronization";
		?UUID_MPS_Profile -> "MPS";
		?UUID_MPS_SC -> "MPS-SC";
		?UUID_CTN_Access_Service -> "CTNAccessService";
		?UUID_CTN_Notification_Service ->"CTNNotificationService";
		?UUID_CTN_Profile -> "CTNProfile";
		?UUID_PnPInformation -> "PnPInformation";
		?UUID_GenericNetworking -> "GenericNetworking";
		?UUID_GenericFileTransfer -> "GenericFileTransfer";
		?UUID_GenericAudio -> "GenericAudio";
		?UUID_GenericTelephony -> "GenericTelephony";
		?UUID_UPNP_Service -> "UPNP_Service";
		?UUID_UPNP_IP_Service -> "UPNP_IP_Service";
		?UUID_ESDP_UPNP_IP_PAN -> "ESDP_UPNP_IP_PAN";
		?UUID_ESDP_UPNP_IP_LAP -> "ESDP_UPNP_IP_LAP";
		?UUID_ESDP_UPNP_L2CAP -> "ESDP_UPNP_L2CAP";
		?UUID_VideoSource -> "VideoSource";
		?UUID_VideoSink -> "VideoSink";
		?UUID_VideoDistribution -> "VideoDistribution";
		?UUID_HDP -> "HDP";
		?UUID_HDP_Source -> "HDP-Source";
		?UUID_HDP_Sink ->  "HDP-Sink";
		_ -> bt_util:uuid_to_string(UUID128)
	    end;
	?UUID_SyncMLServer   -> "SyncMLServer";
	?UUID_SyncMLClient   -> "SyncMLClient";
	?UUID_SyncMLDMServer -> "SyncMLDMServer";
	?UUID_SyncMLDMClient -> "SyncMLDMClient";
	?UUID_NokiaSyncMLServer -> "NokiaSyncMLServer";
	?UUID_NokiaObexPcSuiteServices -> "NokiaObexPcSuiteService";
	UUID128 -> bt_util:uuid_to_string(UUID128)
    end.

	     

%% Try convert symboic name into UUID
string_to_uuid(Name) when is_list(Name) ->
    case tolower(Name) of
	%% PROTOCOLS
	"sdp" -> ?UUID_SDP;
	"udp" -> ?UUID_UDP;
	"rfcomm" -> ?UUID_RFCOMM;
	"tcp" -> ?UUID_TCP;
	"tcs_bin" -> ?UUID_TCS_BIN;
	"tcs_at" -> ?UUID_TCS_AT;
	"obex" -> ?UUID_OBEX;
	"ip" -> ?UUID_IP;
	"ftp" -> ?UUID_FTP;
	"http" -> ?UUID_HTTP;
	"wsp" -> ?UUID_WSP;
	"bnep" -> ?UUID_BNEP;
	"upnp" -> ?UUID_UPNP;
	"hidp" -> ?UUID_HIDP;
	"hardcopycontrolchannel" -> ?UUID_HCRP_CTRL;
	"hardcopydatachannel"    -> ?UUID_HCRP_DATA;
	"hardcopynotification"   -> ?UUID_HCRP_NOTE;
	"avctp" -> ?UUID_AVCTP;
	"avdtp" -> ?UUID_AVDTP;
	"cmpt" -> ?UUID_CMPT;
	"udi" -> ?UUID_UDI;
	"l2cap" -> ?UUID_L2CAP;
		%% SERVICE Classes
	"servicediscoveryserver" -> ?UUID_ServiceDiscoveryServer;
	"browsegroupdescriptor" -> ?UUID_BrowseGroupDescriptor;
	"publicbrowsegroup" -> ?UUID_PublicBrowseGroup;
	"serialport" -> ?UUID_SerialPort;
	"lanaccessusingppp" -> ?UUID_LANAccessUsingPPP;
	"dialupnetworking" -> ?UUID_DialupNetworking;
	"irmcsync" -> ?UUID_IrMCSync;
	"obexobjectpush" -> ?UUID_OBEXObjectPush;
	"obexfiletransfer" -> ?UUID_OBEXFileTransfer;
	"irmcsynccommand" -> ?UUID_IrMCSyncCommand;
	"headset" -> ?UUID_Headset;
	"cordlesstelephony" -> ?UUID_CordlessTelephony;
	"audiosource" -> ?UUID_AudioSource;
	"audiosink" -> ?UUID_AudioSink;
	"avremotecontroltarget" -> ?UUID_AVRemoteControlTarget;
	"advancedaudiodistribution" -> ?UUID_AdvancedAudioDistribution;
	"avremotecontrol" -> ?UUID_AVRemoteControl;
	"videoconferencing" -> ?UUID_VideoConferencing;
	"intercom" -> ?UUID_Intercom;
	"fax" -> ?UUID_Fax;
	"headsetaudiogateway" -> ?UUID_HeadsetAudioGateway;
	"wap" -> ?UUID_WAP;
	"wapclient" -> ?UUID_WAPClient;
	"panu" -> ?UUID_PANU;
	"nap" -> ?UUID_NAP;
	"gn" -> ?UUID_GN;
	"directprinting" -> ?UUID_DirectPrinting;
	"referenceprinting" -> ?UUID_ReferencePrinting;
	"imaging" -> ?UUID_Imaging;
	"imagingresponder" -> ?UUID_ImagingResponder;
	"imagingautomaticarchive" -> ?UUID_ImagingAutomaticArchive;
	"imagingreferencedobjects" -> ?UUID_ImagingReferencedObjects;
	"handsfree" -> ?UUID_Handsfree;
	"handsfreeaudiogateway" -> ?UUID_HandsfreeAudioGateway;
	"directprintingreferenceobjectsservice" -> ?UUID_DirectPrintingReferenceObjectsService;
	"reflectedui" -> ?UUID_ReflectedUI;
	"basicprinting" -> ?UUID_BasicPrinting;
	"printingstatus" -> ?UUID_PrintingStatus;
	"humaninterfacedeviceservice" -> ?UUID_HumanInterfaceDeviceService;
	"hardcopycablereplacement" -> ?UUID_HardcopyCableReplacement;
	"hcr_print" -> ?UUID_HCR_Print;
	"hcr_scan" -> ?UUID_HCR_Scan;
	"commonisdnaccess" -> ?UUID_CommonISDNAccess;
	"videoconferencinggw" -> ?UUID_VideoConferencingGW;
	"udi_mt" -> ?UUID_UDI_MT;
	"udi_ta" -> ?UUID_UDI_TA;
	"audio/video" -> ?UUID_Audio_Video;
	"sim_access" -> ?UUID_SIM_Access;
	"phonebookaccess-pce" -> ?UUID_PhonebookAccess_PCE;
	"phonebookaccess-pse" -> ?UUID_PhonebookAccess_PSE;
	"pnpinformation" -> ?UUID_PnPInformation;
	"genericnetworking" -> ?UUID_GenericNetworking;
	"genericfiletransfer" -> ?UUID_GenericFileTransfer;
	"genericaudio" -> ?UUID_GenericAudio;
	"generictelephony" -> ?UUID_GenericTelephony;
	"upnp_service" -> ?UUID_UPNP_Service;
	"upnp_ip_service" -> ?UUID_UPNP_IP_Service;
	"esdp_upnp_ip_pan" -> ?UUID_ESDP_UPNP_IP_PAN;
	"esdp_upnp_ip_lap" -> ?UUID_ESDP_UPNP_IP_LAP;
	"esdp_upnp_l2cap" -> ?UUID_ESDP_UPNP_L2CAP;
	"videosource" -> ?UUID_VideoSource;
	"videosink" -> ?UUID_VideoSink;
	"videodistribution" -> ?UUID_VideoDistribution;

	%% SyncML
	"syncmlserver" -> ?UUID_SyncMLServer;
	"syncmlclient" -> ?UUID_SyncMLClient;
	"syncmldmserver" -> ?UUID_SyncMLDMServer;
	"syncmldmclient" -> ?UUID_SyncMLDMClient;
	_ -> bt_util:string_to_uuid(Name)
    end;
string_to_uuid(Name) when is_atom(Name) ->
    string_to_uuid(atom_to_list(Name)).


string_to_attribute(Name) ->
    string_to_attribute(Name, 16#0100).

string_to_attribute(Name,LanguageBase) when is_list(Name) ->
    case tolower(Name) of
	"servicerecordhandle"    -> ?ATTR_ServiceRecordHandle;
	"serviceclassidlist"     -> ?ATTR_ServiceClassIDList;
	"servicerecordstate"     -> ?ATTR_ServiceRecordState;
	"serviceid"              -> ?ATTR_ServiceID;
	"protocoldescriptorlist" -> ?ATTR_ProtocolDescriptorList;
	"browsegrouplist"        -> ?ATTR_BrowseGroupList;
	"languagebaseattributeidlist" -> 
	    ?ATTR_LanguageBaseAttributeIDList;
	"serviceinfotimetolive"  -> ?ATTR_ServiceInfoTimeToLive;
	"serviceavailability"    -> ?ATTR_ServiceAvailability;
	"bluetoothprofiledescriptorlist" -> 
	    ?ATTR_BluetoothProfileDescriptorList;
	"documentationurl"       -> ?ATTR_DocumentationURL;
	"clientexecutableurl"    -> ?ATTR_ClientExecutableURL;
	"iconurl"                -> ?ATTR_IconURL;
	"additionalprotocolsdescriptorlist" -> 
	    ?ATTR_AdditionalProtocolsDescriptorList;
	%% 
	"servicename"        -> ?ATTR_ServiceName+LanguageBase;
	"servicedescription" -> ?ATTR_ServiceDescription+LanguageBase;
	"providername"       -> ?ATTR_ProviderName+LanguageBase;
	
	%% SDP
	"servicedatabasestate" -> 16#0201;
	%% PAN 
	"ipsubnet" -> 16#0200;
	%% OTHER
	"externalnetwork" -> 16#0301;
	"network"         -> 16#0301;
	"supporteddatastoreslist" -> 16#0301;
	"faxclass1support" -> 16#0302;
	"faxclass2support" -> 16#0303;
	"remoteaudiovolumecontrol" -> 16#0302;
	"supporterformatslist" -> 16#0303;
	%% Check hex
	_ ->
	    case catch erlang:list_to_integer(Name, 16) of
		{'EXIT',_} ->
		    erlang:error(badarg);
		ID -> ID band 16#ffff
	    end
    end;
string_to_attribute(Name,LanguageBase) when is_atom(Name) ->
    string_to_attribute(atom_to_list(Name),LanguageBase).

	
attribute_to_string(ID) ->
    attribute_to_string(ID,16#0100).

attribute_to_string(ID,LanguageBase) ->
    attribute_to_string(ID,LanguageBase,<<>>).

attribute_to_string(ID,LanguageBase,UUID) ->
    case uuid_128(UUID) of
	?BT_UUID16(UUID16) ->
	    case UUID16-LanguageBase of
		16#0000 -> "ServiceName";
		16#0001 -> "ServiceDescription";
		16#0002 -> "ProviderName";
		_ -> attr16(ID,<<UUID16:16>>)
	    end;
	UUID128 ->
	    attr(ID, LanguageBase,UUID128)
    end.

attr(ID,_LanuageBase,_UUID) ->
    attr16_id(ID).
    
%%
%% Universal Attributes
%%
attr16(?ATTR_ServiceRecordHandle,_UUID) -> "ServiceRecordHandle";
attr16(?ATTR_ServiceClassIDList,_UUID) -> "ServiceClassIDList";
attr16(?ATTR_ServiceRecordState,_UUID) -> "ServiceRecordState";
attr16(?ATTR_ServiceID,_UUID) -> "ServiceID";
attr16(?ATTR_ProtocolDescriptorList,_UUID) -> "ProtocolDescriptorList";
attr16(?ATTR_BrowseGroupList,_UUID) -> "BrowseGroupList";
attr16(?ATTR_LanguageBaseAttributeIDList,_UUID) -> "LanguageBaseAttributeIDList";
attr16(?ATTR_ServiceInfoTimeToLive,_UUID) -> "ServiceInfoTimeToLive";
attr16(?ATTR_ServiceAvailability,_UUID) -> "ServiceAvailability";
attr16(?ATTR_BluetoothProfileDescriptorList,_UUID) -> "BluetoothProfileDescriptorList";
attr16(?ATTR_DocumentationURL,_UUID) -> "DocumentationURL";
attr16(?ATTR_ClientExecutableURL,_UUID) -> "ClientExecutableURL";
attr16(?ATTR_IconURL,_UUID) -> "IconURL";
attr16(?ATTR_AdditionalProtocolsDescriptorList,_UUID) ->"AdditionalProtocolsDescriptorList";

attr16(16#0200, ?UUID_BrowseGroupDescriptor) -> "GroupID";

%% SDP 1000
attr16(ID,?UUID_ServiceDiscoveryServer) -> attr16_sdp(ID);

%% AVRCP
attr16(ID,?UUID_AVRemoteControlTarget) -> attr16_avrcp(ID);
attr16(ID,?UUID_AVRemoteControl) -> attr16_avrcp(ID);
attr16(ID,?UUID_VideoConferencing) -> attr16_avrcp(ID);

%% PAN 
attr16(ID,?UUID_PANU) -> attr16_pan(ID);
attr16(ID,?UUID_NAP) -> attr16_pan(ID);
attr16(ID,?UUID_GN) -> attr16_pan(ID);

%% BIP 0x111B, 0x111C, 0x111D
attr16(ID,?UUID_ImagingResponder) -> attr16_bip(ID);
attr16(ID,?UUID_ImagingAutomaticArchive) -> attr16_bip(ID);
attr16(ID,?UUID_ImagingReferencedObjects) -> attr16_bip(ID);

attr16(ID,?UUID_PnPInformation) -> attr16_pn(ID);

attr16(ID,_UUID) -> attr16_id(ID).
    

attr16_sdp(?ATTR_SDP_VersionNumberList) -> "VersionNumberList";
attr16_sdp(?ATTR_SDP_ServiceDatabaseState) -> "ServiceDatabaseState";
attr16_sdp(ID) -> attr16_id(ID).

attr16_bip(?ATTR_BIP_GoepL2capPsm) -> "GoepL2capPsm";
attr16_bip(?ATTR_BIP_SupportedCapabilities) -> "SupportedCapabilities";
attr16_bip(?ATTR_BIP_SupportedFeatures) -> "SupportedFeatures";
attr16_bip(?ATTR_BIP_SupportedFunctions) -> "SupportedFunctions";
attr16_bip(?ATTR_BIP_TotalImagingDataCapacity) -> "TotalImagingDataCapacity";
attr16_bip(ID) ->  attr16_id(ID).

attr16_pan(16#0200) -> "IpSubnet";
attr16_pan(16#030A) -> "SecurityDescription";
attr16_pan(16#030B) -> "NetAccessType";
attr16_pan(16#030C) -> "MaxNetAccessrate";
attr16_pan(16#030D) -> "IPv4Subnet";
attr16_pan(16#030E) -> "IPv6Subnet";
attr16_pan(ID) -> attr16_id(ID).

attr16_avrcp(16#0311) -> "SupportedFeatures";
attr16_avrcp(ID) -> attr16_id(ID).
    
attr16_pn(16#0200) -> "SpecificationID";
attr16_pn(16#0201) -> "VendorID";
attr16_pn(16#0202) -> "ProductID";
attr16_pn(16#0203) -> "Version";
attr16_pn(16#0204) -> "PrimaryRecord";
attr16_pn(16#0205) -> "VendorIDSource";
attr16_pn(ID) ->  attr16_id(ID).

%% convert to 0xABCD format
attr16_id(ID) ->
    "0x"++tl(integer_to_list((ID band 16#ffff) bor 16#10000, 16)).

tolower([H|T]) when H>=$A, H=<$Z ->
    [(H-$A)+$a | tolower(T)];
tolower([H|T]) ->
    [H|tolower(T)];
tolower([]) ->
    [].
