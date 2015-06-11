%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@up13>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    HCI socket server
%%% @end
%%% Created :  6 Apr 2015 by Tony Rogvall <tony@up13>
%%%-------------------------------------------------------------------
-module(hci_socket).

-behaviour(gen_server).

%% API
-export([open/1]).
-export([close/1]).
-export([send/4]).
-export([call/5]).
-export([get_info/1]).
-export([get_conn_list/1]).
-export([get_conn_info/3]).
-export([get_auth_info/2]).
-export([debug/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("../include/hci_drv.hrl").
-include("hci_api.hrl").

-record(state,
	{
	  hci :: hci_socket_t(),
	  devid :: hci_devid_t(),
	  from,
	  saved_filter :: #hci_filter {},
	  current_opcode :: uint16_t(),
	  current_event :: uint16_t(),
	  current_decode :: function()
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
open(DevID) ->
    gen_server:start_link(?MODULE, [DevID], []).

close(Pid) ->
    gen_server:call(Pid, close).

send(Pid, OGF, OCG, Data) ->
    gen_server:call(Pid, {send, OGF, OCG, Data}).

call(Pid, OGF, OCG, Data, Event) ->
    gen_server:call(Pid, {call, OGF, OCG, Data, Event}).

get_info(Pid) ->
    gen_server:call(Pid, get_info).

get_conn_list(Pid) ->
    gen_server:call(Pid, get_conn_list).

get_conn_info(Pid,Addr,Type) ->
    gen_server:call(Pid, {get_conn_info,Addr,Type}).

get_auth_info(Pid,Addr) ->
    gen_server:call(Pid, {get_auth_info,Addr}).    

debug(Pid, Level) ->
    gen_server:call(Pid, {debug, Level}).
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([DevID]) ->
    Hci = hci_drv:open(),
    case hci_drv:bind(Hci, DevID) of
	ok -> 
	    hci_drv:activate(Hci),  %% receive async data
	    {ok, #state{ hci = Hci, devid = DevID }};
	Error -> {stop,Error}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send,OGF,OCF,Data}, _From, State) ->
    Pkt = [?HCI_COMMAND_PKT,
	   <<(?cmd_opcode_pack(OGF,OCF)):16/little>>,
	   <<(byte_size(Data)):8>>,
	   Data],
    R = hci_drv:send(State#state.hci,Pkt),
    %% io:format("send pkt ~p = ~p\n", [Pkt,R]),
    {reply, R, State};

handle_call({call,OGF,OCF,Data,Decode}, From, State) ->
    Opcode = ?cmd_opcode_pack(OGF,OCF),
    {ok,OldFilter} = hci_drv:get_filter(State#state.hci),
    %% io:format("call: saved_filter = ~p\n", [OldFilter]),
    Event = 0,  %% fixme, use ?EVT_xxxx
    NewFilter = hci_drv:make_filter(Opcode,
				    [?HCI_EVENT_PKT],
				    [?EVT_CMD_STATUS,
				     ?EVT_CMD_COMPLETE,
				     ?EVT_LE_META_EVENT,
				     ?EVT_REMOTE_NAME_REQ_COMPLETE,
				     Event]),
    %% io:format("call: new_filter = ~p\n", [NewFilter]),
    case hci_drv:set_filter(State#state.hci, NewFilter) of
	ok ->
	    Pkt = [?HCI_COMMAND_PKT,
		   <<Opcode:16/little>>,
		   <<(byte_size(Data)):8>>,
		   Data],
	    _R = hci_drv:send(State#state.hci,Pkt),
	    %% io:format("send pkt ~p = ~p\n", [Pkt,_R]),
	    {noreply, State#state { from = From,
				    saved_filter = OldFilter,
				    current_opcode = Opcode,
				    current_event = Event,
				    current_decode = Decode
				  }};
	Error ->
	    {reply, Error, State}
    end;

handle_call(get_info, _From, State) ->
    {reply, hci_drv:get_dev_info(State#state.hci, State#state.devid), State};
handle_call(get_conn_list, _From, State) ->
    {reply, hci_drv:get_conn_list(State#state.hci, State#state.devid), State};
handle_call({get_conn_info,Addr,Type}, _From, State) ->
    {reply, hci_drv:get_conn_info(State#state.hci,Addr,Type), State};
handle_call({get_auth_info,Addr},_From, State) ->
    {reply, hci_drv:get_auth_info(State#state.hci, Addr), State};
handle_call(close, _From, State) ->
    hci_drv:close(State#state.hci),
    {stop, normal, ok, State#state { hci=undefined }};
handle_call({debug,Level}, _From, State) ->
    {reply, hci_drv:debug(State#state.hci, Level), State};
handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Hci,{data,_D0= <<_:8,Evt:8,Plen:8,Packet:Plen/binary,_R/binary>>}},
	    State) when Hci =:= State#state.hci ->
    case Evt of
	?EVT_CMD_COMPLETE -> %% Fixme: match opcode
	    %%io:format("EVT_CMD_COMPLETE, data ~p, rest=~p\n", [_D0,_R]),
	    <<?evt_cmd_complete_bin(_Ncmd,_Opcode),Reply/binary>> = Packet,
	    %% io:format("Ncmd=~w,Opcode=~p,Reply=~p\n",[_Ncmd,_Opcode,Reply]),
	    State1 = do_reply(Reply, State),
	    {noreply, State1 };

	?EVT_CMD_STATUS -> %% Fixme: match opcode
	    %% io:format("EVT_CMD_STATUS, data ~p, rest=~p\n", [_D0,_R]),
	    <<?evt_cmd_status_bin(_Status,_Ncmd,_Opcode),Reply/binary>> = Packet,
	    %% io:format("Status=~w,Ncmd=~w,Opcode=~p,Reply=~p\n",[_Status,_Ncmd,_Opcode,Reply]),
	    State1 = do_reply(Reply, State),
	    {noreply, State1 };

	?EVT_REMOTE_NAME_REQ_COMPLETE ->
	    %% io:format("EVT_REMOTE_NAME_REQ_COMPLETE,data ~p,rest=~p\n", [_D0,_R]),
	    <<?evt_remote_name_req_complete_bin(Status,Bdaddr,Name)>> = Packet,
	    NameComplete = #evt_remote_name_req_complete { status=Status,
							   bdaddr=Bdaddr,
							   name=cname(Name)},
	    gen_server:reply(State#state.from, {ok, NameComplete}),
	    hci_drv:set_filter(State#state.hci, 
			       State#state.saved_filter),
	    {noreply, State#state { from = undefined,
				    saved_filter = undefined }};
	?EVT_LE_META_EVENT ->
	    %% io:format("EVT_LE_META_EVENT, data ~p, rest=~p\n", [_D0,_R]),
	    <<?evt_le_meta_event_bin(Subevent,_D1),LePacket/binary>> = Packet,
	    Le = hci_api:decode_le(Subevent, LePacket),
	    gen_server:reply(State#state.from, {ok, Le}),
	    hci_drv:set_filter(State#state.hci,
			       State#state.saved_filter),
	    {noreply, State#state { from = undefined,
				    saved_filter = undefined }};
	_ ->
	    try hci_api:decode(Evt,Packet) of
		ReplyEvent ->
		    %% io:format("ReplyEvent ~p\n", [ReplyEvent]),
		    %% fixme: check that event match
		    gen_server:reply(State#state.from, {ok, ReplyEvent}),
		    hci_drv:set_filter(State#state.hci,
				       State#state.saved_filter),
		    {noreply, State#state { from = undefined,
					    saved_filter = undefined }}
	    catch
		error:Reason ->
		    io:format("decode error: ~p ~p\n", 
			      [Reason, erlang:get_stacktrace()]),
		    if State#state.from =:= undefined ->
			    {noreply, State};
		       true ->
			    hci_drv:set_filter(State#state.hci, 
					       State#state.saved_filter),
			    gen_server:reply(State#state.from, {error, Reason}),
			    {noreply, State#state { from = undefined,
						    saved_filter = undefined }}
		    end
	    end
    end;
handle_info(_Info, State) ->
    io:format("Got info: ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cname(<<0,_/binary>>) -> [];
cname(<<C,Cs/binary>>) -> [C|cname(Cs)];
cname(<<>>) -> [].
    

do_reply(Binary, State) ->
    case State#state.current_decode of
	undefined ->
	    hci_drv:set_filter(State#state.hci, State#state.saved_filter),
	    gen_server:reply(State#state.from, {ok, Binary}),
	    State#state { from=undefined, saved_filter=undefined };
	F when is_function(F,1) ->
	    Decoded = F(Binary),
	    gen_server:reply(State#state.from, {ok, Decoded}),
	    hci_drv:set_filter(State#state.hci, State#state.saved_filter),
	    State#state { from = undefined,
			  current_event = undefined,
			  current_opcode = undefined,
			  current_decode = undefined,
			  saved_filter = undefined
			}
    end.
