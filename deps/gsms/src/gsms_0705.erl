%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    SMS service using gsms_uart to access 07.05 commands
%%% @end
%%% Created : 24 Oct 2012 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(gsms_0705).

-behaviour(gen_server).

-export([start/2]).
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% api from gsms_router
-export([send/3]).   %% send straight
-export([write/3]).  %% write to sim but do not send
-export([scan_input/1]).


%% access 07.05 commands
-export([get_version/1]).
-export([get_manufacturer/1, get_model/1]).
-export([get_imei/1, get_msisdn/1, get_imsi/1]).
-export([get_activity_status/1]).
-export([get_network_registration_status/1]).
-export([get_signal_strength/1]).
-export([get_battery_status/1]).
-export([get_smsc/1]).

-export([list_unread_messages/1]).
-export([list_read_messages/1]).
-export([list_unsent_messages/1]).
-export([list_sent_messages/1]).
-export([list_all_messages/1]).
-export([list_indices/1]).

-export([delete_message/2]).
-export([delete_read_messages/1]).
-export([delete_sent_messages/1]).
-export([delete_unsent_messages/1]).
-export([delete_all_messages/1]).

-export([read_message/2]).

%% access 07.05 commands given the driver pid
-export([drv_reset/1]).
-export([drv_init_csms_service/1]).
-export([drv_check_csms_capability/1]).
-export([drv_set_csms_notification/1]).
-export([drv_set_csms_pdu_mode/1]).
-export([drv_get_version/1]).
-export([drv_get_manufacturer/1, drv_get_model/1]).
-export([drv_get_imei/1, drv_get_msisdn/1, drv_get_imsi/1]).
-export([drv_get_activity_status/1]).
-export([drv_get_network_registration_status/1]).
-export([drv_get_signal_strength/1]).
-export([drv_get_battery_status/1]).
-export([drv_get_smsc/1]).

-export([drv_list_unread_messages/1]).
-export([drv_list_read_messages/1]).
-export([drv_list_unsent_messages/1]).
-export([drv_list_sent_messages/1]).
-export([drv_list_all_messages/1]).
-export([drv_list_indices/1]).

-export([drv_delete_message/2]).
-export([drv_delete_message/3]).
-export([drv_delete_all_message/1]).
-export([drv_read_message/2]).
-export([drv_write_message/3]).
-export([drv_send_message/3]).

-include("../include/gsms.hrl").

-type qkey_t()    :: {#gsms_addr{},MRef::integer}.
-type isegment_t() :: {I::integer(),Ix::integer(),Pdu::#gsms_deliver_pdu{}}.
-type qelem_t()   :: {qkey_t(),TRef::reference(),N::integer,[isegment_t()]}.

-type osegment_t() :: {send | write,
		       I::integer,
		       N::integer,
		       SRef::reference(),
		       Notify::boolean(),
		       Sender::pid(), 
		       Pdu::#gsms_submit_pdu{}}.

-type uart_driver() :: pid() | atom().

-define(DEFAULT_SEND_DELAY, 0).
-define(DEFAULT_SEGMENT_TIMEOUT, 60000).
-define(DEFAULT_CONCAT_8BIT,     true).
-define(DEFAULT_CONCAT_SEQUENCE, true).
-define(DEFAULT_CONCAT_REF,  0).
-define(DEFAULT_SIMPIN, "").

-type gsms_option() ::
	{simpin, Pin::string()} |
	{bnumber, Number::string()} |
	{attributes, [{Key::atom(),Value::term()}]} |
	{segment_timeout, Timeout::timeout()} |
	{send_delay, Delay::timeout()} |
	{concat_8bit, EightBit :: boolean()} |
	{concat_seq,  Sequential :: boolean()}
	.

-type gsms_send_option() ::
	{ref, ConCatRef::uint16()} |
	{notify, boolean()}.

-record(state,
	{
	  id :: integer(),          %% id in gsms_router
	  drv :: pid(),             %% pid of the gsms_uart AT driver
	  ref :: reference(),       %% notification reference
	  %% state
	  drv_up  = false,                 %% drv ready to process
	  sending = false :: boolean(),    %% sending message outstanding?
	  inq = []  :: [qelem_t()],
	  outq = [] :: [osegment_t()],
	  concat_ref = ?DEFAULT_CONCAT_REF :: integer(),
	  %% config
	  bnumber = "" :: string(),    %% modem phone number
	  simpin = "" :: string(),     %% SIM pin when needed
	  segment_timeout :: timeout(),%% max wait for segment
	  send_delay :: timeout(),     %% delay between sending segments
	  concat_seq  :: boolean(),    %% concat ref is sequence or random
	  concat_8bit :: boolean(),    %% 8bit or 16bit
	  attributes = [] :: [{atom(),term()}]
	}).

%%%===================================================================
%%% API
%%%===================================================================

-spec send(Pid::pid(), Opts::[gsms_pdu_option()|gsms_send_option()],
	   Message::list()) ->
		  {ok, Ref::reference()} | {error, Reason::term()}.

%%  send options: pdu_options ++ [{notify,boolean()},{ref,ConCatRef}]

send(Pid, Opts, Message) ->
    gen_server:call(Pid, {send, self(), Opts, Message}).

-spec write(Pid::pid(), 
	    Opts::[gsms_pdu_option()|gsms_send_option()],
	    Message::list()) ->
		   {ok, Ref::reference()} | {error, Reason::term()}.

write(Pid, Opts, Message) ->
    gen_server:call(Pid, {write, self(), Opts, Message}).

%%
%% Check if we have messages to process in the inbox
%%
scan_input(Pid) ->
    Pid ! scan_input.

get_version(Pid) ->
    gen_server:call(Pid, get_version).
get_manufacturer(Pid) ->
    gen_server:call(Pid, get_manufacturer).
get_model(Pid) ->
    gen_server:call(Pid, get_model).
get_imei(Pid) ->
    gen_server:call(Pid, get_imei).
get_msisdn(Pid) ->
    gen_server:call(Pid, get_msisdn).
get_imsi(Pid) ->
    gen_server:call(Pid, get_imsi).
get_activity_status(Pid) ->
    gen_server:call(Pid, get_activity_status).
get_network_registration_status(Pid) ->
    gen_server:call(Pid, get_network_registration_status).
get_signal_strength(Pid) ->
    gen_server:call(Pid, get_signal_strength).
get_battery_status(Pid) ->
    gen_server:call(Pid, get_battery_status).
get_smsc(Pid) ->
    gen_server:call(Pid, get_smsc).

list_unread_messages(Pid) ->
    gen_server:call(Pid, list_unread_messages).
list_read_messages(Pid) ->
    gen_server:call(Pid, list_read_messages).    
list_unsent_messages(Pid) ->
    gen_server:call(Pid, list_unsent_messages).
list_sent_messages(Pid) ->
    gen_server:call(Pid, list_sent_messages).
list_all_messages(Pid) ->
    gen_server:call(Pid, list_all_messages).
list_indices(Pid) ->
    gen_server:call(Pid, list_indices).
    
delete_message(Pid, I) when is_integer(I), I>=0 ->
    gen_server:call(Pid, {delete_message, I}).
delete_read_messages(Pid) ->
    gen_server:call(Pid, {delete_messages,1}).
delete_sent_messages(Pid) ->  %% and read messages
    gen_server:call(Pid, {delete_messages,2}).
delete_unsent_messages(Pid) ->  %% and read,sent messages
    gen_server:call(Pid, {delete_messages,3}).
delete_all_messages(Pid) ->
    gen_server:call(Pid, {delete_messages,4}).

read_message(Pid,I) when is_integer(I), I>=0 ->
    gen_server:call(Pid, {read_messages,I}).
    
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start(Options) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

-spec start(Id::integer(),Opts::[gsms_option()]) ->
		   {ok,pid()} | {error,Reason::term()}.

start(Id, Opts) when is_integer(Id), is_list(Opts) ->
    gsms:start(),
    ChildSpec= {{?MODULE,Id}, {?MODULE, start_link, [Id,Opts]},
		permanent, 5000, worker, [?MODULE]},
    supervisor:start_child(gsms_if_sup, ChildSpec).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id, Opts) ->
    gen_server:start_link(?MODULE, [Id,Opts], []).

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
init([Id,Opts]) ->
    State0 = #state { simpin = ?DEFAULT_SIMPIN,
		      segment_timeout = ?DEFAULT_SEGMENT_TIMEOUT,
		      send_delay = ?DEFAULT_SEND_DELAY,
		      concat_seq = ?DEFAULT_CONCAT_SEQUENCE,
		      concat_8bit = ?DEFAULT_CONCAT_8BIT },
    {Opts1,State1} = setopts(Opts, State0),
    {ok,Pid} = gsms_uart:start_link(Opts1),
    {ok,Ref} = gsms_uart:subscribe(Pid),  %% subscribe to all events
    {ok, State1#state { id = Id,
			drv = Pid,
			ref = Ref }}.

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
handle_call({send,Sender,Opts,Body}, _From, State) ->
    case gsms_codec:make_sms_submit(Opts, Body) of
	{ok,PduList} ->
	    {CRef,Opts1} = next_concat_ref(Opts,State),
	    Notify = proplists:get_bool(notify, Opts1),
	    N = length(PduList),
	    SRef = make_ref(),
	    OutQ = State#state.outq ++
		[{send,I,N,SRef,Notify,Sender,Pdu} || 
		    {Pdu,I} <- lists:zip(PduList, lists:seq(1,N))],
	    State1 = sending(OutQ,State),
	    {reply, {ok,SRef}, State1#state { concat_ref = CRef}};
	Error ->
	    {reply, Error, State}
    end;
handle_call({write,Sender,Opts,Body}, _From, State) ->
    case gsms_codec:make_sms_submit(Opts, Body) of
	{ok,PduList} ->
	    {CRef,Opts1} = next_concat_ref(Opts,State),
	    Notify = proplists:get_bool(notify, Opts1),
	    N = length(PduList),
	    WRef = make_ref(),
	    OutQ = State#state.outq ++
		[{write,I,N,WRef,Notify,Sender,Pdu} || 
		    {Pdu,I} <- lists:zip(PduList, lists:seq(1,N))],
	    State1 = sending(OutQ,State),
	    {reply, {ok,WRef}, State1#state { concat_ref = CRef}};
	Error ->
	    {reply, Error, State}
    end;
handle_call({cancel,Ref}, _From, State) ->
    %% Remove and segments not sent for SRef
    %% Possibly sent a cancel command ? if supported
    OutQ =
	lists:foldr(
	  fun(E={_Operation,_I,_N,Ref1,_Notify,_Sender,_Pdu}, Acc) ->
		  if Ref1 =:= Ref -> Acc;
		     true -> [E|Acc]
		  end
	  end, [], State#state.outq),
    {reply, ok, State#state { outq = OutQ }};

handle_call(get_version, _From, State) ->
    if State#state.drv_up ->
	    {reply, drv_get_version(State#state.drv), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call(get_manufacturer, _From, State) ->
    if State#state.drv_up ->
	    {reply, drv_get_manufacturer(State#state.drv), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call(get_model, _From, State) ->
    if State#state.drv_up ->
	    {reply, drv_get_model(State#state.drv), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call(get_imei, _From, State) ->
    if State#state.drv_up ->
	    {reply, drv_get_imei(State#state.drv), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call(get_msisdn, _From, State) ->
    if State#state.drv_up ->
	    {reply, drv_get_msisdn(State#state.drv), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call(get_imsi, _From, State) ->
    if State#state.drv_up ->
	    {reply, drv_get_imsi(State#state.drv), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call(get_network_registration_status, _From, State) ->
    if State#state.drv_up ->
	    {reply, drv_get_network_registration_status(State#state.drv),
	     State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call(get_signal_strength, _From, State) ->
    if State#state.drv_up ->
	    {reply, drv_get_signal_strength(State#state.drv), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call(get_battery_status, _From, State) ->
    if State#state.drv_up ->
	    {reply, drv_get_battery_status(State#state.drv), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call(get_smsc, _From, State) ->
    if State#state.drv_up ->
	    {reply, drv_get_smsc(State#state.drv), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call({list_messages,N},_From,State) ->
    if State#state.drv_up ->
	    {reply, drv_list_messages(State#state.drv, N), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call({delete_message,I},_From,State) ->
    if State#state.drv_up ->
	    {reply, drv_delete_message(State#state.drv,I), State};
       true ->
	    {reply, {error, not_up}, State}
    end;
handle_call({delete_messages,F},_From,State) when F>0 ->
    if State#state.drv_up ->
	    {reply, drv_delete_message(State#state.drv,0,F), State};
       true ->
	    {reply, {error, not_up}, State}
    end;


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({gsms_event,Ref,Event}, State) when State#state.ref =:= Ref ->
    case Event of
	{cmti,[{"store",_Name},{"index", Ix}]} ->
	    %% read a stored message
	    case drv_read_message(State#state.drv, Ix) of
		{ok, Sms} ->
		    lager:debug("read_message: ~p", [Sms]),
		    State1 = handle_sms(Sms, Ix, State),
		    {noreply, State1};
		Error ->
		    lager:info("read_message failed: ~p\n", [Error]),
		    {noreply, State}
	    end;
	{data,"+CREG:"++Params} ->
	    case erl_scan:string(Params) of
		{ok,[{integer,_,Status}|_],_} ->
		    %% we do not use the the access value
		    gsms_router:input_from(State#state.bnumber,{creg,Status}),
		    {noreply, State};
		_ ->
		    lager:info("event ignored ~p\n", [Event]),
		    {noreply, State}
	    end;
	_ ->
	    lager:info("event ignored ~p\n", [Event]),
	    {noreply, State}
    end;
handle_info({gsms_uart, Pid, up}, State) when State#state.drv =:= Pid ->
    %% gsms_uart is up and running 
    %% FIXME: check if sim is locked & unlock if possible
    gsms_uart:at(Pid,"E0"),  %% disable echo (again)
    timer:sleep(100),        %% help?
    ok = drv_check_csms_capability(Pid),
    ok = drv_set_csms_pdu_mode(Pid),
    ok = drv_set_csms_notification(Pid),
    BNumber = if State#state.bnumber =:= "" ->
		      case drv_get_msisdn(Pid) of
			  ok -> "";
			  {ok,B} -> B
		      end;
		 true -> 
		      State#state.bnumber
	      end,
    State1 = State#state { bnumber = BNumber, drv_up = true },
    ok = gsms_router:join(BNumber, State1#state.attributes),
    %% make sure we scan messages that arrived while we where gone,
    scan_input(self()),
    lager:debug("running state = ~p", [State1]),
    {noreply, State1};

handle_info({timeout,TRef,{cancel,Key}}, State) ->
    %% reject incoming message because of timeout
    Q = State#state.inq,
    case lists:keytake(Key, 1, Q) of
	false ->
	    lager:warning("message from ~p not present in timeout", [Key]),
	    {noreply, State};
	{value,{_,TRef,_N,Segments},Q0} ->
	    lager:warning("message from ~p dropped, timeout", [Key]),
	    lists:foreach(
	      fun({_I,Ix,_}) ->
		      drv_delete_message(State#state.drv, Ix)
	      end, Segments),
	    {noreply, State#state {inq = Q0}}
    end;
handle_info(scan_input, State) ->
    %% scan_input should be run after startup to get buffered
    %% messages stored in SIM card while application was down
    case drv_list_indices(State#state.drv) of
	{ok,[Ixs | _]} ->
	    Pid = self(),
	    Ref = State#state.ref,
	    lists:foreach(
	      fun(Ix) ->
		      Event = {cmti,[{"store","SM"},{"index",Ix}]},
		      Pid ! {gsms_event,Ref,Event}
	      end, expand_index_list(Ixs)),
	    {noreply, State};
	Reply ->
	    lager:debug("list_indices reply ~p", [Reply]),
	    {noreply, State}
    end;
handle_info(send, State) ->
    %% send next PDU
    case State#state.outq of
	[{send,I,N,SRef,Notify,Sender,Pdu}|OutQ] ->
	    gsms_codec:dump_yang(Pdu),
	    Bin = gsms_codec:encode_sms(Pdu),
	    Hex = gsms_codec:binary_to_hex(Bin),
	    Len = (length(Hex)-2) div 2,
	    Reply =
		gsms_uart:atd(State#state.drv,
			      "+CMGS="++integer_to_list(Len),Hex),
	    lager:debug("send status segment ~w of ~w response=~p\n", 
			[I,N,Reply]),
	    %% Fixme handle Reply=error!!! cancel rest of segments etc
	    if I =:= N, Notify =:= true ->
		    Sender ! {gsms_notify, SRef, ok};
	       true ->
		    ok
	    end,
	    {noreply, sending(OutQ, State)};
	[{write,I,N,SRef,Notify,Sender,Pdu}|OutQ] ->
	    gsms_codec:dump_yang(Pdu),
	    Bin = gsms_codec:encode_sms(Pdu),
	    Hex = gsms_codec:binary_to_hex(Bin),
	    Len = (length(Hex)-2) div 2,
	    Reply = gsms_uart:atd(State#state.drv,
				  "+CMGW="++integer_to_list(Len),Hex),
	    lager:debug("wrote status segment ~w of ~w response=~p\n", 
			[I,N,Reply]),
	    %% Fixme handle Reply=error!!! cancel rest of segments etc
	    if I =:= N, Notify -> %% assume ok 
		    Sender ! {gsms_notify, SRef, ok};
	       true ->
		    ok
	    end,
	    {noreply, sending(OutQ, State)};
	[] ->
	    {noreply, State#state { sending = false }}
    end;
handle_info(_Info, State) ->
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


-spec drv_reset(Drv::uart_driver()) -> ok.

drv_reset(Drv)  ->
    gsms_uart:at(Drv,"Z"),
    gsms_uart:at(Drv,"E0"),
    drv_set_csms_pdu_mode(Drv),
    drv_set_csms_notification(Drv).


-spec drv_init_csms_service(Drv::uart_driver()) -> ok.    
drv_init_csms_service(Drv) ->
    ok = drv_check_csms_capability(Drv),
    ok = drv_set_csms_pdu_mode(Drv),
    ok = drv_set_csms_notification(Drv),
    %% how do we read the stored pdu like they where receive and when?
    %% trigger automatic read of stored SMS when a subsrciber is 
    %% registered? subscriber need to ack in order to delete the
    %% stored SMS
    ok.


-spec drv_check_csms_capability(Drv::uart_driver()) -> ok.
drv_check_csms_capability(Drv) ->
    case gsms_uart:at(Drv,"+CSMS=0") of
	{ok, "+CSMS:"++Storage} ->
	    lager:debug("sms_capability: +CSMS: ~s", [Storage]);
	Error ->
	    Error
    end.

-spec drv_set_csms_pdu_mode(Drv::uart_driver()) -> ok.

drv_set_csms_pdu_mode(Drv) ->  
    gsms_uart:at(Drv,"+CMGF=0").

%% AT+CNMI=1,1,0,0,0 Set the new message indicators.
%%
%% AT+CNMI=<mode>,<mt>,<bm>,<ds>,<bfr>
%% 
%% <mode>=1 discard unsolicited result codes indication when TA – 
%%          TE link is reserved.
%% <mt>=1 SMS-DELIVERs are delivered to the SIM and routed using 
%%        unsolicited code.
%% <bm>=0 no CBM indications are routed to the TE.
%% <ds>=0 no SMS-STATUS-REPORTs are routed.
%% <bfr>=0 TA buffer of unsolicited result codes defined within this
%%         command is flushed to the TE.
%% OK Modem Response.
drv_set_csms_notification(Drv) -> 
    gsms_uart:at(Drv,"+CNMI=1,1,0,0,0").

%% pick up information about various things
drv_get_version(Drv) -> 
    gsms_uart:at(Drv,"+CGMR").
    
drv_get_manufacturer(Drv) ->
    gsms_uart:at(Drv,"+CGMI").

drv_get_model(Drv) ->
    gsms_uart:at(Drv,"+CGMM").

drv_get_imei(Drv) ->
    gsms_uart:at(Drv,"+CGSN").

drv_get_msisdn(Drv) ->
    gsms_uart:at(Drv,"+CNUM").

drv_get_imsi(Drv) ->
    gsms_uart:at(Drv,"+CIMI").

drv_get_activity_status(Drv) -> 
    gsms_uart:eat(Drv,"+CPAS").

drv_get_network_registration_status(Drv) ->
    gsms_uart:at(Drv,"+CREG?").

drv_get_signal_strength(Drv) -> 
    gsms_uart:at(Drv,"+CSQ").

drv_get_battery_status(Drv) ->
    gsms_uart:at(Drv,"+CBC").

drv_get_smsc(Drv) ->
    gsms_uart:at(Drv, "+CSCA?").
    
%% SMS commands

drv_list_unread_messages(Drv) ->
    drv_list_messages(Drv, 0).
drv_list_read_messages(Drv)   ->  
    drv_list_messages(Drv, 1).
drv_list_unsent_messages(Drv) ->  
    drv_list_messages(Drv, 2).
drv_list_sent_messages(Drv)   ->  
    drv_list_messages(Drv, 3).
drv_list_all_messages(Drv)    ->  
    drv_list_messages(Drv, 4).

drv_list_messages(Drv, N) when is_integer(N), N>=0, N=<4 ->
    gsms_uart:at(Drv, "+CMGL="++integer_to_list(N)).

%% message list
drv_list_indices(Drv) ->
    case gsms_uart:at(Drv,"+CMGD=?") of
	{ok,"+CMGD:"++Params} ->
	    case erl_scan:string(Params) of
		{ok,Ts,_} ->
		    parse_index_lists(Ts);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.


%% +CMGD=I    == +CMDG=I,0   only delete message I
%% +CMGD=I,1  == +CMGD=0,1   delete ALL "read" messages
%% +CMGD=I,2  == +CMGD=0,2   delete ALL "read","sent" messages
%% +CMGD=I,3  == +CMGD=0,3   delete ALL "read","sent", "unsent" messages
%% +CMFD=I,4  == +CMGD=0,4   delete ALL messages

drv_delete_message(Drv,I) when is_integer(I), I>=0 ->
    drv_delete_message(Drv,I,0).

drv_delete_message(Drv,I,F) when is_integer(I), I>=0, F>=0,F=<4 ->
    gsms_uart:at(Drv,"+CMGD="++integer_to_list(I)++","++integer_to_list(F)).

drv_delete_all_message(Drv) ->
    drv_delete_message(Drv,1,4).

drv_read_message(Drv,I) when is_integer(I), I>=0 ->
    case gsms_uart:at(Drv,"+CMGR="++integer_to_list(I)) of
	ok ->
	    {error, no_such_index};
	{ok,["+CMGR: "++_StatStoreLen,HexPdu]} ->
	    gsms_codec:decode_in_hex(HexPdu);
	{error, Error} ->
	    {error, cms_error(Error)}
    end.


%% test send message
drv_send_message(Drv,Opts,Body) ->
    case gsms_codec:make_sms_submit(Opts, Body) of
	{ok,PduList} ->
	    lists:foreach(
	      fun(Pdu) ->
		      gsms_codec:dump_yang(Pdu),
		      Bin = gsms_codec:encode_sms(Pdu),
		      Hex = gsms_codec:binary_to_hex(Bin),
		      Len = (length(Hex)-2) div 2,
		      gsms_uart:atd(Drv,"+CMGS="++integer_to_list(Len),Hex)
	      end, PduList),
	    ok;
	Error ->
	    Error
    end.

%% test write message
drv_write_message(Drv,Opts,Body) ->
    case gsms_codec:make_sms_submit(Opts, Body) of
	{ok,PduList} ->
	    lists:foreach(
	      fun(Pdu) ->
		      gsms_codec:dump_yang(Pdu),
		      Bin = gsms_codec:encode_sms(Pdu),
		      Hex = gsms_codec:binary_to_hex(Bin),
		      Len = (length(Hex)-2) div 2,
		      gsms_uart:atd(Drv,"+CMGW="++integer_to_list(Len),Hex)
	      end, PduList),
	    ok;
	Error ->
	    Error
    end.

parse_index_lists(Ts) ->
    parse_index_lists(Ts, []).

parse_index_lists([{'(',_}|Ts], Acc) ->   parse_index_list(Ts, [], Acc);
parse_index_lists([], Acc) ->   {ok,lists:reverse(Acc)};
parse_index_lists(_Ts, _Acc) ->  {error, {syntax_error, _Ts}}.

parse_index_lists1([{',',_}|Ts], Acc) -> parse_index_lists(Ts, Acc);
parse_index_lists1([], Acc) -> {ok,lists:reverse(Acc)};
parse_index_lists1(_Ts, _Acc) ->  {error, {syntax_error, _Ts}}.

%%
%% ival = <i> '-' <j>
%% ival = <i>
%% ival-list = '(' (ival (',' ival)*) ? ')'
%%
parse_index_list([{integer,_,I},{'-',_},{integer,_,J}|Ts],Iv,Acc) ->
    parse_index_list1(Ts,[{I,J}|Iv],Acc);
parse_index_list([{integer,_,I}|Ts],Iv,Acc) ->
    parse_index_list1(Ts,[I|Iv],Acc);
parse_index_list([{')',_}|Ts],Iv,Acc) ->
    parse_index_lists1(Ts,[lists:reverse(Iv)|Acc]);
parse_index_list(_Ts, _Iv, _Acc) ->
    {error, {syntax_error,_Ts}}.    

parse_index_list1([{',',_},{integer,_,I},{'-',_},{integer,_,J}|Ts],Iv,Acc) ->
    parse_index_list1(Ts,[{I,J}|Iv],Acc);
parse_index_list1([{',',_},{integer,_,I}|Ts],Iv,Acc) ->
    parse_index_list1(Ts,[I|Iv],Acc);
parse_index_list1([{')',_}|Ts],Iv,Acc) ->
    parse_index_lists1(Ts,[lists:reverse(Iv)|Acc]);
parse_index_list1(_Ts,_Iv,_Acc) ->
    {error, {syntax_error,_Ts}}.

expand_index_list([{I,J}|Is]) when is_integer(I),is_integer(J),I>=0,I=<J ->
    lists:seq(I,J) ++ expand_index_list(Is);
expand_index_list([I|Is]) when is_integer(I), I>=0 ->
    [I|expand_index_list(Is)];
expand_index_list([]) ->
    [].

cms_error(Code) when is_list(Code) ->
    Code ++ ": " ++ cms_error_string(list_to_integer(Code)).

cms_error_string(300) -> "Phone failure";
cms_error_string(301) -> "SMS service of phone reserved";
cms_error_string(302) -> "Operation not allowed";
cms_error_string(303) -> "Operation not supported";
cms_error_string(304) -> "Invalid PDU mode parameter";
cms_error_string(305) -> "Invalid text mode parameter";
cms_error_string(310) -> "SIM not inserted";
cms_error_string(311) -> "SIM PIN necessary";
cms_error_string(312) -> "PH-SIM PIN necessary";
cms_error_string(313) -> "SIM failure";
cms_error_string(314) -> "SIM busy";
cms_error_string(315) -> "SIM wrong";
cms_error_string(320) -> "Memory failure";
cms_error_string(321) -> "Invalid memory index";
cms_error_string(322) -> "Memory full";
cms_error_string(330) -> "SMSC (message service center) address unknown";
cms_error_string(331) -> "No network service";
cms_error_string(332) -> "Network timeout";
cms_error_string(500) -> "Unknown error";
cms_error_string(512) -> "Manufacturer specific";
cms_error_string(_) -> "Unknown".


setopts(Opts, State) ->
    setopts(Opts, State, []).

setopts([Opt|Opts], State, Opts1) ->
    case Opt of
	{simpin,Pin} when is_list(Pin) ->
	    setopts(Opts, State#state { simpin = Pin }, Opts1);
	{bnumber,BNumber} when is_list(BNumber) ->
	    setopts(Opts, State#state { bnumber = BNumber }, Opts1);
	{attributes,As} when is_list(As) ->
	    setopts(Opts, State#state { attributes = As }, Opts1);
	{segment_timeout,T} when is_integer(T), T >= 0 ->
	    setopts(Opts, State#state { segment_timeout = T }, Opts1);
	{send_delay, T}  when is_integer(T), T >= 0 ->
	    setopts(Opts, State#state { send_delay = T }, Opts1);
	{concat_8bit, B} when is_boolean(B) ->
	    setopts(Opts, State#state { concat_8bit = B }, Opts1);
	{concat_seq, B} when is_boolean(B) ->
	    setopts(Opts, State#state { concat_seq = B }, Opts1);
	_ ->
	    setopts(Opts, State, [Opt|Opts1])
    end;
setopts([], State, Opts1) ->
    {lists:reverse(Opts1), State}.


next_concat_ref(Opts, State) ->
    case lists:keymember(ref,1,Opts) of
	true  -> %% ref is givent in the pdu 
	    {State#state.concat_ref, Opts};
	false ->
	    CRef0 = if State#state.concat_seq ->
			    State#state.concat_ref + 1;
		       true ->
			    random:unifrom(16#10000)-1
		    end,
	    CRef1 = if State#state.concat_8bit ->
			    CRef0 band 16#ff;
		       true ->
			    CRef0 band 16#ffff
		    end,
	    {CRef1, [{ref,CRef1} | Opts]}
    end.

%%
%% Continue send segments or we are done ?
%%
sending([], State) ->
    State#state { sending=false, outq = []};
sending(OutQ, State) ->
    erlang:send_after(State#state.send_delay, self(), send),
    State#state { sending=true, outq = OutQ }.

handle_sms(Sms, Ix, State) ->
    %% check if this is Sms is part of an concatenated message
    case lists:keyfind(concat, 1, Sms#gsms_deliver_pdu.udh) of
	false -> %% singleton message, forward
	    forward_sms(Sms, [Ix], State);
	{concat,_MRef,1,1} -> %% singleton message, forward !
	    forward_sms(Sms, [Ix], State);
	{concat,MRef,N,I} when I > 0, I =< N ->
	    %% check if we already have segments stored
	    Key = {Sms#gsms_deliver_pdu.addr,MRef},
	    Q = State#state.inq,
	    Tmo = State#state.segment_timeout,
	    case lists:keytake(Key, 1, Q) of
		false -> %% new enqueue
		    TRef = start_timer(Tmo,{cancel,Key}),
		    Segments = [{I,Ix,Sms}],
		    Q1 = [{Key,TRef,N,Segments} | Q],
		    State#state { inq = Q1 };
		{value,{_,TRef,N,Segments},Q0} ->
		    case lists:keymember(I,1,Segments) of
			false ->
			    stop_timer(TRef),
			    Segments1 = [{I,Ix,Sms}|Segments],
			    case length(Segments1) of
				N ->
				    {Sms1,Ixs} = assemble_sms(Segments1),
				    State1 = State#state { inq=Q0},
				    forward_sms(Sms1,Ixs,State1);
				_ ->
				    TRef1 = start_timer(Tmo,{cancel,Key}),
				    Q1 = [{Key,TRef1,N,Segments1} | Q0],
				    State#state { inq=Q1 }
			    end;
			true ->
			    lager:warning("segment ~w already received!", [I]),
			    State
		    end
	    end;
	Concat ->
	    lager:warning("bad concat udh element ~p", [Concat]),
	    State
    end.

%%
%% Send sms to gsms_router & delete stored segments
%%
forward_sms(Sms, Ixs, State) ->
    gsms_router:input_from(State#state.bnumber, Sms),
    lists:foreach(
      fun(Ix) ->
	      drv_delete_message(State#state.drv, Ix)
      end, Ixs),
    State.

%% assemble sms segments into one message,
%% assume all I 1..N are present (by pigeon hole principle)
-spec assemble_sms([isegment_t()]) ->
			  {#gsms_deliver_pdu{}, [Ix::integer()]}.
		      
assemble_sms(Segments) ->
    [{1,Ix,Sms} | Segments1] = lists:keysort(1, Segments),
    Udh0 = lists:keydelete(concat, 1, Sms#gsms_deliver_pdu.udh),
    Sms0 = Sms#gsms_deliver_pdu { udh=Udh0, udl=0, ud=[] },
    assemble_(Segments1,
	     [Sms#gsms_deliver_pdu.ud], Sms#gsms_deliver_pdu.udl,
	     [Ix], Sms0).

assemble_([{_,Ix,Sms}|Segments], Uds, Udl, Ixs, Sms0) ->
    assemble_(Segments,
	     [Sms#gsms_deliver_pdu.ud|Uds], Sms#gsms_deliver_pdu.udl + Udl,
	     [Ix|Ixs], Sms0);
assemble_([], Uds, Udl, Ixs, Sms0) ->
    Ud = lists:append(lists:reverse(Uds)),
    {Sms0#gsms_deliver_pdu { udl = Udl, ud = Ud }, lists:reverse(Ixs)}.
	     

start_timer(Time, Message) ->
    erlang:start_timer(Time, self(), Message).

stop_timer(undefined) ->
    ok;
stop_timer(Ref) ->
    erlang:cancel_timer(Ref),
    receive
	{timeout, Ref, _} -> 
	    ok
    after 0 -> 
	    ok
    end.
