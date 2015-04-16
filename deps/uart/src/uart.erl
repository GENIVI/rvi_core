%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Cross platform tty interface.
%%% @end
%%% Created : 29 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(uart).

-export([open/2, close/1]).
-export([send/2, send_char/2]).
-export([recv/2, recv/3]).
-export([async_recv/2, async_recv/3, async_send/2]).
-export([unrecv/2]).
-export([break/2, hangup/1, flow/2]).
-export([get_modem/1, set_modem/2, clear_modem/2]).
-export([options/0, validate_opts/1, validate_opt/2]).
-export([setopt/3, setopts/2]).
-export([getopt/2, getopts/2]).
-export([controlling_process/2]).
-export([i/0]).

%% testing
-export([encode_opt/2]).

-define(UART_CMD_OPEN,      1).
-define(UART_CMD_HANGUP,    2).
-define(UART_CMD_CLOSE,     4).
-define(UART_CMD_FLOW,      5).
-define(UART_CMD_BREAK,     7).
-define(UART_CMD_SETOPTS,   8).
-define(UART_CMD_GETOPTS,   9).
-define(UART_CMD_SENDCHAR,  10).
-define(UART_CMD_SEND,      11).
-define(UART_CMD_GET_MODEM, 12).
-define(UART_CMD_SET_MODEM, 13).
-define(UART_CMD_CLR_MODEM, 14).
-define(UART_CMD_UNRECV,    15).
-define(UART_CMD_RECV,      16).
-define(UART_CMD_CONNECT,   17).

%% Option bits are also used as bit numbers, so do not exceed 32.
-define(UART_OPT_DEVICE, 1).
-define(UART_OPT_IBAUD,  2).
-define(UART_OPT_OBAUD,  3).
-define(UART_OPT_CSIZE,  4).
-define(UART_OPT_BUFSZ, 5).
-define(UART_OPT_BUFTM, 6).
-define(UART_OPT_STOPB,  7).
-define(UART_OPT_PARITY, 8).
-define(UART_OPT_IFLOW, 9).
-define(UART_OPT_OFLOW, 10).
-define(UART_OPT_XOFFCHAR, 11).
-define(UART_OPT_XONCHAR,  12).
-define(UART_OPT_EOLCHAR,  13).
%% -define(UART_OPT_14,    14).
-define(UART_OPT_ACTIVE,   15).
-define(UART_OPT_DELAY_SEND, 16).
-define(UART_OPT_DELIVER, 17).
-define(UART_OPT_MODE, 18).
%% -define(UART_OPT_19,    19).
-define(UART_OPT_HEADER, 20).
-define(UART_OPT_PACKET, 21).
-define(UART_OPT_PSIZE, 22).
-define(UART_OPT_HIGH,  23).
-define(UART_OPT_LOW, 24).
-define(UART_OPT_SENDTMO, 25).  %% send timeout
-define(UART_OPT_CLOSETMO, 26).  %% send close timeout
-define(UART_OPT_BUFFER,   27).
-define(UART_OPT_DEBUG,    28).
-define(UART_OPT_EXITF,     29).
-define(UART_OPT_PTYPKT,    30).
%% -define(UART_OPT_31,    31).

-define(UART_PB_LITTLE_ENDIAN, 16#00008000). %% UART_PB_<n> 
-define(UART_PB_BYTES_MASK,    16#00000F00). %% UART_PB_<n> 0..8 allowed
-define(UART_PB_FIXED_MASK,    16#FFFF0000). %% UART_PB_RAW
-define(UART_PB_TYPE_MASK,     16#000000FF). %% UART_PB_x

-define(UART_PB_RAW,           0).
-define(UART_PB_N,             1).
-define(UART_PB_LINE_LF,       2).
-define(UART_PB_BASIC_0710,    3).
-define(UART_PB_ADVANCED_0710, 4).
-define(UART_PB_GSM_0710,      5).

-define(UART_PASSIVE, 0).
-define(UART_ACTIVE,  1).
-define(UART_ONCE,    2).

-define(UART_PARITY_NONE, 0).
-define(UART_PARITY_ODD,  1).
-define(UART_PARITY_EVEN, 2).
-define(UART_PARITY_MARK, 3).
-define(UART_PARITY_SPACE, 4).

-define(UART_DELIVER_PORT, 0).
-define(UART_DELIVER_TERM, 1).

-define(UART_MODE_LIST,   0).
-define(UART_MODE_BINARY, 1).

-define(UART_OK,      0).
-define(UART_ERROR,   1).
-define(UART_OPTIONS, 2).

-define(UART_DTR,  16#0002).
-define(UART_RTS,  16#0004).
-define(UART_CTS,  16#0008).
-define(UART_CD,   16#0010).
-define(UART_RI,   16#0020).
-define(UART_DSR,  16#0040).
-define(UART_SW,   16#8000).

-define(bool(X), if (X) -> 1; true -> 0 end).

-define(is_uint8(X),  (((X) band (bnot 16#ff)) =:= 0)).
-define(is_uint16(X),  (((X) band (bnot 16#ffff)) =:= 0)).
-define(is_uint32(X),  (((X) band (bnot 16#ffffffff)) =:= 0)).

-define(is_uart(P),  is_port((P))).


-type uart() :: port().

-type uart_option() :: 
	device | baud | ibaud | obaud | csize | bufsz |
	buftm | stopb | parity | iflow | oflow | xonchar |
	xoffchar | eolchar | active | delay_send |
	header | packet | packet_size | deliver | mode |
	buffer | exit_on_close | debug.

%%  Todo:
%% 	high_watermark | low_watermark | send_timeout | send_timeout_close | 

-type uart_input_pins()  ::  cts | cd | ri | dcr.
-type uart_output_pins() ::  dtr | rts.
-type uart_modem_pins() :: uart_input_pins() | uart_output_pins().

%%--------------------------------------------------------------------
%% @doc
%%   This function is for documentations purpose.
%%
%%   List of available options:
%% <ul>
%% <li> `{device, "//pty" | string()}' </li>
%% <li> `{ibaud, baudrate()}' </li>
%% <li> `{obaud, baudrate()}' </li>
%% <li> `{baud, baudrate()}' </li>
%% <li> `{csize, 5|6|7|8}' </li>
%% <li> `{stopb, 1|2|3}' </li>
%% <li> `{parity,none|odd|even|mark|space}' </li>
%% <li> `{iflow, [sw|rts|dtr]}' </li>
%% <li> `{oflow, [sw|cts|dsr|cd]}' </li>
%% <li> `{xonchar, byte()}' </li>
%% <li> `{xoffchar, byte()}' </li>
%% <li> `{eolchar, byte()}' </li>
%% <li> `{active, true | false | once}' </li>
%% <li> `{delay_send, boolean()}' </li>
%% <li> `{header, size()}' </li>
%% <li> `{packet, packet_type()}' <br/> 
%%      `      packet_type() ::= -8..-1|0|1..8,line,{size,0..65535}' </li>
%% <li> `{packet_size, integer()}' </li>
%% <li> `{deliver, port | term}' </li>
%% <li> `{mode,    list | binary}' </li>
%% <li> `{buffer,  integer()}' </li>
%% <li> `{exit_on_close, boolean()}' </li>
%% <li> `{bufsz, 0..255}' - Max low level uart buffer size </li>
%% <li> `{buftm, 0..25500}' - Inter character timeout </li>
%% <li> `{debug, log_level()} - Set debug level</li>
%% <li> `{ptypkt, boolean()}  - Set pty packet mode</li>
%% </ul>
%% @end
%%--------------------------------------------------------------------
-spec options() -> atom().

options() ->
    [
     device,
     baud,
     ibaud,
     obaud,
     csize,
     bufsz,
     buftm,
     stopb,
     parity,
     iflow,
     oflow,
     xonchar,
     xoffchar,
     eolchar,
     active,
     delay_send,
     header,
     packet,
     packet_size,
     deliver,
     mode,
     buffer,
     exit_on_close,
     debug,
     ptypkt
    ].

%%--------------------------------------------------------------------
%% @doc Opens a tty device.
%%
%%   The device name `//pty' is reserved for opening a pseudo terminal
%%   using the openpty call. The slave device is accessed through the
%%   device option. See {@link options/0} for a description of 
%%   available options.
%% @end
%%--------------------------------------------------------------------
-spec open(DeviceName::iolist(), Options::[{uart_option(),term()}]) ->
		  {ok,uart()} | {error,term()}.

open(DeviceName, Opts) ->
    Path = code:priv_dir(uart),
    {Type,_} = os:type(),
    Driver = "uart_drv",
    case load_driver(Path, Driver) of
	ok ->
	    Command =
		case proplists:get_bool(ftdi, Opts) of
		    true -> Driver ++ " ftdi";
		    false -> Driver ++ " " ++ atom_to_list(Type)
		end,
	    Opts1 = proplists:delete(ftdi, Opts),
	    Uart = erlang:open_port({spawn_driver, Command}, [binary]),
	    Opts2 = [{ibaud,9600},{device,DeviceName} | Opts1],
	    case setopts(Uart, Opts2) of
		ok ->
		    {ok,Uart};
		Error ->
		    close(Uart),
		    Error
	    end;

	Err={error,Error} ->
	    io:format("Error: ~s\n", [erl_ddll:format_error_int(Error)]),
	    Err
    end.

%%--------------------------------------------------------------------
%% @doc List information about open uart ports
%% @end
%%--------------------------------------------------------------------

i() ->
    Us = lists:filter(
	   fun(P) -> 
		   {_,Name} = erlang:port_info(P, name),
		   lists:prefix("uart_drv ",Name)
	   end, erlang:ports()),
    lists:foreach(
      fun(U) ->
	      try fmt_(U) of
		  Fmt ->
		      io:put_chars([Fmt,"\n"])
	      catch
		  _ -> ignore
	      end
      end, Us).

fmt_(U) ->
    {ok,Opts} = getopts(U, [device,baud,csize,parity,stopb]),
    {_,Connected} = erlang:port_info(U, connected),
    io_lib:format("~s ~w ~w~s~w connected to ~p",
		  [proplists:get_value(device,Opts),
		   proplists:get_value(baud,Opts),
		   proplists:get_value(csize,Opts),
		   case proplists:get_value(parity,Opts) of
		       none -> "N";
		       even -> "E";
		       odd  -> "O";
		       mark -> "M"
		   end, proplists:get_value(stopb,Opts),
		   Connected]).

%%--------------------------------------------------------------------
%% @doc Close a tty device.
%% @end
%%--------------------------------------------------------------------

-spec close(Uart::uart()) -> true.

close(Uart) when ?is_uart(Uart) ->
    (catch erlang:port_close(Uart)),
    true.

%%--------------------------------------------------------------------
%% @doc Get single option value.
%% See {@link options/0} for available options.
%% @end
%%--------------------------------------------------------------------
-spec getopt(Uart::uart(), Option::uart_option()) ->
		    {ok,term()} | {error,term()}.
    
getopt(Uart, baud) ->
    getopt(Uart, ibaud);
getopt(Uart, Opt) ->
    case command(Uart, ?UART_CMD_GETOPTS, <<(encode_opt(Opt))>>) of
	{ok,[{_,Value}]} -> {ok,Value};
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Get multiple option values.
%% @end
%%--------------------------------------------------------------------

-spec getopts(Uart::uart(), Option::[uart_option()]) ->
		     {ok,[{uart_option(),term()}]} | {error,term()}.

getopts(Uart, Opts) when ?is_uart(Uart), is_list(Opts) ->
    Opts1 = translate_getopts(Opts),
    Data = << <<(encode_opt(Opt))>> || Opt <- Opts1 >>,
    case command(Uart, ?UART_CMD_GETOPTS, Data) of
	{ok, Values} ->
	    {ok, translate_getopts_reply(Opts,Values)};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc
%%   Set single option.
%%
%% See {@link options/0} for available options.
%% @end
%%--------------------------------------------------------------------
-spec setopt(Uart::uart(), Option::uart_option(), Value::term()) ->
		    ok | {error,term()}.

setopt(Uart, Opt, Value) ->
    setopts(Uart, [{Opt,Value}]).

%%--------------------------------------------------------------------
%% @doc
%%   Set multiple options.
%% @end
%%--------------------------------------------------------------------
-spec setopts(Uart::uart(), Options::[{uart_option(),term()}]) ->
		     ok | {error,term()}.

setopts(Uart, Opts) when ?is_uart(Uart), is_list(Opts) ->
    Opts1 = translate_set_opts(Opts),
    Data = << <<(encode_opt(Opt,Value))/binary>> || {Opt,Value} <- Opts1 >>,
    command(Uart, ?UART_CMD_SETOPTS, Data).

%%--------------------------------------------------------------------
%% @doc
%%   Send break for Duration number of milliseconds .
%% @end
%%--------------------------------------------------------------------
-spec break(Uart::uart(), Duration::non_neg_integer()) ->
		   ok | {error,term()}.
break(Uart,Duration) when ?is_uart(Uart),
			  is_integer(Duration), Duration > 0 ->
    command(Uart, ?UART_CMD_BREAK, <<Duration:32>>).

%%--------------------------------------------------------------------
%% @doc
%%   Hangup
%% @end
%%--------------------------------------------------------------------
-spec hangup(Uart::uart()) ->
		   ok | {error,term()}.
hangup(Uart) when ?is_uart(Uart) ->
    command(Uart, ?UART_CMD_HANGUP, []).

%%--------------------------------------------------------------------
%% @doc
%%   Manage input and output flow control 
%% @end
%%--------------------------------------------------------------------
-spec flow(Uart::uart(), Mode::(input_off|input_on|output_off|output_on)) ->
		 ok | {error,term()}.

flow(Uart, input_off) when ?is_uart(Uart) ->
    command(Uart, ?UART_CMD_FLOW, [0]);
flow(Uart, input_on) when ?is_uart(Uart) ->
    command(Uart, ?UART_CMD_FLOW, [1]);
flow(Uart, output_off) when ?is_uart(Uart) ->
    command(Uart, ?UART_CMD_FLOW, [2]);
flow(Uart, output_on) when ?is_uart(Uart) ->
    command(Uart, ?UART_CMD_FLOW, [3]).

%%--------------------------------------------------------------------
%% @doc
%%   Get modem pins status.
%% @end
%%--------------------------------------------------------------------
-spec get_modem(Uart::uart()) ->
		       {ok, [uart_modem_pins()]} |
		       {error, term()}.

get_modem(Uart) ->
    command(Uart, ?UART_CMD_GET_MODEM, []).

%%--------------------------------------------------------------------
%% @doc
%%   Set modem pins.
%% @end
%%--------------------------------------------------------------------

-spec set_modem(Uart::uart(), Flags::[uart_modem_pins()]) ->
		       ok | {error, term()}.

set_modem(Uart, Fs) when is_list(Fs) ->
    Flags = encode_flags(Fs),
    command(Uart, ?UART_CMD_SET_MODEM, <<Flags:32>>).

%%--------------------------------------------------------------------
%% @doc
%%   Clear modem pins.
%% @end
%%--------------------------------------------------------------------

-spec clear_modem(Uart::uart(), Flags::[uart_modem_pins()]) ->
			 ok | {error, term()}.
clear_modem(Uart, Fs) when ?is_uart(Uart), is_list(Fs) ->
    Flags = encode_flags(Fs),
    command(Uart, ?UART_CMD_CLR_MODEM, <<Flags:32>>).

%%--------------------------------------------------------------------
%% @doc
%%   Send characters
%% @end
%%--------------------------------------------------------------------

-spec send(Uart::uart(), Data::iolist()) ->
		  ok | {error, term()}.

send(Port, [C]) when is_port(Port), ?is_uint8(C) ->
    command(Port, ?UART_CMD_SENDCHAR, [C]);
send(Port, <<C>>) when is_port(Port) ->
    command(Port, ?UART_CMD_SENDCHAR, [C]);
send(Port, Data) when is_port(Port),is_list(Data) ->
    command(Port, ?UART_CMD_SEND, Data);
send(Port, Data) when is_port(Port), is_binary(Data) ->
    command(Port, ?UART_CMD_SEND, Data).

%%--------------------------------------------------------------------
%% @doc
%%   Send a single character
%% @end
%%--------------------------------------------------------------------
-spec send_char(Uart::uart(), C::byte()) ->
		       ok | {error, term()}.

send_char(Port, C) when ?is_uart(Port), ?is_uint8(C) ->
    command(Port, ?UART_CMD_SENDCHAR, [C]).

%%--------------------------------------------------------------------
%% @doc
%%   Send asynchronous data.
%% @end
%%--------------------------------------------------------------------
-spec async_send(Uart::uart(), Data::iolist()) -> ok.

async_send(Port, Data) ->
    true = erlang:port_command(Port, Data),
    ok.

%%--------------------------------------------------------------------
%% @doc
%%   Push back data onto the receive buffer.
%% @end
%%--------------------------------------------------------------------
-spec unrecv(Uart::uart(), Data::iolist()) ->
		    ok | {error,term()}.

unrecv(Port, Data) when is_list(Data); is_binary(Data)  ->
    command(Port, ?UART_CMD_UNRECV, Data).

%%--------------------------------------------------------------------
%% @doc
%%   Receive data from a device in passive mode, no timeout.
%% @end
%%--------------------------------------------------------------------    
-spec recv(Uart::uart(), Length::non_neg_integer()) ->
		  {ok,iolist()} | {error, term()}.
    
recv(Port, Length) ->
    recv_(Port, Length, -1).

%%--------------------------------------------------------------------
%% @doc
%%   Receive data from a device in passive mode.
%% @end
%%--------------------------------------------------------------------    
-spec recv(Uart::uart(), Length::non_neg_integer(), Timeout::timeout()) ->
		  {ok,iolist()} | {error, term()}.

recv(Uart, Length, infinity) ->
    recv_(Uart, Length, -1);

recv(Uart, Length, Timeout) when is_integer(Timeout) ->
    recv_(Uart, Length, Timeout).

recv_(Uart, Length, Timeout) when 
      ?is_uart(Uart),
      is_integer(Length), Length >= 0 ->
    case async_recv(Uart, Length, Timeout) of
	{ok, Ref} ->
	    receive
		{Ref, Result} ->
		    Result;
		{uart_async, Uart, Ref, Data} when is_list(Data) ->
		    {ok,Data};
		{uart_async, Uart, Ref, Data} when is_binary(Data) ->
		    {ok,Data};
		{uart_async, Uart, Ref, Other} ->
		    Other;
		{'EXIT', Uart, _Reason} ->
		    {error, closed}
	    end;
	Error -> 
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc
%%   Initiate an async receive operation.
%% @end
%%--------------------------------------------------------------------

-spec async_recv(Uart::uart(), Length::non_neg_integer()) ->
			{ok,integer()} | {error,term()}.

async_recv(Uart, Length) ->
    async_recv(Uart, Length, -1).

%%--------------------------------------------------------------------
%% @doc
%%   Initiate an async receive operation.
%%   To initiate an async operation reading a certain length and with
%%   a timeout the async_recv can be useful.
%%   ```{ok,Ref} = uart:async_recv(Uart, 16, 1000),
%%      receive 
%%        {Ref,{error,Reason}} -> {error,Reason}
%%        {uart_async,Uart,Ref,{ok,Data}} -> {ok,Data};
%%        {uart_async,Uart,Ref,{error,Reason}} -> {error,Reason};
%%        {'EXIT',Uart,_Reason} -> {error,closed}
%%        ...
%%      end'''
%%   The above can also be achived by using active once and 
%%   a fixed packet mode.
%%   ```uart:setopts(Uart, [{packet,{size,16}},{active,once}]),
%%      receive
%%         {uart,Uart,Data} -> {ok,Data};
%%         {uart_error,Uart,enxio} -> {error,usb_device_pulled_out};
%%         {uart_error,Uart,Err} -> {error,Err};
%%         {uart_closed,Uart} -> {error,close}
%%      after Timeout ->
%%         {error,timeout}
%%      end'''
%%   Packet size are however limited (to 16 bits), so any size
%%   above 64K must be handled with async_recv or split into
%%   chunks.
%% @end
%%--------------------------------------------------------------------    
    
-spec async_recv(Uart::uart(), 
		 Length::non_neg_integer(), 
		 Timeout::timeout() | -1) ->
			{ok,integer()} | {error,term()}.

async_recv(Uart, Length, Time) ->
    command_(Uart, ?UART_CMD_RECV, [<<Time:32,Length:32>>]).

%%
%% set controlling process of uart to the caller
%%
-spec controlling_process(Uart::uart(), NewOwner::pid()) ->
				 ok | {error,term()}.

%%--------------------------------------------------------------------
%% @doc
%%   Connect uart to a new controlling process, that is the process that
%%   is the event data receiver. The caller must be the current owner of
%%   the uart.
%% @end
%%--------------------------------------------------------------------

%% Set controlling process for uart.
controlling_process(U, NewOwner) when ?is_uart(U), is_pid(NewOwner) ->
    case erlang:port_info(U, connected) of
	{connected, NewOwner} ->
	    ok;
	{connected, Pid} when Pid =/= self() ->
	    {error, not_owner};
	undefined ->
	    {error, einval};
	_ ->
	    case getopt(U, active) of
		{ok, A0} ->
		    setopt(U, active, false),
		    case sync_input(U, NewOwner, false) of
			true ->  %% uart already closed, 
			    ok;
			false ->
			    try erlang:port_connect(U, NewOwner) of
				true ->
				    unlink(U), %% unlink from port
				    %% make thread reconnect to port 
				    ok = command(U, ?UART_CMD_CONNECT, []),
				    setopt(U, active, A0),
				    ok
			    catch
				error:Reason -> 
				    {error, Reason}
			    end
		    end;
		Error ->
		    Error
	    end
    end.

sync_input(U, Owner, Flag) ->
    receive
	{uart, U, Data} ->
	    Owner ! {uart, U, Data},
	    sync_input(U, Owner, Flag);
	{uart_closed, U} ->
	    Owner ! {uart_closed, U},
	    sync_input(U, Owner, true);
	{U, {data, Data}} ->
	    Owner ! {U, {data, Data}},
	    sync_input(U, Owner, Flag);	    
	{uart_async, U, Ref, Data} ->
	    Owner ! {uart_async, U, Ref, Data},
	    sync_input(U, Owner, Flag)
    after 0 -> 
	    Flag
    end.
%%--------------------------------------------------------------------
%% Internal functions    
%%--------------------------------------------------------------------
%% @private
command(Uart, Cmd, Args) ->
    case command_(Uart,Cmd,Args) of
	{ok,Ref} ->
	    receive
		{Ref, Result} ->
		    Result
	    end;
	Error -> Error
    end.

command_(Uart, Cmd, Args) ->
    case erlang:port_control(Uart, Cmd, Args) of
	<<?UART_OK,Ref:32>> ->
	    {ok, Ref};
	<<?UART_ERROR>> ->
	    {error, unknown};
	<<?UART_ERROR,Reason/binary>> ->
	    {error, binary_to_atom(Reason,latin1)}
    end.

%% @private
validate_opts([{K,V}|Kvs]) ->
    case validate_opt(K,V) of
	true -> validate_opts(Kvs);
	false -> {error,{type_error,K,V}};
	undefined -> {error,{unknown_opt,K}};
	Error -> Error
    end;
validate_opts([]) ->
    ok.

%% @private
validate_opt(device,Arg) -> is_string(Arg);
validate_opt(ibaud,Arg) -> is_baudrate(Arg);
validate_opt(obaud,Arg) -> is_baudrate(Arg);
validate_opt(baud,Arg)  -> is_baudrate(Arg);
validate_opt(csize,Arg) -> lists:member(Arg,[5,6,7,8]);
validate_opt(stopb,Arg) -> lists:member(Arg,[1,2,3]);
validate_opt(parity,Arg) -> lists:member(Arg,[none,odd,even,mark,space]);
validate_opt(iflow,Arg) -> (Arg -- [sw,rts,dtr]) =:= [];
validate_opt(oflow,Arg) -> (Arg -- [sw,cts,dsr,cd]) =:= [];
validate_opt(xonchar, Arg) -> is_uint8(Arg);
validate_opt(xoffchar, Arg) -> is_uint8(Arg);
validate_opt(eolchar, Arg) -> is_uint8(Arg);
validate_opt(active, Arg) -> lists:member(Arg,[true,false,once]);
validate_opt(delay_send,Arg) -> is_boolean(Arg);
validate_opt(header, Arg) -> is_integer(Arg) andalso (Arg >= 0);
validate_opt(packet, {size,Sz}) -> is_uint16(Sz);
validate_opt(packet, line) -> true;
validate_opt(packet, basic_0710) -> true;
validate_opt(packet, advanced_0710) -> true;
validate_opt(packet, gsm_0710) -> true;
validate_opt(packet, Arg) -> lists:member(Arg,lists:seq(-8,8));
validate_opt(packet_size, Arg) -> is_uint32(Arg);
validate_opt(deliver, Arg) -> lists:member(Arg,[port,term]);
validate_opt(mode,Arg) -> lists:member(Arg,[list,binary]);
validate_opt(buffer,Arg) -> is_uint32(Arg);
validate_opt(exit_on_close, Arg) -> is_boolean(Arg);
validate_opt(ptypkt, Arg) -> is_boolean(Arg);
validate_opt(bufsz, Arg) -> (Arg >= 0 andalso Arg =< 255);
validate_opt(buftm, Arg) -> (Arg >= 0 andalso Arg =< 25500);
validate_opt(debug, Arg) ->
    lists:member(Arg,[debug,info,notice,warning,
		      error,critical,alert,emergency,none]);
validate_opt(_,_Arg) -> undefined.

is_baudrate(Rate) ->
    %% only check some limit reset is implementation specific
    is_integer(Rate) andalso (Rate > 0).

is_string([C|Cs]) when ?is_uint8(C) ->
    is_string(Cs);
is_string([]) -> true;
is_string(_) -> false.

is_uint8(C) -> ?is_uint8(C).
is_uint16(C) -> ?is_uint16(C).
is_uint32(C) -> ?is_uint32(C).

%% @private
translate_set_opts([{baud,B}|Opts]) ->
    [{ibaud,B},{obaud,B}|translate_set_opts(Opts)];
translate_set_opts([Opt={device,Name}|Opts]) ->
    case os:type() of
	{win32,_} ->
	    %% Translate device name on windows to allow for device above COM9
	    %% COM10 must be written like \\.\COM10 => "\\\\.\\COM10
	    %% We apply this scheme for all com ports.
	    case Name of
		"\\\\.\\"++_Name -> %% already fixed
		    [Opt|translate_set_opts(Opts)];
		_ ->
		    [{device,"\\\\.\\"++Name}|
		     translate_set_opts(Opts)]
	    end;
	_ ->
	    [Opt|translate_set_opts(Opts)]
    end;
translate_set_opts([Opt|Opts]) ->
    [Opt|translate_set_opts(Opts)];
translate_set_opts([]) ->
    [].

translate_getopts([baud|Opts]) ->
    [ibaud|translate_getopts(Opts)];
translate_getopts([Opt|Opts]) ->
    [Opt|translate_getopts(Opts)];
translate_getopts([]) ->
    [].

translate_getopts_reply([baud|Opts],[{ibaud,B}|Vs]) ->
    [{baud,B}|translate_getopts_reply(Opts,Vs)];
translate_getopts_reply([device|Opts],[V={device,Name}|Vs]) ->
    case os:type() of
	{win32,_} ->
	    case Name of
		"\\\\.\\"++Name1 ->
		    [{device,Name1}|translate_getopts_reply(Opts,Vs)];
		_ ->
		    [V|translate_getopts_reply(Opts,Vs)]
	    end;
	_ ->
	    [V|translate_getopts_reply(Opts,Vs)]
    end;
translate_getopts_reply([_Opt|Opts],[V|Vs]) ->
    [V|translate_getopts_reply(Opts,Vs)];
translate_getopts_reply([],[]) ->
    [].

%% @private
-spec encode_opt(Option::atom(),Value::term()) -> 
			binary().

encode_opt(packet,0) -> 
    <<?UART_OPT_PACKET, ?UART_PB_RAW:32>>;
encode_opt(packet,PB) when PB>0, PB=< 8 -> 
    <<?UART_OPT_PACKET, (?UART_PB_N bor (PB bsl 8)):32>>;
encode_opt(packet,PB) when PB<0, PB >= -8 ->
    <<?UART_OPT_PACKET, (?UART_PB_N bor ?UART_PB_LITTLE_ENDIAN bor 
			     ((-PB) bsl 8)):32>>;
encode_opt(packet,{size,N}) when is_integer(N), N > 0, N =< 16#ffff ->
    <<?UART_OPT_PACKET, ((N bsl 16) + ?UART_PB_RAW):32>>;
encode_opt(packet,raw) ->
    <<?UART_OPT_PACKET, ?UART_PB_RAW:32>>;
encode_opt(packet,line) ->
    <<?UART_OPT_PACKET, ?UART_PB_LINE_LF:32>>;
encode_opt(packet,basic_0710) ->
    <<?UART_OPT_PACKET, ?UART_PB_BASIC_0710:32>>;
encode_opt(packet,advanced_0710) ->
    <<?UART_OPT_PACKET, ?UART_PB_ADVANCED_0710:32>>;
encode_opt(packet,gsm_0710) ->
    <<?UART_OPT_PACKET, ?UART_PB_GSM_0710:32>>;

encode_opt(device,Name) when is_list(Name); is_binary(Name) ->
    Bin = iolist_to_binary(Name),
    Len = byte_size(Bin),
    <<?UART_OPT_DEVICE,Len,Bin/binary>>;
encode_opt(ibaud,X) when X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_IBAUD,X:32>>;
encode_opt(obaud,X) when X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_OBAUD,X:32>>;
encode_opt(csize,X) when X >= 5, X =< 8 ->
    <<?UART_OPT_CSIZE,X:32>>;
encode_opt(bufsz,X) when X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_BUFSZ,X:32>>;
encode_opt(buftm,X) when X >= 0,X =< 16#ffffffff ->
    <<?UART_OPT_BUFTM,X:32>>;
encode_opt(stopb,Value) when Value >= 1, Value =< 3 ->
    <<?UART_OPT_STOPB,Value:32>>;
encode_opt(parity,none) ->
    <<?UART_OPT_PARITY,?UART_PARITY_NONE:32>>;
encode_opt(parity,odd) ->
    <<?UART_OPT_PARITY,?UART_PARITY_ODD:32>>;
encode_opt(parity,even) ->
    <<?UART_OPT_PARITY,?UART_PARITY_EVEN:32>>;
encode_opt(parity,mark) ->
    <<?UART_OPT_PARITY,?UART_PARITY_MARK:32>>;
encode_opt(parity,space) ->
    <<?UART_OPT_PARITY,?UART_PARITY_SPACE:32>>;
encode_opt(parity,P) when P >= 0, P=< 4 ->
    <<?UART_OPT_PARITY,P:32>>;
encode_opt(oflow,Value) when is_list(Value) ->
    <<?UART_OPT_OFLOW,(encode_flags(Value)):32>>;
encode_opt(iflow,Value) when is_list(Value) ->
    <<?UART_OPT_IFLOW,(encode_flags(Value)):32>>;
encode_opt(xonchar,Value) when Value >= 0, Value =< 255 ->
    <<?UART_OPT_XONCHAR,Value:32>>;
encode_opt(xoffchar,Value) when Value >= 0, Value =< 255 ->
    <<?UART_OPT_XOFFCHAR,Value:32>>;
encode_opt(eolchar,Value) when Value >= 0, Value =< 255 ->
    <<?UART_OPT_EOLCHAR,Value:32>>;
encode_opt(active,true) ->   <<?UART_OPT_ACTIVE,?UART_ACTIVE:32>>;
encode_opt(active,false) ->   <<?UART_OPT_ACTIVE,?UART_PASSIVE:32>>;
encode_opt(active,once) ->   <<?UART_OPT_ACTIVE,?UART_ONCE:32>>;

encode_opt(delay_send,X) when is_boolean(X) ->
    <<?UART_OPT_DELAY_SEND,?bool(X):32>>;
encode_opt(header,X) when is_integer(X), X >= 0 ->
    <<?UART_OPT_HEADER, X:32>>;
encode_opt(packet_size,X) when is_integer(X), X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_PSIZE, X:32>>;    
encode_opt(deliver,port) ->
    <<?UART_OPT_DELIVER, ?UART_DELIVER_PORT:32>>;
encode_opt(deliver,term) ->
    <<?UART_OPT_DELIVER, ?UART_DELIVER_TERM:32>>;
encode_opt(mode,list) ->
    <<?UART_OPT_MODE, ?UART_MODE_LIST:32>>;
encode_opt(mode,binary) ->
    <<?UART_OPT_MODE, ?UART_MODE_BINARY:32>>;

encode_opt(packet,0) -> 
    <<?UART_OPT_PACKET, ?UART_PB_RAW:32>>;
encode_opt(packet,PB) when is_integer(PB), PB>0, PB=< 8 -> 
    <<?UART_OPT_PACKET, (?UART_PB_N bor (PB bsl 8)):32>>;
encode_opt(packet,PB) when is_integer(PB), PB<0, PB >= -8 ->
    <<?UART_OPT_PACKET, (?UART_PB_N bor ?UART_PB_LITTLE_ENDIAN bor 
			     ((-PB) bsl 8)):32>>;
encode_opt(packet,{size,N}) when is_integer(N), N > 0, N =< 16#ffff ->
    <<?UART_OPT_PACKET, ((N bsl 16) + ?UART_PB_RAW):32>>;
encode_opt(packet,raw) ->
    <<?UART_OPT_PACKET, ?UART_PB_RAW:32>>;
encode_opt(packet,line) ->
    <<?UART_OPT_PACKET, ?UART_PB_LINE_LF:32>>;
encode_opt(packet,basic_0710) ->
    <<?UART_OPT_PACKET, ?UART_PB_BASIC_0710:32>>;
encode_opt(packet,advanced_0710) ->
    <<?UART_OPT_PACKET, ?UART_PB_ADVANCED_0710:32>>;
encode_opt(packet,gsm_0710) ->
    <<?UART_OPT_PACKET, ?UART_PB_GSM_0710:32>>;
encode_opt(high_watermark,X) when is_integer(X), X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_HIGH, X:32>>;    
encode_opt(low_watermark,X) when is_integer(X), X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_LOW, X:32>>;
encode_opt(send_timeout,X) when is_integer(X),X >= -1, X =< 16#7fffffff ->
    <<?UART_OPT_SENDTMO, X:32>>;
encode_opt(send_timeout_close,X) when is_integer(X),X >= -1, X =< 16#7fffffff ->
    <<?UART_OPT_CLOSETMO, X:32>>;
encode_opt(buffer, X) when is_integer(X), X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_BUFFER, X:32>>;    
encode_opt(exit_on_close, X) when is_boolean(X) ->
    <<?UART_OPT_EXITF,?bool(X):32>>;
encode_opt(ptypkt, X) when is_boolean(X) ->
    <<?UART_OPT_PTYPKT,?bool(X):32>>;
encode_opt(debug, X) ->
    case X of 
	debug      -> <<?UART_OPT_DEBUG, 7:32>>;
	info       -> <<?UART_OPT_DEBUG, 6:32>>;
	notice     -> <<?UART_OPT_DEBUG, 5:32>>;
	warning    -> <<?UART_OPT_DEBUG, 4:32>>;
	error      -> <<?UART_OPT_DEBUG, 3:32>>;
	critical   -> <<?UART_OPT_DEBUG, 2:32>>;
	alert      -> <<?UART_OPT_DEBUG, 1:32>>;
	emergency -> <<?UART_OPT_DEBUG,  0:32>>;
	none      -> <<?UART_OPT_DEBUG, -1:32>>
    end.


encode_opt(device) -> ?UART_OPT_DEVICE;
encode_opt(ibaud)  -> ?UART_OPT_IBAUD;
encode_opt(obaud)  -> ?UART_OPT_OBAUD;
encode_opt(csize)  -> ?UART_OPT_CSIZE;
encode_opt(bufsz) -> ?UART_OPT_BUFSZ;
encode_opt(buftm) -> ?UART_OPT_BUFTM;
encode_opt(stopb) -> ?UART_OPT_STOPB;
encode_opt(parity) -> ?UART_OPT_PARITY;
encode_opt(iflow) -> ?UART_OPT_IFLOW;
encode_opt(oflow) -> ?UART_OPT_OFLOW;
encode_opt(xonchar) -> ?UART_OPT_XONCHAR;
encode_opt(xoffchar) -> ?UART_OPT_XOFFCHAR;
encode_opt(eolchar) ->  ?UART_OPT_EOLCHAR;
encode_opt(active) -> ?UART_OPT_ACTIVE;
encode_opt(delay_send) -> ?UART_OPT_DELAY_SEND;
encode_opt(header)     -> ?UART_OPT_HEADER;
encode_opt(packet) ->  ?UART_OPT_PACKET;
encode_opt(packet_size) ->  ?UART_OPT_PSIZE;
encode_opt(deliver)     ->  ?UART_OPT_DELIVER;
encode_opt(mode)     ->  ?UART_OPT_MODE;
encode_opt(high_watermark) -> ?UART_OPT_HIGH;
encode_opt(low_watermark) -> ?UART_OPT_LOW;
encode_opt(send_timeout) -> ?UART_OPT_SENDTMO;
encode_opt(send_timeout_close) -> ?UART_OPT_CLOSETMO;
encode_opt(buffer) -> ?UART_OPT_BUFFER;
encode_opt(exit_on_close) -> ?UART_OPT_EXITF;
encode_opt(debug) -> ?UART_OPT_DEBUG;
encode_opt(ptypkt) -> ?UART_OPT_PTYPKT.
    
encode_flags([F|Fs]) ->
    encode_flag(F) + encode_flags(Fs);
encode_flags([]) ->
    0.

encode_flag(dtr) -> ?UART_DTR;
encode_flag(rts) -> ?UART_RTS;
encode_flag(cts) -> ?UART_CTS;
encode_flag(cd)  -> ?UART_CD;
encode_flag(ri)  -> ?UART_RI;
encode_flag(dsr) -> ?UART_DSR;
encode_flag(sw) ->  ?UART_SW.

%% can be replaced with dloader later
load_driver(Path, Name) ->
    Ext = filename:extension(Name),
    Base = filename:basename(Name,Ext),
    NameExt = case os:type() of
		  {unix,_} ->  Base++".so";
		  {win32,_} -> Base++".dll"
	      end,
    SysPath = filename:join(Path,erlang:system_info(system_architecture)),
    case filelib:is_regular(filename:join(SysPath,NameExt)) of
	true -> erl_ddll:load(SysPath, Name);
	false ->
	    case filelib:is_regular(filename:join(Path,NameExt)) of
		true -> erl_ddll:load(Path, Name);
		false -> {error, enoent}
	    end
    end.
