

#Module uart#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



Cross platform tty interface.

Copyright (c) (C) 2012, Tony Rogvall

__Authors:__ Tony Rogvall ([`tony@rogvall.se`](mailto:tony@rogvall.se)).
<a name="types"></a>

##Data Types##




###<a name="type-uart">uart()</a>##



<pre>uart() = port()</pre>



###<a name="type-uart_input_pins">uart_input_pins()</a>##



<pre>uart_input_pins() = cts | cd | ri | dcr</pre>



###<a name="type-uart_modem_pins">uart_modem_pins()</a>##



<pre>uart_modem_pins() = <a href="#type-uart_input_pins">uart_input_pins()</a> | <a href="#type-uart_output_pins">uart_output_pins()</a></pre>



###<a name="type-uart_option">uart_option()</a>##



<pre>uart_option() = device | baud | ibaud | obaud | csize | bufsz | buftm | stopb | parity | iflow | oflow | xonchar | xoffchar | eolchar | active | delay_send | header | packet | packet_size | deliver | mode | buffer | exit_on_close</pre>



###<a name="type-uart_output_pins">uart_output_pins()</a>##



<pre>uart_output_pins() = dtr | rts</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_recv-2">async_recv/2</a></td><td>
Initiate an async receive operation.</td></tr><tr><td valign="top"><a href="#async_recv-3">async_recv/3</a></td><td>
Initiate an async receive operation.</td></tr><tr><td valign="top"><a href="#async_send-2">async_send/2</a></td><td>
Send asynchronous data.</td></tr><tr><td valign="top"><a href="#break-2">break/2</a></td><td>
Send break for Duration number of milliseconds .</td></tr><tr><td valign="top"><a href="#clear_modem-2">clear_modem/2</a></td><td>
Clear modem pins.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close a tty device.</td></tr><tr><td valign="top"><a href="#controlling_process-2">controlling_process/2</a></td><td>
Connect uart to a new controlling process, that is the process that
is the event data receiver.</td></tr><tr><td valign="top"><a href="#flow-2">flow/2</a></td><td>
Manage input and output flow control.</td></tr><tr><td valign="top"><a href="#get_modem-1">get_modem/1</a></td><td>
Get modem pins status.</td></tr><tr><td valign="top"><a href="#getopt-2">getopt/2</a></td><td>Get single option value.</td></tr><tr><td valign="top"><a href="#getopts-2">getopts/2</a></td><td>Get multiple option values.</td></tr><tr><td valign="top"><a href="#hangup-1">hangup/1</a></td><td>
Hangup.</td></tr><tr><td valign="top"><a href="#i-0">i/0</a></td><td>List information about open uart ports.</td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>Opens a tty device.</td></tr><tr><td valign="top"><a href="#options-0">options/0</a></td><td>
This function is for documentations purpose.</td></tr><tr><td valign="top"><a href="#recv-2">recv/2</a></td><td>
Receive data from a device in passive mode, no timeout.</td></tr><tr><td valign="top"><a href="#recv-3">recv/3</a></td><td>
Receive data from a device in passive mode.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>
Send characters.</td></tr><tr><td valign="top"><a href="#send_char-2">send_char/2</a></td><td>
Send a single character.</td></tr><tr><td valign="top"><a href="#set_modem-2">set_modem/2</a></td><td>
Set modem pins.</td></tr><tr><td valign="top"><a href="#setopt-3">setopt/3</a></td><td>
Set single option.</td></tr><tr><td valign="top"><a href="#setopts-2">setopts/2</a></td><td>
Set multiple options.</td></tr><tr><td valign="top"><a href="#unrecv-2">unrecv/2</a></td><td>
Push back data onto the receive buffer.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="async_recv-2"></a>

###async_recv/2##


<pre>async_recv(Uart::<a href="#type-uart">uart()</a>, Length::non_neg_integer()) -> {ok, integer()} | {error, term()}</pre>
<br></br>



Initiate an async receive operation.<a name="async_recv-3"></a>

###async_recv/3##


<pre>async_recv(Uart::<a href="#type-uart">uart()</a>, Length::non_neg_integer(), Timeout::timeout() | -1) -> {ok, integer()} | {error, term()}</pre>
<br></br>



Initiate an async receive operation.
To initiate an async operation reading a certain length and with
a timeout the async_recv can be useful.
<pre>       {ok,Ref} = uart:async_recv(Uart, 16, 1000),
       receive
         {Ref,{error,Reason}} -> {error,Reason}
         {uart_async,Uart,Ref,{ok,Data}} -> {ok,Data};
         {uart_async,Uart,Ref,{error,Reason}} -> {error,Reason};
         {'EXIT',Uart,_Reason} -> {error,closed}
         ...
       end</pre>
The above can also be achived by using active once and
a fixed packet mode.
<pre>       uart:setopts(Uart, [{packet,{size,16}},{active,once}]),
       receive
          {uart,Uart,Data} -> {ok,Data};
          {uart_error,Uart,enxio} -> {error,usb_device_pulled_out};
          {uart_error,Uart,Err} -> {error,Err};
          {uart_closed,Uart} -> {error,close}
       after Timeout ->
          {error,timeout}
       end</pre>
Packet size are however limited (to 16 bits), so any size
above 64K must be handled with async_recv or split into
chunks.<a name="async_send-2"></a>

###async_send/2##


<pre>async_send(Uart::<a href="#type-uart">uart()</a>, Data::iolist()) -> ok</pre>
<br></br>



Send asynchronous data.<a name="break-2"></a>

###break/2##


<pre>break(Uart::<a href="#type-uart">uart()</a>, Duration::non_neg_integer()) -> ok | {error, term()}</pre>
<br></br>



Send break for Duration number of milliseconds .<a name="clear_modem-2"></a>

###clear_modem/2##


<pre>clear_modem(Uart::<a href="#type-uart">uart()</a>, Flags::[<a href="#type-uart_modem_pins">uart_modem_pins()</a>]) -> ok | {error, term()}</pre>
<br></br>



Clear modem pins.<a name="close-1"></a>

###close/1##


<pre>close(Uart::<a href="#type-uart">uart()</a>) -> true</pre>
<br></br>


Close a tty device.<a name="controlling_process-2"></a>

###controlling_process/2##


<pre>controlling_process(Uart::<a href="#type-uart">uart()</a>, NewOwner::pid()) -> ok | {error, term()}</pre>
<br></br>



Connect uart to a new controlling process, that is the process that
is the event data receiver. The caller must be the current owner of
the uart.<a name="flow-2"></a>

###flow/2##


<pre>flow(Uart::<a href="#type-uart">uart()</a>, Mode::(input_off | input_on | output_off | output_on)) -> ok | {error, term()}</pre>
<br></br>



Manage input and output flow control<a name="get_modem-1"></a>

###get_modem/1##


<pre>get_modem(Uart::<a href="#type-uart">uart()</a>) -> {ok, [<a href="#type-uart_modem_pins">uart_modem_pins()</a>]} | {error, term()}</pre>
<br></br>



Get modem pins status.<a name="getopt-2"></a>

###getopt/2##


<pre>getopt(Uart::<a href="#type-uart">uart()</a>, Option::<a href="#type-uart_option">uart_option()</a>) -> {ok, term()} | {error, term()}</pre>
<br></br>


Get single option value.
See [`options/0`](#options-0) for available options.<a name="getopts-2"></a>

###getopts/2##


<pre>getopts(Uart::<a href="#type-uart">uart()</a>, Option::[<a href="#type-uart_option">uart_option()</a>]) -> {ok, [{<a href="#type-uart_option">uart_option()</a>, term()}]} | {error, term()}</pre>
<br></br>


Get multiple option values.<a name="hangup-1"></a>

###hangup/1##


<pre>hangup(Uart::<a href="#type-uart">uart()</a>) -> ok | {error, term()}</pre>
<br></br>



Hangup<a name="i-0"></a>

###i/0##


`i() -> any()`

List information about open uart ports<a name="open-2"></a>

###open/2##


<pre>open(DeviceName::iolist(), Options::[{<a href="#type-uart_option">uart_option()</a>, term()}]) -> {ok, <a href="#type-uart">uart()</a>} | {error, term()}</pre>
<br></br>




Opens a tty device.

The device name `pty` is reserved for opening a pseudo terminal
using the openpty call. The slave device is accessed through the
device option. See [`options/0`](#options-0) for a description of
available options.<a name="options-0"></a>

###options/0##


<pre>options() -&gt; atom()</pre>
<br></br>





This function is for documentations purpose.

List of available options:

* `{device, "pty" | string()}`

* `{ibaud, baudrate()}`

* `{obaud, baudrate()}`

* `{baud, baudrate()}`

* `{csize, 5|6|7|8}`

* `{stopb, 1|2|3}`

* `{parity,none|odd|even|mark|space}`

* `{iflow, [sw|rts|dtr]}`

* `{oflow, [sw|cts|dsr|cd]}`

* `{xonchar, byte()}`

* `{xoffchar, byte()}`

* `{eolchar, byte()}`

* `{active, true | false | once}`

* `{delay_send, boolean()}`

* `{header, size()}`

* `{packet, packet_type()}` 
<br></br>

`packet_type() ::= -8..-1|0|1..8,line,{size,0..65535}`

* `{packet_size, integer()}`

* `{deliver, port | term}`

* `{mode,    list | binary}`

* `{buffer,  integer()}`

* `{exit_on_close, boolean()}`

* `{bufsz, 0..255}` - Max low level uart buffer size

* `{buftm, 0..25500}` - Inter character timeout

<a name="recv-2"></a>

###recv/2##


<pre>recv(Uart::<a href="#type-uart">uart()</a>, Length::non_neg_integer()) -> {ok, iolist()} | {error, term()}</pre>
<br></br>



Receive data from a device in passive mode, no timeout.<a name="recv-3"></a>

###recv/3##


<pre>recv(Uart::<a href="#type-uart">uart()</a>, Length::non_neg_integer(), Timeout::timeout()) -> {ok, iolist()} | {error, term()}</pre>
<br></br>



Receive data from a device in passive mode.<a name="send-2"></a>

###send/2##


<pre>send(Uart::<a href="#type-uart">uart()</a>, Data::iolist()) -> ok | {error, term()}</pre>
<br></br>



Send characters<a name="send_char-2"></a>

###send_char/2##


<pre>send_char(Uart::<a href="#type-uart">uart()</a>, C::byte()) -> ok | {error, term()}</pre>
<br></br>



Send a single character<a name="set_modem-2"></a>

###set_modem/2##


<pre>set_modem(Uart::<a href="#type-uart">uart()</a>, Flags::[<a href="#type-uart_modem_pins">uart_modem_pins()</a>]) -> ok | {error, term()}</pre>
<br></br>



Set modem pins.<a name="setopt-3"></a>

###setopt/3##


<pre>setopt(Uart::<a href="#type-uart">uart()</a>, Option::<a href="#type-uart_option">uart_option()</a>, Value::term()) -> ok | {error, term()}</pre>
<br></br>





Set single option.

See [`options/0`](#options-0) for available options.<a name="setopts-2"></a>

###setopts/2##


<pre>setopts(Uart::<a href="#type-uart">uart()</a>, Options::[{<a href="#type-uart_option">uart_option()</a>, term()}]) -> ok | {error, term()}</pre>
<br></br>



Set multiple options.<a name="unrecv-2"></a>

###unrecv/2##


<pre>unrecv(Uart::<a href="#type-uart">uart()</a>, Data::iolist()) -> ok | {error, term()}</pre>
<br></br>



Push back data onto the receive buffer.