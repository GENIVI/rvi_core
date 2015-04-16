

#The uart application#



### Interface description ###


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/tonyrog/uart/blob/master/doc/uart.md" class="module">uart</a></td></tr></table>

### Dependencies ###

To build uart you will need a working installation of Erlang R15B (or
later).<br/>
Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

uart is built using rebar that can be found [here](https://github.com/rebar/rebar), with building instructions [here](https://github.com/rebar/rebar/wiki/Building-rebar). rebar's dynamic configuration mechanism, described [here](https://github.com/rebar/rebar/wiki/Dynamic-configuration), is used so the environment variable `REBAR_DEPS` should be set to the directory where your erlang applications are located.

uart also requires the following applications to be installed:
<ul>
<li>dthread - https://github.com/tonyrog/dthread</li>
</ul>

If you use ftdi devices you can find drivers at http://ftdichip.com/.

### Downloading

Clone the repository in a suitable location:

```sh
$ git clone git://github.com/tonyrog/uart.git
```
### Configurating
#### Concepts

...

#### Files

...

### Building

Compile:

```sh
$ cd uart
$ rebar compile
...
==> uart (compile)
```

### Testing

#### Without hardware 

Basic test of uart can be done without having any dongles:

```sh
$ cd uart
$ rebar ct
...
==> uart (ct)
```
#### With hardware 

If you want to test with hardware you must configure the appropriate devices in [uart.cfg](http://github.com/tonyrog/uart/blob/master/test/uart.cfg) and then run:
```sh
$ cd uart/test
$ ct_run  -spec uart_hw.spec
...

```



