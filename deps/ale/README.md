ale
=====

ale, a [lager](https://github.com/basho/lager) extension, makes it possible for several processes to trace the same modules.


### Dependencies

To build ale you will need a working installation of Erlang R15B (or
later).<br/>
Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

ale is built using rebar that can be found [here](https://github.com/rebar/rebar), with building instructions [here](https://github.com/rebar/rebar/wiki/Building-rebar). rebar's dynamic configuration mechanism, described [here](https://github.com/rebar/rebar/wiki/Dynamic-configuration), is used so the environment variable `REBAR_DEPS` should be set to the directory where your erlang applications are located.

ale also requires [lager](https://github.com/basho/lager) to be installed.

### Downloading

Clone the repository in a suitable location:

```sh
$ git clone git://github.com/tonyrog/ale.git
```
### Functionality
#### Concepts

ale extends lager by using a server that keeps track of all trace request thus making it possible for several processes to add the same traces while only adding one to lager and likewise not removing it from lager until all processes have removed it.<br/>
Available api is:
<ul>
<li> trace(on | off, ModuleOrPidOrFilter::atom() | pid() | tuple() | list(tuple)), Loglevel::atom()) - prints trace output on console as long as calling process hasn't terminated.</li>
<li> trace(on | off, ModuleOrPidOrFilter::atom() | pid() | tuple() | list(tuple)), Loglevel::atom(), File::string()) - prints trace output to File as long as calling process hasn't terminated. File must exist.</li>
<li> trace_gl(on | off, ModuleOrPidOrFilter::atom() | pid() | tuple() | list(tuple)), Loglevel::atom()) - prints trace output on console as long as calling process' group leader hasn't terminated. Suitable for calls from a shell.</li>
<li> trace_gl(on | off, ModuleOrPidOrFilter::atom() | pid() | tuple() | list(tuple)), Loglevel::atom(), File::string()) - prints trace output to File as long as calling process' group leader hasn't terminated. Suitable for calls from a shell. File must exist.</li>
</ul>
Filter is a tuple {tag, Tag} that lager uses to determine what to output. <br/>
LogLevel is  debug | info | notice | warning | error | critical | alert |  emergency. <br/>
See lager documentations for more details.<br/>
Examples:<br/>
<code>
ale:trace(on, sz_master, debug).<br/>
ale:trace(on, self(), debug).<br/>
ale:trace_gl(on, sz_node, info, "/tmp/ale.log").<br/>
ale:trace(off, sz_master, debug)<br/>
ale:trace(on, [{module, ale}, {function, start}], debug).<br/>
</code>

There are also some shortcut functions:
<ul>
<li> debug(ModuleOrPidOrList::atom() | pid() | tuple() | list(atom())) - calls trace(on, ModuleOrPidOrList, debug).</li>
<li> info(ModuleOrPidOrList::atom() | pid() | tuple() | list(atom())) - calls trace(on, ModuleOrPidOrList, info).</li>
<li> error(ModuleOrPidOrList::atom() | pid() | tuple() | list(atom())) - calls trace(on, ModuleOrPidOrList, error).</li>
<li> warning(ModuleOrPidOrList::atom() | pid() | tuple() | list(atom())) - calls trace(on, ModuleOrPidOrList, warning).</li>
</ul><br/>
These can be called with lists of modules.

#### Config Files

Arguments to all applicable erlang applications are specified in an erlang configuration file.<br/>
Traces can be added to the ale part of the configuration file. These traces will be active as long as ale i running.<br/>
Example:<br/>
<code>
	{init_traces, [<br/>
		       {[{module, sz_master}], debug}, <br/>
		       {[{module, sz_node}], info, "/tmp/ale.log"}<br/>
		      ]}<br/>
</code>

An example can be found in ["sys.config"](https://github.com/tonyrog/ale/raw/master/sys.config).<br/>


#### Tips

If you start tracing using the non-groupleader function calls from the shell you can stop it by 'crashing' the shell, for ex with a=b. Might be handy if you get more output than expected ;-)

### Building

Rebar will compile all needed dependencies.<br/>
Compile:

```sh
$ cd ale
$ rebar compile
...
==> ale (compile)
```

### Running

There is a quick way to run the application for testing:

```sh
$ erl -sname ale -config sys -pa <path>/ale/ebin
>ale:start().
```
(Instead of specifing the path to the ebin directory you can set the environment ERL_LIBS.)

Stop:

```sh
>halt().
```

### Release

To generate a proper release follow the instructions in 
 [Release Handling](https://github.com/basho/rebar/wiki/Release-handling) or look in the [Rebar tutorial](http://www.metabrew.com/article/erlang-rebar-tutorial-generating-releases-upgrades).

<b>Before</b> the last step you have to update the file "ale/rel/files/sys.config" with your own settings.
You probably also have to update "ale/rel/reltool.config" with the correct path to your application (normally "{lib_dirs, ["../.."]}") and all apps you need.
```
       {app, sasl,   [{incl_cond, include}]},
       {app, stdlib, [{incl_cond, include}]},
       {app, kernel, [{incl_cond, include}]},
       {app, lager, [{incl_cond, include}]}
       {app, ale, [{incl_cond, include}]}
```


And then you run: 
```
$ rebar generate
```

Before generating a new version of a release the old version should be (re)moved.

Start node:

```sh
$ cd rel
$ ale/bin/ale start
```

(If you want to have access to the erlang node use 
``` 
console 
```
instead of 
``` 
start
```
.)

### Documentation

ale is documented using edoc. To generate the documentation do:

```sh
$ cd ale
$ rebar doc
```
You get the resulting html-files in ale/doc subdirectory.
