Copyright (C) 2014-2016, Jaguar Land Rover

This document is licensed under Creative Commons
Attribution-ShareAlike 4.0 International.

**Version 0.5.0**

# INSTALLATION OF RVI (DEBIAN) #

This document describes the installation process for the RVI project 
on a Debian 8 ("jessie") Linux machine. Packages are also available 
for [Ubuntu](INSTALL_ubuntu.md) and [Raspbian](INSTALL_raspbian.md). 
See ```BUILD.md``` for building from source.

Please see ```README.md``` for a general description of the project
and its structure.

Please see ```CONFIGURE.md``` for details on configuring and launching
the system once it has been built.

The first milestone of the RVI project is the HVAC demo. Please see
```hvac_demo/README.md``` for details on how to setup, launch and
drive the demo.

# READER ASSUMPTIONS #
In order to build the system, the reader is assumed to be able to:

1. Have a basic understanding of Linux system operations.
2. Install packages on the system.

Please note that the configuration process described in
```CONFIGURE.md``` may have additional skill requirements.

# PREREQUISITES #

1. The Debian 8 system has the latest updates installed.
2. The user can gain root access to install packages.
3. There is at least 5GB of space availabled for packages and code.

----

# INSTALLATION PROCESS #

## INSTALL DEPENDENCIES ##

Install dependent libraries via ```apt-get``` - note you must be root:

    apt-get install python-jsonrpclib

## INSTALL ESL-ERLANG ##

Install `esl-erlang` 18.2, or a later version 18 release:

Tested packages of the latest versions of Erlang can be downloaded from 
[packages.erlang-solutions.com](https://www.erlang-solutions.com/resources/download.html)

Add the following line to your /etc/apt/sources.list

    deb http://packages.erlang-solutions.com/debian jessie contrib

Update and install `esl-erlang`:

    apt-get update
    apt-get install esl-erlang

**If you receive an authentication error** (such as NO_PUBKEY): note 
the hexadecimal value (e.g., 6D975C4791E7EE5E) and request the key:

    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys HEX

where HEX is the hexadecimal value specified in the error. 
Then rerun the ```update``` and ```install``` commands.

## DOWNLOAD AND INSTALL RVI ##

Download the RVI package from https://github.com/PDXostc/rvi_core/releases.

Then install RVI via dpkg:

    sudo dpkg -i rvi_0.5.0-1_amd64.deb

----

## TEST THE RVI SYSTEM ##

To confirm that RVI has started, use `systemctl` as root user:

    systemctl status rvi

Expected output:

    ● rvi.service - Remote Vehicle Interaction Service
       Loaded: loaded (/lib/systemd/system/rvi.service; disabled)
       Active: active (running) since Mon 2016-02-22 14:16:27 PST; 15min ago
      Process: 1922 ExecStart=/usr/bin/rvi_ctl -c /etc/rvi/rvi.config start (code=exited, status=0/SUCCESS)
     Main PID: 1954 (run_erl)
       CGroup: /system.slice/rvi.service
           ├─1954 run_erl -daemon /tmp/rvi_1922/ /var/log/rvi exec erl -boot /tmp/rvi_1922/rvi/start -sname rvi -config /tmp/rvi_1922/rvi/sys -setcookie rvi_cookie
           ├─1955 /usr/lib/erlang/erts-7.2/bin/beam.smp -- -root /usr/lib/erlang -progname erl -- -home /usr/lib/rvi_core -- -boot /tmp/rvi_1922/rvi/start -sname rvi -config /tmp/rvi_1922/rvi/sys -setcookie rvi_cookie
           ├─1962 /usr/lib/erlang/erts-7.2/bin/epmd -daemon
           ├─1983 /usr/lib/rvi_core/deps/bt/priv/bt
           ├─1989 inet_gethost 4
           └─1990 inet_gethost 4

## GET STARTED WITH SERVICES ##

Find out which services are registered through `rvi_get_services`:

    /usr/lib/rvi_core/rvi_get_services

The command should return no output, since we have not registered any services yet.

Register a service by calling `rvi_service` -- start with "hello":

    /usr/lib/rvi_core/rvi_service hello

Expected output:

    RVI General Service.
    RVI node URL:         http://localhost:9001
    Service:              genivi.org/vin/default_vin/hello
    Press enter to quit: 

In another terminal, call `rvi_get_services` again to see the newly registered service:

    genivi.org/vin/default_vin/hello

Invoke the service through `rvi_call` and the full service name:

    /usr/lib/rvi_core/rvi_call genivi.org/vin/default_vin/hello

In terminal 1 (where you called `rvi_service`), you should see the following output:

    Service invoked!
    args: {}

In terminal 2 (where you called `rvi_call`), you should see the following output:

    RVI Node:          http://localhost:9001
    Service:           genivi.org/vin/default_vin/hello
    args:              {}

You can pass arguments to a service call with the format name=value:

    /usr/lib/rvi_core/rvi_call genivi.org/vin/default_vin/hello \
    a=b message=hello

## CREATE A RELEASE ##

The installer configures a release with default (insecure) credentials.

See ```CONFIGURE.md``` for additional details on configuring and
creating a developer and production release.

