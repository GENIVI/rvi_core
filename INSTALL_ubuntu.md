Copyright (C) 2014-2016, Jaguar Land Rover

This document is licensed under Creative Commons
Attribution-ShareAlike 4.0 International.

**Version 0.5.0**

# INSTALLATION OF RVI (UBUNTU 14.04 TRUSTY) #

This document describes the installation process for the RVI project on 
an Ubuntu 14.04 Linux machine. Packages are also available for 
[Debian](BUILD_debian.md) and [Raspbian](BUILD_raspbian.md). 
See [```BUILD.md```](BUILD.md) for building from source.

Please see [```README.md```](README.md) for a general description of the project
and its structure.

Please see [```CONFIGURE.md```](CONFIGURE.md) for details on configuring and launching
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

1. The Ubuntu 14.04 system have the latest updates installed.
2. The user can gain root access to install packages.
3. There is at least 5GB of space availabled for packages and code.

----

<div class="pagebreak"></div>

# INSTALLATION PROCESS #

## INSTALL DEPENDENCIES ##

Install dependent libraries via `apt-get`:

    sudo apt-get install python-jsonrpclib

## INSTALL ESL-ERLANG ##

Install `esl-erlang` 18.2, or a later version 18 release:

Tested packages of the latest versions of Erlang can be downloaded from 
[packages.erlang-solutions.com](https://www.erlang-solutions.com/resources/download.html)

Add the following line to your /etc/apt/sources.list

    deb http://packages.erlang-solutions.com/ubuntu trusty contrib

Update and install esl-erlang

    sudo apt-get update
    sudo apt-get install esl-erlang

**If you receive an authentication error** (such as NO_PUBKEY): 
note the hexadecimal value (e.g., 6D975C4791E7EE5E) and request the key:

    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys HEX

where HEX is the hexadecimal value specified in the error. 
Then rerun the ```update``` and ```install``` commands.

## DOWNLOAD AND INSTALL RVI ##

Download the RVI package from the remote location:

    wget http://(TODO)/rvi_0.5.0-1ubuntu1_amd64.deb

Then install RVI via dpkg:

    sudo dpkg -i rvi_0.5.0-1ubuntu1_amd64.deb

----

## TEST THE RVI SYSTEM ##

To confirm that RVI has installed successfully, run:

    sudo service rvi status

Expected output:

    TODO

## CREATE A RELEASE ##

The installer configures a release with default (insecure) values.

See ```CONFIGURE.md``` for details on configuring and creating a
developer and production release that can be launched.

