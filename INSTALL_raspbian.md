Copyright (C) 2014-2016, Jaguar Land Rover

This document is licensed under Creative Commons
Attribution-ShareAlike 4.0 International.

**Version 0.5.0**

# INSTALLATION OF RVI (RASPBIAN) #

This document describes the installation process for the RVI project 
on Raspbian. Packages are also available for 
[Ubuntu](INSTALL_ubuntu.md) and [Raspbian](INSTALL_raspbian.md). 
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

1. The Raspbian system has the latest updates installed.
2. The user has sudo access to install packages.
3. There is at least 5GB of space availabled for packages and code.

----

# INSTALLATION PROCESS #

## GET RASPBIAN PACKAGES ##

Download Erlang and RVI for Raspbian:

    wget (TODO)/erlang-18.2.tar.bz2
    wget (TODO)/rvi_0.5.0-1_armhf.deb

Also download the install script:

    wget (TODO)/installRVI

## INSTALL ERLANG AND RVI ##

Make the install script executable:

    sudo chmod +x installRVI

Then run the install script:

    ./installRVI

This does the following:

1. Extracts the Erlang tarball
2. Installs the required Erlang packages
  * `erlang-base`
  * `erlang-asnl`
  * `erlang-crypto`
  * `erlang-eunit`
  * `erlang-public key`
  * `erlang-ssl`
  * `erlang-syntax-tools`
3. Installs other dependencies (`bluez` and `python-jsonrpclib`)
4. Installs `rvi`
5. Starts `rvi` via `systemctl`
6. Checks the status of `rvi` via `systemctl`.

----

## TEST THE RVI SYSTEM ##

To confirm that RVI has started, use `systemctl` as root user:

    systemctl status rvi

Expected output:

    ● rvi.service - Remote Vehicle Interaction Service
       Loaded: loaded (/lib/systemd/system/rvi.service; disabled)
       Active: active (running) since Tue 2016-02-23 18:30:03 UTC; 48ms ago
      Process: 6049 ExecStart=/usr/bin/rvi_ctl -c /etc/rvi/rvi.config start (code=exited, status=0/SUCCESS)
     Main PID: 6081 (run_erl)
       CGroup: /system.slice/rvi.service
               ├─6081 run_erl -daemon /tmp/rvi_6049/ /var/log/rvi exec erl -boot /tmp/rvi_6049/rvi/start -sname rvi -config /tmp/rvi_6049/rvi/sys -setcookie rvi_cookie
               ├─6084 /usr/lib/erlang/erts-7.2/bin/erlexec -boot /tmp/rvi_6049/rvi/start -sname rvi -config /tmp/rvi_6049/rvi/sys -setcookie rvi_cookie
           ├─6093 sh -c "/usr/lib/erlang/erts-7.2/bin/epmd" -daemon
               └─6094 /usr/lib/erlang/erts-7.2/bin/epmd -daemon


## CREATE A RELEASE ##

The installer configures a release with default (insecure) credentials.

See ```CONFIGURE.md``` for additional details on configuring and
creating a developer and production release.

