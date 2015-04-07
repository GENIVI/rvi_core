# Exosense Device - Jaguar Land Rover Tizen IVI HVAC control demo

# Building the Exosense JLR demo.

Follow the instructions under:

    `https://github.com/Feuerlabs/exosense_specs/blob/master/doc/exosense_demo_tutorial.pdf`

Replace meta-exodemo with meta-jlrdemo (the Yocto build layerfor this
demo). The meta-sbc6845 layer, mentioned in the tutorial, will not be
needed.

## Setting up configuration files
After setting up the basic environment, as described in the tutorial,
init the build environment with this command:

    `. oe-init-build-env ../build`

Copy `meta-jlrdemo/build_conf/*.conf` into the `conf` subdirectory of
the build directory you are currently in.

	cp ../meta-jlrdemo/build_conf/*.conf conf

## Build the RPM 
Since the demo is installed on Tizen, we will not create a complete
image, but rather a set of RPMs that can be installed on the standard
demo.

Build the RPMs with:

	
    bitbake erlang-jlrdemo

The rpms will be deposited in 

    build/tmp/deploy/rpm/i586

# Installing the Exosense JLR demo RPMs on the target system

All rpms to be copied over from the directory above to the target system are listed in

    https://github.com/Feuerlabs/jlrdemo/blob/master/tizen_rpm_list.txt

Once copied, install them all using a single `rpm -i` command


# Setting up automatic launch during boot

There is a start script for the Exosense JLR demo installed on the target under:

    /usr/lib/erlang/jlrdemo-???/priv/jlrdemo_ctl.sh

This script also installs the pcan driver kernel module (unless already loaded).

Copy this script to /usr/sbin

    /usr/lib/erlang/jlrdemo-*/priv/jlrdemo_ctl.sh /usr/sbin
	
Edit the uxlaunch systemd service in file:

     /etc/systemd/system/display-manager.service
	 
Edit the ExecStart= line so that it looks like this:

    ExecStart=/bin/sh /usr/sbin/tizenctl.sh start

**Note** The /usr/sbin/tizenctl.sh, which will start the dashboard UI,
  and the `/usr/sbin/jlrdemo.sh` script is not provided by the
  Exosense RPMs. Please see the Tizen documentation for details on
  where to source this.

# Upgrading the Exosense JLR demo

Be sure to remove the old package using `rpm -e` before installing the new version.
Also be sure to execute the following command to wipe any old setup data.

    rm -rf /root/setup



