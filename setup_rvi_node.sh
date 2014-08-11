#!/bin/sh
#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Setup an RVI release with a configuration file.
#
# This script will setup a directory with with the same name
# as the release name. The script uses Ulf Wiger's setup application 
# (github.com/Feuerlabs/setup) to generate the release.
#
#  Once setup, the RVI node can be started with ./rvi_node <release_na,e?
#
#  Please note that the generated release will depend on the built
#
#  In order to create a standalone release, use create_rvi_release.sh
#
SETUP_GEN=./deps/setup/setup_gen  # Ulf's kitchen sink setup utility

if [ "$#" != "2" ] 
then
    echo "Usage: $0 <release_name> <config_file>"
    echo "Will create a subdirectory named  <release_name> which contains"
    echo "all configuration and boot files necessary to bring up an rvi node."
    echo
    echo "Launch the node with ./rvi_node <release_name>"
    echo
    echo "See priv/sample.config for example configuration file"
    echo 
    exit 1
fi

export ERL_LIBS=$PWD/deps:$ERL_LIBS:$PWD 

$SETUP_GEN $1 $2 $1

echo "RVI Node $1 has been setup. Launch with ./rvi_node $1"
exit 0
