#!/bin/sh
#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Launch an RVI node
#
# This script launches the erlang runtime system and executes the RVI
# code directly from its build directory. It does not (yet) launch a
# separate release, but is instead intended for testing and evaluation.
#
#
# This script can be executed after a setup_node.sh has been executed to 
# create the necessary config files and erlang boot scripts.
#

# ./rvi_node.sh 

if [ "$!" -lt "2" ] 
then
    echo "Usage: $0 node_name [-port port] [-backend ip:port]"
    exit 1
fi


if [ ! -f $1/sys.config ]
then
    echo "Node $1 not setup. Please run setup_node $1 <configuration_file>"
    exit 2
fi

# check man erl for extra arguments

erl -boot $1/start -config $1/sys 
