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

usage() {
    echo "Usage: $0 -n node_name -b|-r [-p port] [-s prefix,ip:port]..."
    echo "  -n node_name          Specify the name of the rvi node to launch"
    echo "  -b                    Launch using development build (default)"
    echo "  -r                    Launch using release"
    echo 
    echo "Configuration data is read from the configuration file"
    echo "provided to the setup_rvi_node.sh script that created the node."
    exit 1
}

mode=build
while getopts ":n:brp:s:" o; do
    case "${o}" in
        n)
            node_name=${OPTARG}
            ;;
        b)
            mode=build
            ;;
        r)
            mode=release
            ;;
        *)
            usage
            ;;
    esac
done

if [ -z "${node_name}" ] ; then
    echo "Missing -n flag"
    usage
fi

# check man erl for extra arguments

if [ "${mode}" = "build" ]
then
    if [ ! -f ${node_name}/sys.config ]
    then
	echo "Node ${node_name} not setup. Please run ./setup_rvi_node.sh ${node_name} <configuration_file>"
	exit 2
    fi
    exec erl -boot ${node_name}/start -config ${node_name}/sys 

elif [ "${mode}" = "release" ]
then
    echo "Not yet supported."
    exit 0
fi
