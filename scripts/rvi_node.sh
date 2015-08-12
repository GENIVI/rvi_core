#!/bin/sh
#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Launch an RVI node using the build direcotriuies.
#
# This script launches the erlang runtime system and executes the RVI
# code directly from its build directory. Use ./setup_rvi_node without
# the -d flag to create an installable release that can execute
# without the use of this script.
#
# This script can be executed after a setup_node.sh has been executed to 
# create the necessary config files and erlang boot scripts.
#
alias realpath="python -c 'import os, sys; print os.path.realpath(sys.argv[1])'"
SELF_DIR=$(dirname $(realpath "$0"))

usage() {
    echo "Usage: $0 [-d] -n node_name"
    echo "  -n node_name          Specify the name of the rvi node to launch"
    echo "  -d                    Daemon mode (using start_erl)"
    echo "Configuration data is read from the configuration file"
    echo "provided to the setup_rvi_node.sh script that created the node."
    exit 1
}

mode=build
daemon=0
while getopts ":n:dbrp:s:" o; do
    case "${o}" in
        n)
            node_name=${OPTARG}
            ;;
        d)
            daemon=1
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
	echo "Node ${node_name} not setup. Please run: "
	echo "$SELF_DIR/setup_rvi_node.sh -n ${node_name} -c <configuration_file>"
	exit 2
    fi
    xboot="-boot ${node_name}/start"
    xname="-name ${node_name}"
    xcfg="-config ${node_name}/sys"
    CMD="erl ${xboot} ${xname} ${xcfg} -setcookie rvi_core"
    if [ ${daemon} = 1 ]
    then
	PIPE=/tmp/rvi_node/${node_name}
	LOG=${node_name}/log
	mkdir -p ${PIPE}
	mkdir -p ${LOG}
	echo "starting with run_erl"
        exec run_erl -daemon ${PIPE} ${LOG} "exec ${CMD}"
    else
	exec ${CMD}
    fi

elif [ "${mode}" = "release" ]
then
    echo "Not yet supported."
    exit 0
fi
