#!/bin/sh
#
# Copyright (C) 2014, Jaguar Land Rover
#
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#
#
# Setup a correct configuration and launch an RVI release node.
# If a UUID file has not been created, it will be done at this time.
# Init.d script to start and stop an RVI system installed
# through an RPM.
#

SELF_DIR=$(dirname $(readlink -f "$0"))
CONFIG_DIR=/etc/opt/rvi
RVI_DIR=/opt/rvi

usage() {
    echo "Usage: $0 [-c config_file] start|stop|console"
    echo "  -c config_file  Configuration file to launch rvi node with. "
    echo "                  If omitted, previously used config file is used."
    echo
    echo "  start           Start an rvi node with the given configuration file."
    echo
    echo "  stop            Stop an rvi node previously started with start."
    echo
    echo "  console         Start an rvi in foreground mode."
    exit 1
}

CONFIG_FILE=""
while getopts "c:" o; do
    case "${o}" in
        c)
            CONFIG_FILE=${OPTARG}
            ;;
        *)
            usage
            ;;
    esac
done

shift $((${OPTIND}-1))
CMD=$1

if [ "$CMD" != "start" -a  "$CMD" != "stop" -a  "$CMD" != "console" ]
then
    usage
fi

if [ "$CMD" = "stop" ]
then
    ${RVI_DIR}/bin/rvi stop
    exit $?
fi

# Check if we have a uuid file.
if [ ! -f ${CONFIG_DIR}/device_id ]
then
    echo "Creating device ID in ${CONFIG_DIR}/device_id"
    cat /proc/sys/kernel/random/uuid > ${CONFIG_DIR}/device_id
fi

if [ -n "${CONFIG_FILE}" ]
then
    #
    # Check if we need to prepend current dir
    # to relative config file path
    #
    if [ $(echo ${CONFIG_FILE} | cut -c 1,1) != "/" ]
    then
	CONFIG_FILE=${PWD}/${CONFIG_FILE}
    fi

    # Check that config file can be read.
    if [ ! -r "${CONFIG_FILE}" ] ; then
	echo "${CONFIG_FILE} cannot be opened for reading."
	usage
    fi

    # 
    # Generate a config file that will end up as
    # /tmp/rvi/sys.config
    #
    (
	cd /tmp/
	rm -rf rvi
	export ERL_LIBS=${RVI_DIR}/setup:${RVI_DIR}/lib/
	${RVI_DIR}/setup_gen rvi ${CONFIG_FILE} rvi
    )

    # Did we succeed with config generation?
    if [ "$?" != "0" ]
    then
	# Nope
	echo "Failed to process configuration file."
	exit "$?"
    fi

    # Copy created config file to /etc/opt/rvi/sys.config,
    # which is symlinked to by /opt/rvi/sys.config
    cp /tmp/rvi/sys.config /etc/opt/rvi/sys.config
fi

exec /opt/rvi/bin/rvi ${CMD}
