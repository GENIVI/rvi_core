#!/bin/sh
#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Setup a correct configuration and launch an RVI release node.
# If a UUID file has not been created, it will be done at this time.
#

SELF_DIR=$(dirname $(readlink -f "$0"))
CONFIG_DIR=/etc/opt/rvi
BIN_DIR=/opt/rvi

usage() {
    echo "Usage: $0 -c config_file"
    echo "  -c config_file        Specify the configuration "
    echo "Configuration data is read from the configuration file."
    exit 1
}

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

if [ -z "${CONFIG_FILE}" ] ; then
    echo "Missing -c flag"
    usage
fi

# Check if we have a uuid file.
if [ ! -f ${CONFIG_DIR}/device_id ]
then
    echo "Creating device ID in ${CONFIG_DIR}/device_id"
    cat /proc/sys/kernel/random/uuid > ${CONFIG_DIR}/device_id
fi

# 
# Generate a config file that will end up as
# /tmp/rvi/sys.config
#
(
    cd /tmp/
    rm -rf rvi
    export ERL_LIBS=${BIN_DIR}/setup:${BIN_DIR}/lib/
    ${BIN_DIR}/setup_gen rvi $CONFIG_FILE rvi
)

# Did we succeed with config generation?
if [ "$?" != "0" ]
then
    # Nope
    exit "$?"
fi   

# Copy created config file to /etc/opt/rvi/sys.config,
# which is symlinked to by /opt/rvi/sys.config
cp /tmp/rvi/sys.config /etc/opt/rvi/sys.config

exec /opt/rvi/bin/rvi start
