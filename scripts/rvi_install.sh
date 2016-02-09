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
# With the -d argument, a developer release will be built with
# only
#
#  Once setup, the RVI node can be started with ./rvi_node <release_na,e?
#
#  Please note that the generated release will depend on the built
#
#  In order to create a standalone release, use create_rvi_release.sh
#

#SELF_DIR=$(dirname $(readlink -f "$0"))
SELF_DIR=$(cd ${0%/*} && pwd)
echo "SELF_DIR=$SELF_DIR"
SETUP_GEN=$SELF_DIR/setup_gen  # Ulf's kitchen sink setup utility

usage() {
    echo "Usage: $0 binary_dir"
    echo 
    echo "RVI will be installed in 'target_dir'."
    echo 
    echo "The created node can be started with: 'target_dir'/rvi.sh"
    echo "RVI in 'target_dir' will rely on a native erlang to function"
    exit 1
}



shift $((${OPTIND}-1))

if [ "${#}" != "1" ]
then
    echo "Target directory not specifiied."
    usage
fi

TARGET_DIR=$1

#
# Prepend abs path if TARGET_DIR is relative
#
if [ $(echo ${TARGET_DIR} | cut -c 1,1) != "/" ]
then
    TARGET_DIR=${PWD}/${TARGET_DIR}
fi


cd ${SELF_DIR}/..;

rm -rf ${TARGET_DIR} > /dev/null 2>&1 

install -m 0755 -d ${TARGET_DIR}/rvi_core
install -m 0755 -d ${TARGET_DIR}/scripts

FILE_SET=$(find priv ebin components deps -name ebin -o -name priv)

echo "Installing rvi at ${TARGET_DIR}."

tar cf - ${FILE_SET} | (cd ${TARGET_DIR}/rvi_core ; tar xf - )
install -m 0755 scripts/rvi.sh ${TARGET_DIR}
install -m 0755 scripts/setup_gen ${TARGET_DIR}/scripts
install -m 0755 rel/files/nodetool ${TARGET_DIR}/scripts
install -m 0755 scripts/rvi_create_root_key.sh ${TARGET_DIR}/scripts
install -m 0755 scripts/rvi_create_device_key.sh ${TARGET_DIR}/scripts
install -m 0755 scripts/rvi_create_certificate_key.sh ${TARGET_DIR}/scripts


echo "RVI installed under ${TARGET_DIR}"
echo "Start:              ${TARGET_DIR}/rvi.sh -c <config_file> start"
echo "Attach started RVI: ${TARGET_DIR}/rvi.sh attach"
echo "Stop:               ${TARGET_DIR}/rvi.sh stop"
echo "Start console mode: ${TARGET_DIR}/rvi.sh -c <config_file> console"

exit 0

