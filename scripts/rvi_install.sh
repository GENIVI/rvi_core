#!/bin/sh
#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Setup an RVI release
#
#

SELF_DIR=$(dirname $(readlink -f "$0"))
SETUP_GEN=$SELF_DIR/setup_gen  # Ulf's kitchen sink setup utility

usage() {
    echo "Usage: $0 binary_dir library_dir log_dir [prefix_strip]"
    echo 
    echo "RVI binaries will be installed in 'binary_dir'."
    echo "RVI libraries will be installed in 'library_dir'."
    echo "RVI logging will be done in 'log_dir'."
    echo
    echo "If 'prefix_strip' is provided, that part of the directories above"
    echo "will be stripped of the given prefix in all internlal references."
    echo
    echo "If, for example, 'binary_dir' is './build/root/usr/bin', and"
    echo "'perfix_strip' is './build/root', all internal references"
    echo "in the files installed udner './build/root/usr/bin' will"
    echo "reference '/usr/bin'."
    echo "Useful in build systems where packages are built up"
    echo "in subdirectories."
    echo 
    echo "The created node can be started with: 'binary_dir'/rvi_ctl"
    echo "The RVI installation will rely on a separate erlang install"
    echo "to run."
    exit 1
}



shift $((${OPTIND}-1))

if [ "${#}" != "3" -a  "${#}" != "4" ]
then
    echo "Wrong number of arguments."
    usage
fi

BIN_DIR=$1
LIB_DIR=$2
LOG_DIR=$3

if [ "${#}" = "4" ]
then
    PREFIX_STRIP=$4
fi

rm -rf ${LIB_DIR} ${BIN_DIR} > /dev/null 2>&1 

install --mode=0755 -d ${BIN_DIR}
install --mode=0755 -d ${LIB_DIR}
install --mode=0755 -d ${LOG_DIR}

FILE_SET=$(find ebin components deps -name ebin -o -name priv)

tar cf - ${FILE_SET} | (cd ${LIB_DIR} ; tar xf - )

# Patch up the rvi with the correct directories.
if [ -s "${PREFIX_STRIP}" ] 
then
    STRIP_BIN_DIR=$(echo ${BIN_DIR} | sed "s|^${PREFIX_STRIP}||")
    STRIP_LIB_DIR=$(echo ${LIB_DIR} | sed "s|^${PREFIX_STRIP}||")
    STRIP_LOG_DIR=$(echo ${LOG_DIR} | sed "s|^${PREFIX_STRIP}||")
else
    STRIP_BIN_DIR=${BIN_DIR}
    STRIP_LIB_DIR=${LIB_DIR}
    STRIP_LOG_DIR=${LOG_DIR}
fi

sed -e "s|__RVI_LIBDIR__|${STRIP_LIB_DIR}|g" \
    -e "s|__RVI_BINDIR__|${STRIP_BIN_DIR}|g" \
    -e "s|__RVI_LOGDIR__|${STRIP_LOG_DIR}|g" < scripts/rvi_ctl > /tmp/rvi_ctl

install --mode=0755 /tmp/rvi_ctl ${BIN_DIR}
install --mode=0755 scripts/setup_gen ${BIN_DIR}
install --mode=0755 rel/files/nodetool ${BIN_DIR}

echo "RVI binary files installed under ${BIN_DIR}"
echo "RVI library files installed under ${LIB_DIR}"
echo "RVI log files installed under ${LOG_DIR}"
echo
echo "Start:              ${BIN_DIR}/rvi_ctl -c <config_file> start"
echo "Attach started RVI: ${BIN_DIR}/rvi_ctl attach"
echo "Stop:               ${BIN_DIR}/rvi_ctl stop"
echo "Start console mode: ${BIN_DIR}/rvi_ctl -c <config_file> console"
echo
exit 0

