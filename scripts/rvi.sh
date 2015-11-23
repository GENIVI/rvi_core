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

ERL=${ERL:=erl}

usage() {
    echo "Usage: $0 -d config_dir -c config_file -l log_dir \\" 
    echo "       start|stop|console|attach|ping"
    echo
    echo "  -c config_file  Configuration file to launch rvi node with. "
    echo "                  If omitted the rvi.config in the configuration "
    echo "                  directory will be used."
    echo
    echo "  -s short_name   Erlang node short name. Defaults to 'rvi_core'"
    echo
    echo "  -C cookie       Erlang node cookie to use. Defaults to 'rvi_cookue'"
    echo
    echo "  -d config_dir   Directory to put generated uuid 'device_id' file and"
    echo "                  processed config files."
    echo "                  Defauts to the '/etc/opt/rvi'."
    echo
    echo "  -l log_dir      The directory to store log files in."
    echo "                  Defaults to '/tmp/rvi/[config]/log' where [config]"
    echo "                  is the base name of the configuration file."
    echo
    echo "  start           Start an rvi node with the given configuration file."
    echo
    echo "  stop            Stop an rvi node previously started with 'start'."
    echo
    echo "  console         Start an rvi in foreground mode."
    echo 
    echo "  attach          Attach to an rvi node previously started with 'start'."
    echo
    echo "  ping            Ping to check if an rvi node is up. Returns 0 if up."
    exit 1
}

CONFIG_FILE=""

SNAME=rvi_core
COOKIE=rvi_cookie
unset CONFIG_DIR
unset LOG_DIR
while getopts "c:d:l:s:C:" o; do
    case "${o}" in
        s)
            SNAME=${OPTARG}
            ;;
        c)
            CONFIG_FILE=${OPTARG}
            ;;
        C)
            COOKIE=${OPTARG}
            ;;
        d)
            CONFIG_DIR=${OPTARG}
            ;;
        l)
            LOG_DIR=${OPTARG}
            ;;
        *)
            usage
            ;;
    esac
done

CONFIG_DIR=${CONFIG_DIR:=/etc/opt/rvi}

shift $((${OPTIND}-1))
CMD=$1

if [ "${CMD}" != "start" -a "${CMD}" != "stop" -a  "${CMD}" != "console" -a  "${CMD}" != "ping" ]
then
    usage
fi

export ERL_LIBS=${SELF_DIR}/rvi_core:${SELF_DIR}/rvi_core/deps:${SELF_DIR}/rvi_core/components


# Check that we have a config dir
if [ ! -d ${CONFIG_DIR} ]
then
    install -d --mode=0755 ${CONFIG_DIR}
fi

# Check if we have a uuid file.
if [ ! -f ${CONFIG_DIR}/device_id ]
then
    echo "Creating device ID in ${CONFIG_DIR}/device_id"
    cat /proc/sys/kernel/random/uuid > ${CONFIG_DIR}/device_id
fi

#
# See if we need to process a config file
#
if [ ${CMD} = "start" -o ${CMD} = "console" ]
then
    # Default to rvi.config
    CONFIG_FILE=${CONFIG_FILE:=${CONFIG_DIR}/rvi.config}
    #
    # Check if we need to prepend current dir
    # to relative config file path
    #
    if [ $(echo ${CONFIG_FILE} | cut -c 1,1) != "/" ]
    then
	CONFIG_FILE=${PWD}/${CONFIG_FILE}
    fi

    # Check that config file can be read.
    if [ ! -r "${CONFIG_FILE}" ]
    then
	echo "${CONFIG_FILE} cannot be opened for reading."
	usage
    fi
    # 
    # Generate a config file that will end up as
    # /tmp/rvi/sys.config
    #
    (
	cd ${CONFIG_DIR}
	${SELF_DIR}/scripts/setup_gen rvi ${CONFIG_FILE} rvi
    )

    # Did we succeed with config generation?
    if [ "$?" != "0" ]
    then
	# Nope
	echo "Failed to process configuration file."
	exit "$?"
    fi
fi
   
TMP_DIR=/tmp/rvi/$(basename ${CONFIG_FILE} .config)
LOG_DIR=${LOG_DIR:=${TMP_DIR}/rvi/log}

LAUNCH="${ERL} -boot ${CONFIG_DIR}/rvi/start -sname ${SNAME} -config ${CONFIG_DIR}/rvi/sys -setcookie ${COOKIE}"

case "${CMD}" in
   start)
	 install -d --mode 0755  ${TMP_DIR}
	 install -d --mode 0755  ${LOG_DIR}
	 exec run_erl -daemon ${TMP_DIR}/ ${LOG_DIR} "exec ${LAUNCH}"
	 ;;

   console)
       exec ${LAUNCH}
       ;;

   stop)
       exec ${SELF_DIR}/scripts/nodetool -sname ${SNAME} -setcookie ${COOKIE} stop
       ;;

   ping)
       exec ${SELF_DIR}/scripts/nodetool -sname ${SNAME} -setcookie ${COOKIE} ping
       ;;

   attach) 
       exec to_erl ${TMP_DIR}
       ;;

esac 

