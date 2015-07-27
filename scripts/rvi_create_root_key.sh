#!/bin/sh

#
# Copyright (C) 2015, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Create a new root key and break it up into a public and a private PEM file
#
OUTPREFIX=""
BITS=""


while getopts ":o:b:" opt
do
  case $opt in
    o)
      OUTPREFIX=$OPTARG
      ;;

    b)
      BITS=$OPTARG
      ;;

    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
      :)
	  echo "Option -$OPTARG requires an argument." >&2
	  exit 1
	  ;;
  esac
done


if [ "${OUTPREFIX}" = "" -o "$BITS" = "" ]
then
    echo "Usage: $0 -o <out_file> -b <bits>"
    echo "  -o <out_file> Name prefix of generated private and public file names"
    echo "  -b <bits> Key bit length. 2048 or higher."
    exit 255
fi

openssl genrsa -out ${OUTPREFIX}_priv.pem ${BITS}
openssl rsa -pubout -in ${OUTPREFIX}_priv.pem -out ${OUTPREFIX}_pub.pem

echo "${OUTPREFIX}_priv.pem - private root key"
echo "${OUTPREFIX}_pub.pem  - public root key"
echo
echo "Use ./rvi_create_device_key.py -p ${OUTPREFIX}_priv.pem -b <bits> -o <device_key_file>"
echo "to create a device key pair signed by the generated private root key."
echo 
echo "Use ./rvi_create_certificate.py ... --root_key=${OUTPREFIX}_priv.pem ..."
echo "to sign a created certificate with the generated private root key."

