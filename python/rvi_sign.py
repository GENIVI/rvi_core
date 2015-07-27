#!/usr/bin/python

#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#
# 
# Reads signed certificate, validates signature and prints payload
# RVI signed certs use the JWT format, described in
# https://tools.ietf.org/html/draft-ietf-jose-json-web-signature-41
#
# Keys are generated using `openssl genrsa -out PemFile 2048`,
# and the cert signature is calculated using {"alg": "RS256"}
#  

import sys
import json
import jwt
import time
import getopt
def usage():
    print "Usage:", sys.argv[0], "Json_file Public_key_file Out_file"

    sys.exit(255)


# 
# Check that we have the correct arguments
#
opts, args= getopt.getopt(sys.argv[1:], "")

if len(args) != 3:
    usage()

certfile = open(args[0], 'r')
keyfile = open(args[1], 'r')
outfile = open(args[2], 'w')

certstr = certfile.read()
cert = json.loads(certstr)

key = keyfile.read()

result = jwt.encode(cert, key, algorithm='RS256')

outfile.write(result)
