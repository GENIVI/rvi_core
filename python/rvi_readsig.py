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
# (see http://jwt.io for more libraries as well as an online debugger)
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
    print "Usage:", sys.argv[0], "signature_file public_key_file"
    print
    print "Example: ./rvi_readsig.py ~/certs/mycert.txt ~/keys/pubkey.pem"

    sys.exit(255)


# 
# Check that we have the correct arguments
#
opts, args= getopt.getopt(sys.argv[1:], "n:")

for o, a in opts:
    if o == "-n":
        rvi_node = a
    else:
        usage()

if len(args) != 2:
    usage()

sigfile = open(args[0], 'r')
keyfile = open(args[1], 'r')

sig = sigfile.read()
key = keyfile.read()

payload = jwt.decode(sig, key)

print payload
