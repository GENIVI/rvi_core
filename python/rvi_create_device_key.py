#!/usr/bin/python

#
# Copyright (C) 2015, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  T full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#
# 
#  1. Generate a device key through openssl.
#  2. Sign the public key using the provided private root key.
#  3. Create a JWT version of the signed device key.
#

import getopt
import sys
from Crypto.PublicKey import RSA 

# pip install jws
import jws

def usage():
    print "Usage:", sys.argv[0], "-p <priv_root_key> -o <prefix> -b <bits>"
    print "  -p <priv_root_key>  Private root key file to sign public device key with."
    print "  -o <prefix>         File name prefix for generated device keys."
    print "  -b <bits>           Device key length. Default 2048."
    sys.exit(255)

opts, args= getopt.getopt(sys.argv[1:], "p:o:b:")

priv_root_key_fname=""
fname_prefix=""
key_lenth=2048
for o, a in opts:
    if o == "-p":
        priv_root_key_fname = a
    elif o == "-b":
        key_length = int(a)
    elif o == "-o":
        fname_prefix = a
    else:
        usage()
        
if priv_root_key_fname == "" or fname_prefix == "":
    usage()


#
# Generate the device RSA key pair
#
new_key = RSA.generate(bits=key_length) 


#
# Create private key file, which also contains the public key.
#
priv_key_fname = "{}_priv.pem".format(fname_prefix)
priv_key_file = open(priv_key_fname, 'w')
priv_key = new_key.exportKey("PEM") 
priv_key_file.write(priv_key)
priv_key_file.close()


# Read the root private key
priv_root_key_file = open(priv_root_key_fname, 'r')
priv_root_key = RSA.importKey(priv_root_key_file.read())
priv_root_key_file.close()


#
# Extract the device public key that we are to sign with the private root key.
# Dump it in a file to be used by rvi_create_certificate.py
#
pub_key_fname = "{}_pub.pem".format(fname_prefix)
pub_key_file = open(pub_key_fname, 'w')
pub_key = new_key.publickey().exportKey("PEM") 
pub_key_file.write(pub_key)
pub_key_file.close()


# Generate a JWS signature based on the pub key and the private root key
header = { 'alg': 'RS256' }
signature = jws.sign(header, pub_key, priv_root_key)



# Verify that we can use the public root key to verify the key.
pub_root_key = priv_root_key.publickey().exportKey()

if not jws.verify(header, pub_key, signature, pub_root_key):
    print "FAILED VERIFICATION!"
    print "The public portion of the generated device key, signed by the provided private root key,"
    print "could not be verified using the public root key."
    os.remove(priv_key_fname)
    sys.exit(0)

#
# Create signed public JWM file
#
pub_sign_key_fname = "{}_pub_sign.jwm".format(fname_prefix)

pub_sign_key_file = open(pub_sign_key_fname, 'w')
pub_sign_key_file.write(signature)
pub_sign_key_file.close()

print "Device private/public key pair stored in:                    ", priv_key_fname
print "Device public-only key stored in:                            ", pub_key_fname
print "Device public key, signed by private root key, stored in:    ", pub_sign_key_fname
print "Root key used to sign the device public key read from:       ", priv_root_key_fname
print
print "Set rvi node's device_key_pair config parameter to point to: ", priv_root_key_fname
print "Set rvi node's authorize_jwt config parameter to point to:   ", pub_sign_key_fname
print
print "use ./rvi_create_certificate.py ... --device_key={} to include".format(pub_key_fname)
print "device public key in a certificate."
