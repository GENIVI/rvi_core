#!/usr/bin/python

#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#
# 
# Simple RVI service caller
#  

import sys
from rvilib import RVI
import threading
import time
import getopt
def usage():
    print "Usage:", sys.argv[0], "[-n RVI-node] service key=val ..."
    print "  RVI-node     DNS name or IP of host running RVI. "
    print "               default: http://localhost:8801"
    print "  service      Service to invoke in RVI."
    print "  key=val      Named arguments to provide to service."
    print
    print "Example: ./callrvi.py http://rvi1.nginfotpdx.net:8801 \\"
    print "                      jlr.com/vin/aaron/4711/test/ping \\"
    print "                      arg1=val1 arg2=val2"                    

    sys.exit(255)


# 
# Check that we have the correct arguments
#
opts, args= getopt.getopt(sys.argv[1:], "n:")


rvi_node = "http://localhost:8801"
for o, a in opts:
    if o == "-n":
        rvi_node = a
    else:
        usage()

if len(args) < 1:
    usage()

# Construct a dictionary from the provided paths.
i = 0
service = args[0]
rvi_args = []
for i in args[1:]:
    print i
    [k, v] = i.split('=')
    rvi_args = rvi_args + [{ k: v}]




#
# Setup an outbound JSON-RPC connection to the backend RVI node
# Service Edge.
#
rvi = RVI(rvi_node)

print "RVI Node:         ", rvi_node
print "Service:          ", service
print "args:             ", rvi_args

#
# Send the messge.
#
rvi.message(service, rvi_args)
