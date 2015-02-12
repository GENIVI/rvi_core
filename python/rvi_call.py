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

def usage():
    print "Usage:", sys.argv[0], " RVI-node service key=val ..."
    print "  RVI-node     DNS name or IP of host running RVI"
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
if len(sys.argv) <3:
    usage()

progname = sys.argv[0]
rvi_node = sys.argv[1]
service = sys.argv[2]
args = []
i=3
while i < len(sys.argv):
    print sys.argv[i]
    [k, v] = sys.argv[i].split('=')
    args = args + [{ k: v}]
    i = i + 1



#
# Setup an outbound JSON-RPC connection to the backend RVI node
# Service Edge.
#
rvi = RVI(rvi_node)


print "RVI Node:         ", rvi_node
print "Service:          ", service
print "args:             ", args

rvi.message(service, args)





