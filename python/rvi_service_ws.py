#!/usr/bin/python

#
# Copyright (C) 2015, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#
#
# Register a service specified by command line with an RVI node.
# Print out a message when the service gets invoked.
#
import sys
from rvilib import RVI
# pip-install websocket-client
import websocket
import json
import getopt

def usage():
    print "Usage:", sys.argv[0], "[-n <rvi_url>] <service_name>"
    print "  <rvi_url>                     URL of Service Edge on a local RVI node."
    print "                                Default: ws://localhost:9008"
    print "  <service_name>                URL of Service to register"
    print
    print "The Service Edge URL is logged as a notice when the"
    print "RVI node is started."
    print
   
    print "Example: ./rvi_service_ws.py -n ws://rvi1.nginfotpdx.net:9008 /test/some_service"
    sys.exit(255)


#
# Our general handler, registered with rvi.register_service() below.
#
# You can also explicitly name the arguments, but then
# the sender has to match the argument names.

# For example:
# rvi_call.py http://localhost:9001 jlr.com/vin/test a=1 b=2 c=3 ->
#    def service(a,b,c)
# 
def service_invoked(**args):
    print
    print "Service invoked!"
    print "args:", args 
    print
    sys.stdout.write("Press enter to quit: ")
    sys.stdout.flush()
    return ['ok']

def services_available(**args):
    print
    print "Services available!"
    print "args:", args 
    print
    sys.stdout.write("Press enter to quit: ")
    sys.stdout.flush()
    return ['ok']

def services_unavailable(**args):
    print
    print "Services unavailable!"
    print "args:", args 
    print
    sys.stdout.write("Press enter to quit: ")
    sys.stdout.flush()
    return ['ok']


# 
# Check that we have the correct arguments
#
opts, args= getopt.getopt(sys.argv[1:], "n:")

rvi_node_url = "ws://localhost:9008"
for o, a in opts:
    if o == "-n":
        rvi_node_url = a
    else:
        usage()

if len(args) != 1:
    usage()

service_name = args[0]


ws = websocket.create_connection(rvi_node_url)

tid = 1
payload = {}
payload['json-rpc'] = "2.0"
payload['id'] =tid
payload['method'] = "register_service"
payload['params'] = {"service_name": service_name}

# Register service
ws.send(json.dumps(payload))
reg_res = json.loads(ws.recv())

full_service_name = reg_res['service']
print "RVI node URL:        ", rvi_node_url
print "Service:             ", full_service_name

while True:
    print 
    print 'Press Ctrl-\ to quit.'
    inp = json.loads(ws.recv())
    print inp
    params=inp['params']
    if inp['method'] == 'services_available':
        print "Services available:", params['services']

    if inp['method'] == 'services_unavailable':
        print "Service unavailable:", params['services']

    if inp['method'] == 'message':
        print "Service invoked."
        print "service:    ", params['service_name']
        print "parameters: ", params['parameters']
