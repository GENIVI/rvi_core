#!/usr/bin/python

#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Simple SOTA SERVER
#
import sys
from rvi_json_rpc_server import RVIJSONRPCServer
import jsonrpclib
import random
import time
import threading
import os
import base64
import struct
import SocketServer
from base64 import b64encode
from hashlib import sha1
from mimetools import Message
from StringIO import StringIO
import json
import Queue
        
transaction_id = 0
package_queue = Queue.Queue()

def usage():
    print "Usage:", sys.argv[0], "<rvi_url>"
    print "  <rvi_url>         URL of  Service Edge on a local RVI node"
    print
    print "The RVI Service Edge URL can be found in"
    print "[backend,vehicle].config as"
    print "env -> rvi -> components -> service_edge -> url"
    print
    print "The Service Edge URL is also logged as a notice when the"
    print "RVI node is started."
    sys.exit(255)
        
 
def report(vin, timestamp, data):
    print "Got report from", vin, " timestamp", timestamp
    print data
    return {u'status': 0}

#
# Setup a localhost URL, using a random port, that we will listen to
# incoming JSON-RPC publish calls on, delivered by our RVI service
# edge (specified by rvi_url).
#
emulator_service_host = 'localhost'
emulator_service_port = random.randint(20001, 59999)
emulator_service_url = 'http://'+emulator_service_host + ':' + str(emulator_service_port)

# 
# Check that we have the correct arguments
#
if len(sys.argv) != 2:
    usage()

# Grab the URL to use
[ progname, rvi_url ] = sys.argv    



# Setup an outbound JSON-RPC connection to the RVI Service Edge.
rvi_server = jsonrpclib.Server(rvi_url)

emulator_service = RVIJSONRPCServer(addr=((emulator_service_host, emulator_service_port)), 
                                    logRequests=False)


#
# Regsiter callbacks for incoming JSON-RPC calls delivered to
# the SOTA server from the vehicle RVI node's Service Edge.
#
emulator_service.register_function(report, "/logging/report" )


# Create a thread to handle incoming stuff so that we can do input
# in order to get new values
thr = threading.Thread(target=emulator_service.serve_forever)
thr.start()

# We may see traffic immediately from the RVI node when
# we register. Let's sleep for a bit to allow the emulator service
# thread to get up to speed.
time.sleep(0.5)

#
# Register our HVAC emulator service with the vehicle RVI node's Service Edge.
# We register both services using our own URL as a callback.
#
res = rvi_server.register_service(service = "/logging/report", network_address = emulator_service_url)
full_report_name = res['service']


print "Big Data Emulator."
print "Vehicle RVI node URL:     ", rvi_url
print "Emulator URL:             ", emulator_service_url
print "Full report service name: ", full_report_name


while True:
    transaction_id += 1
    line = raw_input('Enter <subscribe|unsubscribe> <vin>  <channel> [subscribe-interval] or "q" for quit: ')
    if line == 'q':
        emulator_service.shutdown()
        sys.exit(0)

    
        
    # Read a line and split it into a key val pair
    lst = line.split(' ')
    if len(lst) == 3:
        [cmd, vin, channel] = line.split(' ')
        interval = 0
    elif len(lst) == 4:
        [cmd, vin, channel, interval] = line.split(' ')
    else:
        print "Nope", len(lst), lst
        continue
    
    dst = 'jlr.com/vin/'+vin+'/logging/'
    
    if cmd[0] == 's':
        command = 'subscribe'
        if interval == 0:
            print 'Please specify interval parameter for subscribe commands.'
            continue

        params = [{ u'channels': [ channel ], u'interval': interval}]
    else:
        if interval != 0:
            print 'Please do not specify interval parameter for unsubscribe commands.'
            continue
            
        command = 'unsubscribe'
        params = [{ u'channels': [ channel ]}]
   
    rvi_server.message(calling_service = "/big_data",
                       service_name = dst + command,
                       transaction_id = str(transaction_id),
                       timeout = int(time.time())+60,
                       parameters = params)

    print('{}({}) sent to {}'.format(command, channel, vin))
