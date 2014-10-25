#!/usr/bin/python

#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Simple SOTA Client
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

def package_pusher():
    global package_queue
    global transaction_id

    while True:
        [package, destination] = package_queue.get()
        print "Package pushed will push",package,"to",destination
        try:
            f = open(package)

        except Err:
            print "Could not open",file_name,":", Err
            return
        

        chunk_size = 128*1024

        f_stat = os.stat(package)
    
        transaction_id += 1
        rvi_server.message(calling_service = "/sota",
                               service_name = destination + "/start",
                           transaction_id = str(transaction_id),
                           timeout = int(time.time())+60,
                           parameters = [{ u'package': package, 
                                           u'chunk_size': chunk_size,
                                           u'total_size': f_stat.st_size
                                       }])

        index = 0

        while True:
            offset = f.tell()
            msg =  f.read(chunk_size) 
            if msg == "":
                break

            print "Sending package:", package, " chunk:", index, " offset:", offset, " message size: ", len(msg)

            transaction_id+=1
            rvi_server.message(calling_service = "/sota",
                               service_name = destination + "/chunk",
                               transaction_id = str(transaction_id),
                               timeout = int(time.time())+60,
                               parameters = [
                                   { u'index': index }, 
                                   { u'msg': base64.b64encode(msg) }])
            
            index += 1

        f.close()
        print "Finishing package:", package
        time.sleep(1.0)

        transaction_id+=1
        rvi_server.message(calling_service = "/sota",
                           service_name = destination + "/finish",
                           transaction_id = str(transaction_id),
                           timeout = int(time.time())+60,
                           parameters = [ { u'dummy': 0}])

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
        
 
def initiate_download(package, destination):
    print "Will push packet", package, "to",destination
    
    package_queue.put([package, destination])
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
initiate_download_service_name = "/sota/initiate_download"
emulator_service.register_function(initiate_download, initiate_download_service_name )


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
res = rvi_server.register_service(service = initiate_download_service_name,
                                  network_address = emulator_service_url)

full_initiate_download_service_name = res['service']

print "HVAC Emulator."
print "Vehicle RVI node URL:                 ", rvi_url
print "Emulator URL:                         ", emulator_service_url
print "Full initiate download service name : ", full_initiate_download_service_name

chunk_size = 1024*64

#
# Start the queue dispatcher thread
#
package_pusher_thr = threading.Thread(target=package_pusher)
package_pusher_thr.start()

while True:
    transaction_id += 1
    line = raw_input('Enter <vin> <file_name> or "q" for quit: ')
    if line == 'q':
        emulator_service.shutdown()
        sys.exit(0)

    
        
    # Read a line and split it into a key val pair
    lst = line.split(' ')
    if len(lst) != 2:
        print "Nope", len(lst), lst
        continue
    
    [vin, file_name] = line.split(' ')
    dst = 'jlr.com/vin/'+vin+'/sota'
    try:
        f = open(file_name)
    except Err:
        print "Could not open",file_name,":", Err
        continue
    
    rvi_server.message(calling_service = "/sota",
                       service_name = dst + "/notify",
                       transaction_id = str(transaction_id),
                       timeout = int(time.time())+60,
                       parameters = [{ u'package': file_name}])

    print('Package {} sent to {}'. format(file_name, dst))
