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

g_fd = 0
g_package = ''
g_chunk_size = 0
g_total_size = 0
g_chunk_index = 0

rvi_sota_prefix = "jlr.com/backend/sota"
available_packagess = []

class WebSocketsHandler(SocketServer.StreamRequestHandler):
    magic = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'
 
    def setup(self):
        SocketServer.StreamRequestHandler.setup(self)
        print "connection established", self.client_address
        self.handshake_done = False
 
    def handle(self):
        print "Handle"
        self.active = True
        while self.active:
            if not self.handshake_done:
                self.handshake()
            else:
                self.read_next_message()
 
    def read_next_message(self):
        msg = self.rfile.read(2)
        if len(msg) < 2:
            print "Connection closed"
            self.finish()
            self.active = False
            return 

        length = ord(msg[1]) & 127
        if length == 126:
            length = struct.unpack(">H", self.rfile.read(2))[0]
        elif length == 127:
            length = struct.unpack(">Q", self.rfile.read(8))[0]
        masks = [ord(byte) for byte in self.rfile.read(4)]
        decoded = ""
        for char in self.rfile.read(length):
            decoded += chr(ord(char) ^ masks[len(decoded) % 4])
        self.on_message(decoded)
 
    def send_message(self, message):
        self.request.send(chr(129))
        length = len(message)
        if length <= 125:
            self.request.send(chr(length))
        elif length >= 126 and length <= 65535:
            self.request.send(chr(126))
            self.request.send(struct.pack(">H", length))
        else:
            self.request.send(chr(127))
            self.request.send(struct.pack(">Q", length))
        self.request.send(message)
 
    def handshake(self):
        data = self.request.recv(1024).strip()
        headers = Message(StringIO(data.split('\r\n', 1)[1]))
        if headers.get("Upgrade", None) != "websocket":
            return
        print 'Handshaking...'
        key = headers['Sec-WebSocket-Key']
        digest = b64encode(sha1(key + self.magic).hexdigest().decode('hex'))
        response = 'HTTP/1.1 101 Switching Protocols\r\n'
        response += 'Upgrade: websocket\r\n'
        response += 'Connection: Upgrade\r\n'
        response += 'Sec-WebSocket-Accept: %s\r\n\r\n' % digest
        self.handshake_done = self.request.send(response)
    

    def on_message(self, message):
        cmd = json.loads(message)
        tid = cmd['id']
        if cmd['method'] == 'GetPendingUpdates':
            print "Got message", message
            self._get_pending_updates(cmd['id'])
            return
            
        if cmd['method'] == 'StartUpdate':
            print "Got StartUpdate"
            self._start_update(cmd['id'])
            self.send_message(json.dumps({'jsonrpc': '2.0',
                                          'id': tid,
                                          'result': "" }))
            return 

        if cmd['method'] == 'GetCarSyncState':
            # Check if we have closed the given file.
            # If so, return state idle to shut down progress bar
            # in HMI.
            if g_fd == -1:
                print "File has closed. Done"
                self.send_message(json.dumps({'jsonrpc': '2.0',
                                              'id': tid,
                                              'result': { 'progress': 100, 
                                                          'state': 'Idle'} }))
                return
            
            # CHeck that we have actual progress to report
            if g_chunk_size == 0 or g_total_size == 0:
                self.send_message(json.dumps({'jsonrpc': '2.0',
                                              'id': tid,
                                              'result': { 'progress': 0, 
                                                          'state': 'Update'} }))
                return

            print "Got message", message
            print "g_chunk_size", g_chunk_size
            print "g_total_size", g_total_size
            chunk_frac = float(g_chunk_size) / float(g_total_size )
            print "frac", chunk_frac
            print "g_chunk_index", g_chunk_index
            perc = float(g_chunk_index + 1) * chunk_frac * 100.0
            print "perc", perc

            self.send_message(json.dumps({'jsonrpc': '2.0',
                                          'id': tid,
                                          'result': { 'progress': perc, 
                                                      'state': 'Update'} }))
            # Change 'state' to Idle when done
    
            return
        print "UNKNOWN MESSAGE", message
        return 

    def _start_update(self, tid):
        global full_notify_service_name

        # Strip the last component off self's full notify
        # service name to get a destination to send over
        # to the SOTA server's initiate_download

        last_slash = full_notify_service_name.rfind('/')
        destination = full_notify_service_name[:last_slash]

        self.send_message(json.dumps({'jsonrpc': '2.0',
                                      'id': tid,
                                      'result': [ ] }))

        package = available_packagess.pop(0)['uuid']
        
        print "Will initate download of package:",package
        rvi_server.message(calling_service = "/sota",
                           service_name = rvi_sota_prefix + "/initiate_download",
                           transaction_id = "1",
                           timeout = int(time.time())+60,
                           parameters = [{ 
                               u'package': package,
                               u'destination': destination
                           }])

        return

    def _get_pending_updates(self, tid):
        global available_packagess
        print "Available Packages:", available_packagess
        result = { 
            'jsonrpc': '2.0',
            'id': tid,
            'result': available_packagess
        }
        self.send_message(json.dumps(result))
        
def usage():
    print "Usage:", sys.argv[0], "<rvi_url> <service_id>"
    print "  <rvi_url>         URL of  Service Edge on a local RVI node"
    print
    print "The RVI Service Edge URL can be found in"
    print "[backend,vehicle].config as"
    print "env -> rvi -> components -> service_edge -> url"
    print
    print "The Service Edge URL is also logged as a notice when the"
    print "RVI node is started."
    sys.exit(255)
        
 
def notify(package):
    print "Available packet:", package
    available_packagess.append({
        "uuid": package,
            "version": {
                "version_major":1,
                "version_minor":0,
                "version_build":0
            }
        })

    return {u'status': 0}

def start(package, chunk_size, total_size):
    global g_fd
    global g_package
    global g_chunk_size
    global g_total_size

    g_package = package
    g_chunk_size = chunk_size
    g_total_size = total_size
    file_name = "/tmp/" + package

    print "Starting package:", file_name
    g_fd = open(file_name, "w")
    print "open fd = ", g_fd
    return {u'status': 0}

def chunk(index, msg):
    global g_fd
    global g_package
    global g_chunk_size
    global g_chunk_index
    
    g_chunk_index = index
    decoded_msg  = base64.b64decode(msg)
    print "Got part of package:", g_package, " chunk:", index, " chunk_size:", g_chunk_size, " message size:", len(decoded_msg)
    g_fd.seek(g_chunk_index * g_chunk_size)
    g_fd.write(decoded_msg)
    g_fd.flush()
    return {u'status': 0}

def finish(dummy):
    global g_fd
    global g_package

    print "Package:", g_package, " is complete in /tmp"
    print "finish fd = ", g_fd
    g_fd.close()
    g_fd = -1

    print "Uninstalling old package"
    
    return {u'status': 0}

#
# Publish an updated HVAC value, entered at the command line of the
# HVAC emulator, to all services who have set themselves up as
# subscribers through the jlr.com/vin/1234/hvac/subscribe service.
#

#
# A list of service names that should be notified when a value is
# updated on the HVAC emulator's command line.
#
subscribers = []

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


# setup the service names we will register with
# The complete service name will be: 
#  jlr.com/vin/1234/hvac/publish
#       - and -
#  jlr.com/vin/1234/hvac/subscribe
#
# Replace 1234 with the VIN number setup in the
# node_service_prefix entry in vehicle.config

# Setup an outbound JSON-RPC connection to the RVI Service Edeg.
rvi_server = jsonrpclib.Server(rvi_url)

emulator_service = RVIJSONRPCServer(addr=((emulator_service_host, emulator_service_port)), 
                                    logRequests=False)


#
# Regsiter callbacks for incoming JSON-RPC calls delivered to
# the HVAC emulator from the vehicle RVI node's Service Edge.
#

emulator_service.register_function(notify, "/sota/notify" )
emulator_service.register_function(start, "/sota/start" )
emulator_service.register_function(chunk, "/sota/chunk" )
emulator_service.register_function(finish, "/sota/finish" )

# Create a thread to handle incoming stuff so that we can do input
# in order to get new values
thr = threading.Thread(target=emulator_service.serve_forever)
thr.start()

# Setup a websocket thread
# ws_server = SocketServer.TCPServer(("127.0.0.1", 12999), WebSocketsHandler)
ws_server = SocketServer.TCPServer(("", 9000), WebSocketsHandler)
ws_thread = threading.Thread(target=ws_server.serve_forever)
ws_thread.start()

# We may see traffic immediately from the RVI node when
# we register. Let's sleep for a bit to allow the emulator service
# thread to get up to speed.
time.sleep(0.5)

#
# Register our HVAC emulator service with the vehicle RVI node's Service Edge.
# We register both services using our own URL as a callback.
#

res = rvi_server.register_service(service = "/sota/notify",
                                  network_address = emulator_service_url)

full_notify_service_name = res['service']


res = rvi_server.register_service(service = "/sota/start",
                                  network_address = emulator_service_url)

full_start_service_name = res['service']


res = rvi_server.register_service(service = "/sota/chunk",
                                  network_address = emulator_service_url)

full_chunk_service_name = res['service']

res = rvi_server.register_service(service = "/sota/finish",
                                  network_address = emulator_service_url)

full_finish_service_name = res['service']

print "HVAC Emulator."
print "Vehicle RVI node URL:       ", rvi_url
print "Emulator URL:               ", emulator_service_url
print "Full notify service name :  ", full_notify_service_name
print "Full start service name  :  ", full_start_service_name
print "Full chunk service name  :  ", full_chunk_service_name
print "Full finish service name :  ", full_finish_service_name

while True:
    time.sleep(3600.0)
