#!/usr/bin/python

#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Emulate a mobile device.
#
#  The mobile device emulator connects to Service Edge of the backend
#  server, started with "./rvi_node -n backend", and subscribes to
#  HVAC updates (temp, fan speed, etc) updates from the vehicle with a
#  given VIN. 
#
#  At startup, the mobile emulator will do two things:
#  
#  1. Register a service with the backend RVI node. 
#   
#     This service is invoked by the HVAC emulator to transmit 
#     key/value pairs entered on its commandd line.
#
#     The service name has the following layout:
#       jlr.com/backend/mobile/[phone_nr]/hvac/publish
# 
#     The jlr.com/backend prefix is added by the RVI node to make
#     the service globally unique. The [phone_nr] is the command-line
#     specified phone number.
#
#  2. Send a suscription request to the HVAC emulator.
#     The HVAC emulator's subscription service is invoked as
#       jlr.com/vin/[vin]/hvac/subscribe
#
#     where [vin] is the command-line specified VIN of the 
#     vehicle RVI node to connect to.
#     The subscription request's only parameter is the full name of
#     the publish service registered in step 1.  When the HVAC
#     emulator has a key/value pair entered at its command line, the
#     key/value/vin will be sent to the publish service on the mobile
#     emulator.
#    
#
#  When a new key/value pair is entered on the mobile emulator's
#  command line, simulating screen input on the HVAC app, the
#  key/value pair will be sent to:
#    jlr.com/vin/[vin]/hvac/publish
#
#  The HVAC emulator will receive the published key/value pair and
#  print it on screen.
#  
# Usage:
#
# python mobile_emulator.py <rvi_url> <phone_number> <vin>
#
# Example: mobile_emulator.py http://127.0.0.1:8801 +19491231234 1234
#
#  http://127.0.0.1:8801 is the RVI node's Service Edge URL.
#  See backend.conf's env -> rvi -> components -> service_edge -> url
#  for the correct value.
# 
#  +19491231234 is the phone number. 
#  It can be an arbitrary value, but has to be unique across all
#  registered mobile emulators.
#
#  1234 is the VIN number of the vehicle RVI node.
#  Please see the last field in the vehicle.config's 
#  env -> rvi -> node_service_prefix configuration entry for
#  the correct value.
#  

import sys
from rvi_json_rpc_server import RVIJSONRPCServer
import jsonrpclib
import random
import threading
import time

def usage():
    print "Usage:", sys.argv[0], " <rvi_url> <phone_number> <vin>"
    print "  <rvi_url>        URL of RVI Service Edge on local host."
    print "  <phone_number>   Phone number to register emulator as."
    print "  <vin>            The VIN that the mobile device is to control."
    print
    print "The RVI Service Edge URL can be found in"
    print "[backend,vehicle].config as"
    print "env -> rvi -> components -> service_edge -> url"
    print
    print "The Service Edge URL is also logged as a notice when the"
    print "RVI node is started."
    sys.exit(255)

#
# Publish command is invoked by the 
# subscriber server above when it receives a publish command
# from a VIN that this emulator subscribes to.
#
def publish(vin, key, value):
    print
    print "Publish invoked!"
    print "vin:", vin 
    print "key:", key 
    print "value:", value 
    print
    sys.stdout.write("Enter <key> <val> or q to quit: ")
    sys.stdout.flush()
    return ['ok']



# 
# Check that we have the correct arguments
#
if len(sys.argv) != 4:
    usage()

[ progname, rvi_url, phone_number, vin ] = sys.argv    

# Setup self's server that we will receive incoming calls from service edge on.

#
# Setup a localhost URL, using a random port, that we will listen to
# incoming JSON-RPC publish calls on, delivered by our RVI service
# edge (specified by rvi_url).
#
emulator_service_host = 'localhost'
emulator_service_port = random.randint(20001, 59999)
emulator_service_url = 'http://'+emulator_service_host + ':' + str(emulator_service_port)

#
# Setup the target service, the vehicle hvac app emulator,
# that will receive 'publish' and 'subscribe' commands from us.
#
target_hvac_publish_service = 'jlr.com/vin/'+vin+'/hvac/publish'
target_hvac_subscribe_service = 'jlr.com/vin/'+vin+'/hvac/subscribe'

#
# Setup the service name we will register with.
# The complete service name will be: 
#    jlr.com/backend/mobile/<phone_nr>/hvac/publish
#
emulator_service_name = '/mobile/'+phone_number+'/hvac/publish'

#
# Setup an outbound JSON-RPC connection to the backend RVI node
# Service Edge.
#
rvi_server = jsonrpclib.Server(rvi_url)

emulator_service = RVIJSONRPCServer(addr=((emulator_service_host, emulator_service_port)), 
                                    logRequests=False)

# Register the publish function with the publish service
# so that publish() gets called when we receive a message
# to /mobile/[phone_nr]/hvac/publish
#
emulator_service.register_function(publish, emulator_service_name)

# Create a thread to handle incoming stuff so that we can do input
# in order to get new values
thr = threading.Thread(target=emulator_service.serve_forever)
thr.start()


# We may see traffic immediately from the RVI node when
# we register. Let's sleep for a bit to allow the emulator service
# thread to get up to speed.
time.sleep(0.5)


#
# Register our HVAC mobile emulator service with backend RVI node
# Service Edge.
#
print "Emulator service URL", emulator_service_url

res = rvi_server.register_service(service = emulator_service_name, 
                                  network_address = emulator_service_url)

# Service Edge will return the full, global service name that the
# registration was recorded as:
#
#  jlr.com/backend/mobile/[phone_number]/hvac/publish
#
full_emulator_service_name = res['service']

#
# Send of a subscribe to the hvac emulator running on the
# vehicle. 
#
# We provide our own full service name to be invoked when
# a value is updated on the command line of the hvac emulator.
#
rvi_server.message(calling_service = emulator_service_name,
                   service_name = target_hvac_subscribe_service,
                   timeout = int(time.time()) + 60, # Not yet implemented
                   parameters = [ { u'subscribing_service': full_emulator_service_name}])

print "Mobile Device Emulator."
print "Backend RVI node URL:     ", rvi_url
print "Emulator URL:             ", emulator_service_url
print "Phone Number:             ", phone_number
print "VIN:                      ", vin
print "Full Service Name:        ", full_emulator_service_name

while True:
    line = raw_input('Enter <key> <val> or "q" to quit: ')
    if line == 'q':
        emulator_service.shutdown()
        sys.exit(0)

    # Read a line and split it into a key val pair
    lst = line.split(' ')
    if len(lst) != 2:
        print "Nope", len(lst), lst
        continue
    
    [k, v] = line.split(' ')
    
    # Send out update to the subscriber
    rvi_server.message(calling_service= emulator_service_name,
                       service_name = target_hvac_publish_service,
                       timeout = int(time.time()) + 60,
                       parameters = [{ u'key': k, 
                                      u'value': v}])

    print('Key {} set to {} for vin{}'. format(k, v, vin))
