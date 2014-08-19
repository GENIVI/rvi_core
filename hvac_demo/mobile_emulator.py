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
#  The mobile device emulator connects to Service Edge of the
#  backend server, and subscribes to HVAC updates (temp, fan speed,
#  etc) updates from an vehicle with a given VIN. The emulator can also
#  update HVAC values, simulating HVAC GUI input on the mobile device
#  screen, and send it off to the backend server Service Edge for
#  further distribution to the targeted vehicle.
#
#  
#  Updates entered on the command line are are sent to the backend node
#  service jlr.com/backend/hvac/subscription_service/publish. This
#  service will look up VIN targeted by the update and send out the
#  same data to all services that are subscribing to that VIN.
#  
#  The mobile device emulator will subscribe to updated values
#  published to to the VIN value "hvac_[vin]" where vin is the VIN
#  number of the vehicle.  When a mobile device emulator
#  sends out an updated value (entered in the python console), it will
#  publish the value using the vin "mobile_[vin]"
#
#  Converesely, an HVAC emulator will sbuscribe to
#  "mobile_[vin]", and publish to "hvac_[vin]".
#
#  This setup allows the mobile device emulator to receive updates
#  entered at the HVAC emulator's command promp (sent out by the HVAC
#  emulator to "hvac_[vin]"), while the HVAC emulator receives updates
#  entered on the mobile device screen (sent out by the mobile device
#  to "mobile_[vin]").
#
#  The emulator will register with a phone number as a part of its
#  service name, where the phone number is specified as a command line
#  argument.
#
#  Since the backend RVI node that the mobile device emulator connects
#  to has a service prefix of jlr.com/backend (see
#  backend.config), the mobile device emulator will have a
#  complete service name of:
#  jlr.com/backend/mobile/[phone_number]/hvac/publish
#
#  The VIN of the vehicle whose HVAC to manage is also provided as a
#  command line argument.
#
# Usage:
#
# python hvac_emulator.py <rvi_url> <phone_number> <vin>
#
# Example:
#  Regsiter on Service edge with URL http://127.0.0.1:8811, using
#  phone number +19491231234 and controlling the vehicle with VIN 1234.
#
#    python hvac_emulator.py http://127.0.0.1:8811 +19491231234 1234
#

import sys
from rvi_json_rpc_server import RVIJSONRPCServer
import jsonrpclib
import random
import threading

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
                   target = target_hvac_subscribe_service,
                   timeout = 0, # Not yet implemented
                   parameters = [ { u'subscribing_service': full_emulator_service_name}])

print "Mobile Device Emulator."
print "Backend RVI node URL:     ", rvi_url
print "Emulator URL:             ", emulator_service_url
print "Phone Number:             ", phone_number
print "VIN:                      ", vin
print "Full Service Name:        ", full_emulator_service_name

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
                       target = target_hvac_publish_service,
                       timeout = 0,
                       parameters = [{ u'key': k, 
                                      u'value': v}])

    print('Key {} set to {} for vin{}'. format(k, v, vin))
