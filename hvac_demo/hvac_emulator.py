#!/usr/bin/python

#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Emulate an HVAC apoplication
#
#  This emulator connects to an RVI Service Edge as a service and
#  simulates an HVAC application unit installed in a vehicle.
#
#  The emulator connects to the Service Edge of locally accessible RVI
#  node and subscribes to updates from one or more mobile
#  devices. Local updates, simulating GUI input on the HVAC screen,
#  can be entered at the command line and will be distributed to the
#  phone(s) subscribing to updates from the HVAC.
#
#  Any HVAC values updated on the command line are sent to the
#  service jlr.com/backend/hvac/subscription_service/publish. This
#  service will look up VIN targeted by the update and send out the
#  same data to all services that are subscribing to that VIN.
#  
#  I, the emulator will subscribe to updated
#  values published to to the VIN value "hvac_[vin]" where vin is the
#  VIN number of the vehicle.  When a emulator, still in mobile device
#  mode, sends out an updated value (entered in the python console),
#  it will publish the value using the vin "mobile_[vin]"
#
#  Converesely, an the HVAC emulator will sbuscribe to
#  "mobile_[vin]", and publish to "hvac_[vin]".
#
#  This setup allows the mobile device emulator to receive updates
#  entered on the IVI HVAC screen (sent out by the HVAC emulator to
#  "hvac_[vin]"), while the HVAC emulator receives updates entered on
#  the mobile device screen (sent out by the mobile device to
#  "mobile_[vin]".
#
#  When the HVAC emulatore connects to the vehicle RVI node, the
#  node's configured service prefix (see hvac_demo/vehicle.config,
#  node_service_prefix) will have the VIN number as a part of the
#  prefix. The emulator will thus register its service as /hvac/publish,
#  which, prefixed with the node service prefix, gives it a complete name
#  of jlr.com/vin/[vin]/hvac/publish.
#
#
# Example:
#  python hvac_mobile_emulator.pyh 9491231234 http://127.0.0.1:8811
#
import sys
from rvi_json_rpc_server import RVIJSONRPCServer
import jsonrpclib
import random
import threading

# The subscriber service (MQTT server equivalent), that 
# manages subscriptions and distributes received publish commands to
# the relevant subscribers.
#
SUBSCRIPTION_SERVICE_BASE='jlr.com/backend/subscription_service'
SUBSCRIBE_SERVICE=SUBSCRIPTION_SERVICE_BASE+'/subscribe'
PUBLISH_SERVICE=SUBSCRIPTION_SERVICE_BASE+'/publish'

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
    print "Usage:", sys.argv[0], "<rvi_url>"
    print "  <rvi_url>                     URL of  Service Edge on a local RVI node"
    print
    print "The RVI Service Edge URL can be found in"
    print "[backend,vehicle].config as"
    print "env -> rvi -> components -> service_edge -> url"
    print
    print "The Service Edge URL is also logged as a notice when the"
    print "RVI node is started."
    sys.exit(255)


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
# Check that we have the correct arguments
#
if len(sys.argv) != 2:
    usage()

# Grab the URL to use
[ progname, rvi_url ] = sys.argv    


# setup the service name we will register with
# The complete service name will be: jlr.com/vin/<vin>/hvac/publish
emulator_service_name = '/hvac/publish'

# Setup an outbound JSON-RPC connection to the RVI Service Edeg.
rvi_server = jsonrpclib.Server(rvi_url)

# Register our HVAC emulator service with the RVI Service Edge,
# allowing the RVI to forward requests to the service name to the
# given network addresss (URL):
res = rvi_server.register_service(service = emulator_service_name, 
                                  network_address = emulator_service_url)

# The returned full service name contains the VIN number that we want:
#  jlr.com/vin/<vin>/hvac/publish
#  We need to dig out the <vin> bit
full_emulator_service_name = res['service']

[ t1, t2, vin, t3, t4] = full_emulator_service_name.split('/')

# We are in mobile device mode. Setup the vin numbers for publish
pub_vin = "hvac_"+vin
sub_vin = "mobile_"+vin

print "HVAC Emulator."
print "Vehicle RVI node URL: ", rvi_url
print "Emulator URL:     ", emulator_service_url
print "VIN:              ", vin
print "Full service name ", full_emulator_service_name

# Regsiter self's service with the backend server RVI node
# See rvi_json_rpc_server.py._dispatch() for details on how
# incoming JSON-RPC requests are mapped to local funcitons.
#
emulator_service = RVIJSONRPCServer(addr=((emulator_service_host, emulator_service_port)), logRequests=False)
emulator_service.register_function(publish, emulator_service_name)

# Send of a subscribe to the subscription service running on the
# backend. See hvac_subscription_service.py.subscribe() for details.

rvi_server.message(calling_service = emulator_service_name,
                   target = SUBSCRIBE_SERVICE,
                   timeout = 0,
                   parameters = [{ u'vin': sub_vin, 
                                  u'subscribing_service': full_emulator_service_name}])

# Create a thread to handle incoming stuff so that we can do input
# in order to get new values
thr = threading.Thread(target=emulator_service.serve_forever)
thr.start()

while True:
    line = raw_input('Enter <key> <val> or "q" for quit: ')
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
                       target = PUBLISH_SERVICE,
                       timeout = 0,
                       parameters = [{ u'vin': pub_vin, 
                                      u'key': k, 
                                      u'value': v}])

    print('Key {} set to {} for vin{}'. format(k, v, vin))
