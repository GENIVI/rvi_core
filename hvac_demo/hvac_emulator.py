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


#
# Publish command is invoked by the mobile emulator when it sends
# messages to jlr.com/vin/1234/hvac/publish service.
#
def publish(key, value):
    print
    print "Publish invoked!"
    print "key:", key 
    print "value:", value 
    print
    sys.stdout.write("Enter <key> <val> or q to quit: ")
    sys.stdout.flush()
    return ['ok']

#
# Subscribe command is invoked by the mobile emulator when it sends
# messages to the jlr.com/vin/1234/hvac/subscribe service.
# The single argument provided is the subscribing service to be invoked when
# an value is updated on the command line of the HVAC emulator.
#
def subscribe(subscribing_service):
    print
    print "Got subscription"
    print "vin:", vin 
    print "subscribing_service:", subscribing_service 
    print 
    # Add the subscribing service to list of subscribers,
    # given that is is not already there.
    if not subscribing_service in subscribers:
        subscribers.append(subscribing_service)
    else:
        print subscribing_service," already subscribing"

    sys.stdout.write("Enter <key> <val> or q to quit: ")
    sys.stdout.flush()
    return ['ok']

#
# Publish an updated HVAC value, entered at the command line of the
# HVAC emulator, to all services who have set themselves up as
# subscribers through the jlr.com/vin/1234/hvac/subscribe service.
#
def publish_to_subscribers(vin, key, val):
    for subscriber in subscribers:
        print "Sending:",vin,"   key:", key,"   val:",val,"   to:", subscriber
        rvi_server.message(calling_service = emulator_publish_service_name,
                           target = subscriber,
                           timeout = 0,
                           parameters = [{u'vin': vin, u'key': key, u'value': val}])

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
emulator_publish_service_name = '/hvac/publish'
emulator_subscribe_service_name = '/hvac/subscribe'

# Setup an outbound JSON-RPC connection to the RVI Service Edeg.
rvi_server = jsonrpclib.Server(rvi_url)

#
# Register our HVAC emulator service with the vehicle RVI node's Service Edge.
# We register both services using our own URL as a callback.
#
res = rvi_server.register_service(service = emulator_publish_service_name, 

                                  network_address = emulator_service_url)

# 
# Record the returned full service name so that we can print it out
# below.
#
full_emulator_publish_service_name = res['service']

res = rvi_server.register_service(service = emulator_subscribe_service_name, 
                                  network_address = emulator_service_url)

# 
# Record the returned full service name so that we can print it out
# below.
#
full_emulator_subscribe_service_name = res['service']

[ t1, t2, vin, t3, t4] = full_emulator_subscribe_service_name.split('/')

# We are in mobile device mode. Setup the vin numbers for publish
pub_vin = "hvac_"+vin
sub_vin = "mobile_"+vin

print "HVAC Emulator."
print "Vehicle RVI node URL:       ", rvi_url
print "Emulator URL:               ", emulator_service_url
print "VIN:                        ", vin
print "Full publish service name   ", full_emulator_publish_service_name
print "Full subscribe service name ", full_emulator_subscribe_service_name

emulator_service = RVIJSONRPCServer(addr=((emulator_service_host, emulator_service_port)), 
                                    logRequests=False)

#
# Regsiter callbacks for incoming JSON-RPC calls delivered to
# the HVAC emulator from the vehicle RVI node's Service Edge.
#
emulator_service.register_function(publish, emulator_publish_service_name)
emulator_service.register_function(subscribe, emulator_subscribe_service_name)

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
    publish_to_subscribers(vin, k, v)

    print('Key {} set to {} for vin{}'. format(k, v, vin))
