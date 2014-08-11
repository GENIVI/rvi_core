#!/usr/bin/python

#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# Emulate a mobile device or an IVI.
#
#  This emulator connects to an RVI Service Edge as a service and
#  takes on one of two roles:
#
#  1) mobile device
#     In this mode the emulator connects to the Service Edge of the
#     backend server (since P2P will be supported in RVI Project
#     Milestone 2), and subscribes to HVAC updates (temp, fan speed,
#     etc) updates from an IVI with a given VIN. The emulator can also
#     update HVAC values, simulating HVAC GUI input on the phone
#     screen, and send it off to the (backend server) Service Edge for
#     further distribution to the targeted IVI.
#
#  2) IVI 
#     In this mode, the emulator connects to the Service Edge of a
#     vehicle (device) and subscribes to updates from one or more
#     mobile devices. Local updates, simulating GUI input on the
#     HVAC screen, can be entered at the command line and will be
#     distributed to the phone(s) subscribing to updates from
#     the IVI.
#
#  
#  In both modes, the emulator sends its updated HVAC values to the
#  service jlr.com/backend/hvac/subscription_service/publish. This
#  service will look up VIN targeted by the update and send out the
#  same data to all services that are subscribing to that VIN.
#  
#  In mobile device mode, the emulator will subscribe to updated
#  values published to to the VIN value "ivi_[vin]" where vin is the
#  VIN number of the IVI.  When a emulator, still in mobile device
#  mode, sends out an updated value (entered in the python console),
#  it will publish the value using the vin "mobile_[vin]"
#
#  Converesely, an emulator in IVI mode will sbuscribe to
#  "mobile_[vin]", and publish to "ivi_[vin]".
#
#  This setup allows the mobile device emulator to receive updates
#  entered on the IVI HVAC screen (sent out by the IVI to
#  "ivi_[vin]"), while the IVI emulator receives updates entered on
#  the mobile device screen (sent out by the mobile device to
#  "mobile_[vin]".
#
#  When the emulator connects in IVI mode to a device RVI node, the
#  node's configured service prefix (see priv/setup_device.config,
#  node_service_prefix) will have the VIN number as a part of the
#  prefix. The emulator will thus register its service as /hvac/publish,
#  which, prefixed with the node service prefix, gives it a complete name
#  of jlr.com/vin/[vin]/hvac/publish.
#
#  In mobile device mode, the emulator connects to the backend RVI node, which
#  has a service prefix of jlr.com/backend. In this mode, the emulator
#  will register with a phone number as a part of its service name, where
#  the phone number is specified as a command line argument to the
#  emulator.
#
#  Since the backend RVI node has a service prefix of jlr.com/backend
#  (see priv/setup_backend.config), the mobile device emulator will
#  have a complete service name of:
#  jlr.com/backend/mobile/[phone_number]/hvac/publish
#
#  [phone_number] is given at the command line.
#
#
# Usage:
#
# python hvac_emulator.py <rvi_url> [mobile <phone_number> | ivi]
#
# mob
# The rvi_url is the local URL of the RVI Service Edge. 
# See 
#
# Example:
#  python hvac_emulator.pyh mobile 9491231234 http://127.0.0.1:8811
#
#  python hvac_emulator.pyh ivi saw222992212 http://127.0.0.1:8800
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
    print "Publish invoked!"
    print "vin:", vin 
    print "key:", key 
    print "value:", value 
    return ['ok']

def usage():
    print "Usage:", sys.argv[0], "<rvi_url> mobile <phone_number> <vin> | ivi"
    print "  ivi                           Emulate an IVI."
    print "  mobile <phone_number> <vin>   Emulate a mobile device with number given in"
    print "                                <phone_number> number, communicating with <vin>"
    print "  <rvi_url>                     URL of RVI Service Edge on local host"
    print
    print "The RVI Service Edge URL can be found in"
    print "priv/setup_[backend,device].config as"
    print "env -> rvi -> components -> service_edge -> url"
    print
    print "The RVI Service Edge URL can can also be specified"
    print "on the command line of the rvi_node.sh script."
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
if len(sys.argv) == 5:
    [ progname, rvi_url, mode, phone_number, vin ] = sys.argv    
    if mode != 'mobile':
        print 
        print "Second argument, when three parameters are specified,  must "
        print "be 'mobile', not", mode, "."
        print
        usage()

    # We are in mobile device mode. Setup the vin numbers for publish
    pub_vin = "mobile_"+vin
    sub_vin = "ivi_"+vin

    # Setup the service name we will register with.
    # The complete service name will be: jlr.com/backend/mobile/<phone_nr>/hvac/publish
    emulator_service_name = '/mobile/'+phone_number+'/hvac/publish'


    # Setup an outbound JSON-RPC connection to the RVI Service Edeg.
    rvi_server = jsonrpclib.Server(rvi_url)

    # Register our HVAC mobile emulator service with the RVI Service Edge,
    # allowing the RVI to forward requests to the service name to the
    # given network addresss (URL):
    print "Emulator service URL", emulator_service_url
    res = rvi_server.register_service(service = emulator_service_name, 
                                      network_address = emulator_service_url)

    full_emulator_service_name = res['service']

    print "Will run in mobile device mode."
    print "Backend server node URL:  ", rvi_url
    print "Phone Number:             ", phone_number
    print "VIN:                      ", vin
    print "Full Service Name:        ", full_emulator_service_name

elif len(sys.argv) == 3:
    [ progname, rvi_url, mode ] = sys.argv    
    if mode != 'ivi':
        print 
        print "Second argument, when two are specified,  must "
        print "be 'ivi', not", mode, "."
        print
        usage()

    # setup the service name we will register with
    # The complete service name will be: jlr.com/vin/<vin>/hvac/publish
    emulator_service_name = '/hvac/publish'

    # Setup an outbound JSON-RPC connection to the RVI Service Edeg.
    rvi_server = jsonrpclib.Server(rvi_url)

    # Register our HVAC IVI emulator service with the RVI Service Edge,
    # allowing the RVI to forward requests to the service name to the
    # given network addresss (URL):

    res = rvi_server.register_service(service = emulator_service_name, 
                                      network_address = emulator_service_url)

    # The returned full service name contains the VIN number that we want:
    #  jlr.com/vin/<vin>/hvac/publish
    # We need to dig out the <vin> bit

    full_emulator_service_name = res['service']

    [ t1, t2, vin, t3, t4] = full_emulator_service_name.split('/')
    print "vin:", vin,
    
    # We are in mobile device mode. Setup the vin numbers for publish
    pub_vin = "ivi_"+vin
    sub_vin = "mobile_"+vin

    print "Will run in IVI mode."
    print "Device node URL:  ", rvi_url
    print "VIN:              ", vin
else:
    usage()


# Regsiter self's service with the backend server RVI node
# See rvi_json_rpc_server.py._dispatch() for details on how
# ncoming JSON-RPC requests are mapped to local funcitons.
#
emulator_service = RVIJSONRPCServer(((emulator_service_host, emulator_service_port)))
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
