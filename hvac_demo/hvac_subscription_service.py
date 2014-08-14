#!/usr/bin/python
#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# This is an extremely simple subscription service that connects to
# a central backend RVI node. The node is well known by all
# other RVI nodes in a network, who has the backedn node configured
# as a "static" node. See priv/setup_device.config for an example.
#
# The subscription service will register the following services
# with the backend RVI node:
#
# /subscription_service/subscribe 
#   (Full name: jlr.com/backend/subscription_service/subscribe)
#   Parameters: 
#     vin - The VIN number for which updates are subscribed to.
#     subscribing_service - The service to send a message to when
#                           the given vin is updated.
# 
#   Adds the given service to the list of subscribers to notify
#  
#   When a message is recieved by the /subscribing_service/publish
#   service, all subscribers who have specified a vin 
#   matching that provided with the publish message will have
#   the message forwarded to them.
#
# /subscription_service/unsubscribe 
#   (Full name: jlr.com/backend/subscription_service/unsubscribe)
#   Parameters: 
#     vin - The VIN number from which the service unsubscribes.
#     subscribing_service - The service that unsubscribes.,
# 
#   Removes a service, previously subscribing to the service through
#   a subscribe command, from the given vin. Future updates
#   to the vin will not be forwarded to the given service.
#
# /subscription_service/publish 
#   (Full name: jlr.com/backend/subscription_service/publish)
#   Parameters:
#     vin - The VIN number from which the service unsubscribes.
#     key - The key that has an updated value
#     value - The new value assigned to key
#   
#   Distributes the key/value pair to all services that have previosuly
#   subscribed to updates for the given vin.
#
# For RVI Milestone 1, the mobile and Tizen IVI UI remains to be
# integrated with the RVI system itself. Meanwhile, hvac_emulator.py
# is provded as a simple tester that can emulate either an IVI or
# a mobile device.
#  

from jsonrpclib.SimpleJSONRPCServer import SimpleJSONRPCServer
import jsonrpclib
from rvi_json_rpc_server import RVIJSONRPCServer
import sys
RVI_SERVICE_EDGE='http://localhost:8801'
HVAC_SERVER=('localhost', 8901)

# vin_subs is a simple dictionary with vin numbers as keys and arrays
# of services as values.
# During a subscribe operation, the provided service is added
# as a list element in value, where the list hangs under the key
# with value 'vin'
#
# Thus:
# { '1234': [ 'jlr.com/vin/5555/hvac_update_ui', 'jlr.com/vin/4711/hvac_update_ui'] 
vin_subs = {}        

def subscribe(vin, subscribing_service):
    print
    print "Got subscription"
    print "vin:", vin 
    print "subscribing_service:", subscribing_service 
    print 
    # Delete any existing service with the same name
    # Add the subscribing service
    if not vin in vin_subs:
        vin_subs[vin] = [subscribing_service]
    else:
        if vin_subs[vin].count(subscribing_service) == 0:
            vin_subs[vin].append(subscribing_service)

    return ['ok']


def unsubscribe(vin, subscribing_service):
    print
    print "Got unsubscribe"
    print "vin:", vin 
    print "subscribing_service:", subscribing_service 
    print
    # Delete any existing service with the same name
    
    if vin in vin_subs and vin_subs[vin].count(subscribing_service) > 0:
        vin_subs[vin].remove(subscribing_service)

    return ['ok']

def publish(vin, key, value):
    print
    print "Got publish"
    print "Publish"
    print "vin:", vin 
    print "key:", key 
    print "value:", value 

    # Distribute
    try:
        subs = vin_subs[vin]
    except Err:
        print "No subscribers for vin:", vin
        return ['ok']
            
    for sub in subs:
        print "Sending publish to", sub
        rvi_server.message(calling_service = '/hvac/publish', 
                           target = sub,
                           timeout = 0,
                           parameters = [{ u'vin': vin, u'key': key}, {u'value': value }])
    print

if len(sys.argv) == 2:
    [ progname, rvi_url ] = sys.argv    
else:
    print "Usage:", sys.argv[0], "<rvi_url>"
    print "  <rvi_url>                     URL of RVI Service Edge on local host"
    print
    print "The RVI Service Edge URL can be found in"
    print "priv/setup_[backend,ivi].config as"
    print "env -> rvi -> components -> service_edge -> url"
    print
    print "The Service Edge URL is also logged as a notice when the"
    print "RVI node is started."
    sys.exit(255)

print "Will register with RVI at", rvi_url, "using", HVAC_SERVER, "as my own URL."

# Setup self's server that we will receive incoming calls from service edge on.
hvac_server = RVIJSONRPCServer(HVAC_SERVER, logRequests=False)

hvac_server.register_function(subscribe, '/subscription_service/subscribe')
hvac_server.register_function(unsubscribe, '/subscription_service/unsubscribe')
hvac_server.register_function(publish, '/subscription_service/publish')


# Register our services with the service edge and have
# it associate our service name with the URL that the hvac_server is listening on
rvi_server = jsonrpclib.Server(rvi_url)

res = rvi_server.register_service(service = '/subscription_service/subscribe', 
                                  network_address = 'http://localhost:8901')

print "Registered service", res['service']

res = rvi_server.register_service(service = '/subscription_service/unsubscribe', 
                                  network_address = 'http://localhost:8901')

print "Registered service", res['service']

res = rvi_server.register_service(service = '/subscription_service/publish', 
                                  network_address = 'http://localhost:8901')

print "Registered service", res['service']


hvac_server.serve_forever()
