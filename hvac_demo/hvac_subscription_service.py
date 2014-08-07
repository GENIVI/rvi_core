#!/usr/bin/python

from jsonrpclib.SimpleJSONRPCServer import SimpleJSONRPCServer
import jsonrpclib
from rvi_json_rpc_server import RVIJSONRPCServer

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
vin_subs = []        

def subscribe(vin, subscribing_service):
    print "Subscribe"
    print "vin:", vin 
    print "subscribing_service:", subscribing_service 

    # Delete any existing service with the same name
    try:
        vin_subs[vin].remove(subscribing_service)
    except Err:
        pass

    # Add the subscribing service
    vin_subs[vin].append(subscribing_service)
    return ['ok']


def unsubscribe(vin, subscribing_service):
    print "Unsubscribe"
    print "vin:", vin 
    print "subscribing_service:", subscribing_service 

    # Delete any existing service with the same name
    try:
        vin_subs[vin].remove(subscribing_service)
    except Err:
        pass

    return ['ok']

def publish(vin, key, value):
    print "Publish"
    print "vin:", vin 
    print "key:", key 
    print "value:", value 

    # Delete any existing service with the same name
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
                           parameters = [{ u'key': key}, {u'value': value }])


# Setup self's server that we will receive incoming calls from service edge on.
hvac_server = RVIJSONRPCServer(HVAC_SERVER)
# hvac_server = SimpleJSONRPCServer(HVAC_SERVER)

hvac_server.register_function(subscribe, '/hvac/subscriber_service/subscribe')
hvac_server.register_function(unsubscribe, '/hvac/subscriber_service/unsubscribe')
hvac_server.register_function(publish, '/hvac/subscriber_service/publish')


# Register our services with the service edge and have
# it associate our service name with the URL that the hvac_server is listening on
rvi_server = jsonrpclib.Server(RVI_SERVICE_EDGE)

res = rvi_server.register_service(service = '/subscription_service/subscribe', 
                                  network_address = 'http://localhost:8901')

print "Registered service /subscription_service/subscribe as", res['service']

res = rvi_server.register_service(service = '/subscription_service/unsubscribe', 
                                  network_address = 'http://localhost:8901')

print "Registered service /subscription_service/unsubscribe as", res['service']

res = rvi_server.register_service(service = '/subscription_service/publish', 
                                  network_address = 'http://localhost:8901')

print "Registered service /subscription_service/publish as", res['service']


hvac_server.serve_forever()
