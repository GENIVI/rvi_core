#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#
# rbilib.py 0.3.0
#  
# This moduke
from jsonrpclib.SimpleJSONRPCServer import SimpleJSONRPCServer
from jsonrpclib import Server
import random
import time
import threading

class RVI(SimpleJSONRPCServer):
    # address is either localhost or the ip address of self.
    # 0.0.0.0 should work to listen to all addresses, but haven't been tested
    # Port is either a fixed port (integer) or a tuple with a start and stop
    # port that should be used as a random interval to pick the port from
    def __init__(self, rvi_node_url, address='localhost', port=(20001,59999)):
        if isinstance(port, tuple) == True:
            (start_port, stop_port) = port
            self.rvi_port = random.randint(start_port, stop_port)
        else:
            self.rvi_port = port

        self.rvi_address = address
        print self.rvi_address
        print self.rvi_port
        SimpleJSONRPCServer.__init__(self,addr=((self.rvi_address, self.rvi_port)), logRequests=False)
        self.rvi_client = Server(rvi_node_url)
        self.serve_thread = False
        
    # Set the callback to invoke when RVI reports that one or more new 
    # services are available for invocation somewhere in the network.
    # Arguments will be an array of fully qualified service available.
    def set_services_available_callback(self, function):
        self.register_function(function, 'services_available')
        
    # Set the callback to invoke when RVI reports that one or more previously
    # available services are no longer available for invocation
    #
    # Arguments will be an array of fully qualified service names no longer available.
    def set_services_unavailable_callback(self, function):
        self.register_function(function, 'services_unavailable')
        
    # Register a service in this python program that should
    # be reached from the rest of the network.
    #
    # service_name is the local service name ('/my_stuff/set_fan_speed')
    #
    # function is a python function reference to invoke when an
    # JSON-RPC call for service_nameis received from RVI.
    #
    # This function will return the fully qualified name of the registered service.
    #
    # Thus, if you call:
    #
    #    register_service('/my_stuff/set_fan_speed', set_fan_speed)
    #
    # you will get back the fully qualified service name assigned:
    #
    #    "jlr.com/vin/YV1TS32G54F123456/my_stuff/set_fan_speed"
    #
    # This is the globally accessible name for the service that can be used anywhere
    # in an RVI network to access your service.
    #
    def register_service(self, service_name, function):
        # Register service_name within SimpleJSONRPCServer so that
        # when it gets invoked with the given URL suffic, it will call 'function'.
        #
        # I.e. when self.url() + service_name gets called by the RVI node, we
        # will dispatch that call to 'function'.
        #
        print "Will register {} to function {}".format(service_name, function)
        self.register_function(function, service_name)

        # Register the service name with the RVI node so that we get called
        # at the URL we are listening on (self.url()).
        #
        res = self.rvi_client.register_service(service=service_name, network_address=self.url())
        
        # Check if we have a thread going
        if self.serve_thread == False:
            print "No thread started, will do so now"
            self.start_serve_thread()
            

        # Return the fully qualified service name
        return res['service']

    def start_serve_thread(self):
        self.serve_thread = threading.Thread(target=self.serve_forever)
        self.serve_thread.start()

    #
    # Return the URL that we've setup for incoming JSON-RPC calls
    # from the RVI node.
    #
    def url(self):
        return 'http://' + self.rvi_address + ':' + str(self.rvi_port)
        
    
    #
    # Send a message to the RVI node to be forwarded to the
    # service that registered 'service_name' in the network.
    #
    def message(self, service_name, parameters, timeout = int(time.time()) + 60 ):
        print "message({}, {}, {})".format(service_name, parameters, timeout)
        self.rvi_client.message(calling_service= "not_used",
                                     service_name = service_name,
                                     timeout = timeout,
                                     parameters = parameters)
        
    #
    # Check if method is 'message', if so dispatch on
    # name 'service_name' instead.
    #
    def _dispatch(self, method, params):
        if method == 'message':
            print "Will dispatch message to: " + params['service_name']
            dict_param = {}
            # Extract the 'parameters' element from the top level JSON-RPC
            # 'param'. 
            # Convert 'parameters' from [{'vin': 1234}, {hello: 'world'}] to
            # a regular dictionary: {'vin': 1234, hello: 'world'}

            # print "Parameters:", params['parameters']
            msg_params = params['parameters'] 
            for i in range(0, len(msg_params)):
                for j in range(0, len(msg_params[i].keys())):
                    print "params", msg_params[i].keys()[j], "=", msg_params[i].values()[j]
                    dict_param[msg_params[i].keys()[j]] = msg_params[i].values()[j]

            # print "Parameter disctionary: ", dict_param
            # print 
            # Ship the processed dispatch info upward.
            return SimpleJSONRPCServer._dispatch(self, params['service_name'], dict_param)           

        # Fallthrough to all other methods.
        # Will handle service_re3
        return SimpleJSONRPCServer._dispatch(self,method, params)           
