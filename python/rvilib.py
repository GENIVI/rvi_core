#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#
# rbilib.py 0.3.2
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
        SimpleJSONRPCServer.__init__(self,addr=((self.rvi_address, self.rvi_port)), logRequests=False)
        self.rvi_client = Server(rvi_node_url)
        self.serve_thread = False
        self.registered_services = dict()
        
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
        
    def get_available_services(self):
        # We need at least one dummy argument for the RPC to go
        # through on the RVI side.
        res = self.rvi_client.get_available_services(1)
        return res['services']

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
        # Add a prefixing slash if necessary
        if service_name[0] != '/':
            service_name = '/' + service_name

        # Register service_name within SimpleJSONRPCServer so that
        # when it gets invoked with the given URL suffic, it will call 'function'.
        #
        # I.e. when self.url() + service_name gets called by the RVI node, we
        # will dispatch that call to 'function'.
        #
        self.register_function(function, service_name)

        # Register the service name with the RVI node so that we get called
        # at the URL we are listening on (self.url()).
        #
        res = self.rvi_client.register_service(service=service_name, network_address=self.url())
        
        # Check if we have a thread going
        if self.serve_thread == False:
            self.start_serve_thread()
            
        full_service_name = res['service']
        self.registered_services[service_name] = full_service_name

        # Return the fully qualified service name
        return res['service']

    # Unregister a service 
    #  Unregisters a service previously registered with register_service
    #  The provided 'service_name' is identical to that provided tgo
    #  retister_service()
    #
    def unregister_service(self, service_name):

        if service_name[0] != '/':
            service_name = '/' + service_name

        # Check that the service has been previously registered
        # If not just return
        if service_name not in self.registered_services:
            return False
            

        # Retrieve the fully qualified service name that
        # we need to unregister from rvi
        full_service_name = self.registered_services[service_name]

        # Delete dictionary entry
        del self.registered_services[service_name]

        #
        # Unregister the service from RVI
        #
        res = self.rvi_client.unregister_service(service=full_service_name)
        
        return True

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
        self.rvi_client.message(service_name = service_name,
                                timeout = timeout,
                                parameters = parameters)
        

    #
    # Redefined shutdown method that first unregisters all services.
    #
    def shutdown(self):
        shutdown_list = self.registered_services.keys()
        for svc in shutdown_list:
            self.unregister_service(svc)

        SimpleJSONRPCServer.shutdown(self)

    #
    # Check if method is 'message', if so dispatch on
    # name 'service_name' instead.
    #
    def _dispatch(self, method, params):
        if method == 'message':
            # print "Will dispatch message to: " + params['service_name']
            dict_param = {}
            # Extract the 'parameters' element from the top level JSON-RPC
            # 'param'. 
            # Convert 'parameters' from [{'vin': 1234}, {hello: 'world'}] to
            # a regular dictionary: {'vin': 1234, hello: 'world'}

            # print "Parameters:", params['parameters']
            msg_params = params['parameters'] 
            for i in range(0, len(msg_params)):
                for j in range(0, len(msg_params[i].keys())):
                    # print "params", msg_params[i].keys()[j], "=", msg_params[i].values()[j]
                    dict_param[msg_params[i].keys()[j]] = msg_params[i].values()[j]

            # print "Parameter disctionary: ", dict_param
            # print 
            # Ship the processed dispatch info upward.
            return SimpleJSONRPCServer._dispatch(self, params['service_name'], dict_param)           

        # Fallthrough to all other methods.
        # Will handle service_re3
        return SimpleJSONRPCServer._dispatch(self,method, params)           
