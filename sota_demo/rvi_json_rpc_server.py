#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#
from jsonrpclib.SimpleJSONRPCServer import SimpleJSONRPCServer
import jsonrpclib

class RVIJSONRPCServer(SimpleJSONRPCServer):
    # Check if method is 'message', if so dispatch on
    # name 'service_name' instead.
    def _dispatch(self, method, params):
        # print "dispatch:", params
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


        return SimpleJSONRPCServer._dispatch(self,message, params)           
