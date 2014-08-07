from jsonrpclib.SimpleJSONRPCServer import SimpleJSONRPCServer
import jsonrpclib

class RVIJSONRPCServer(SimpleJSONRPCServer):
    # Check if method is 'message', if so dispatch on
    # name 'target' instead.
    def _dispatch(self, method, params):
        print "dispatch:", params
        if method == 'message':
            print "Will dispatch message to: " + params['target']
            dict_param = {}
            # Extract the 'parameters' element from the top level JSON-RPC
            # 'param'. 
            # Convert 'parameters' from [{'vin': 1234}, {hello: 'world'}] to
            # a regular dictionary: {'vin': 1234, hello: 'world'}

            print params['parameters']
            msg_params = params['parameters'] 
            for i in range(0, len(msg_params)):
                print "params ", msg_params[i].keys()[0], " = ", msg_params[i].values()[0]
                dict_param[msg_params[i].keys()[0]] = msg_params[i].values()[0]

            print "DICT: ", dict_param
            # Ship the processed dispatch info upward.
            return SimpleJSONRPCServer._dispatch(self, params['target'], dict_param)           

        print "Method:",  method
        for x in params:
            print "params ", x, " = ",params[x]
            return SimpleJSONRPCServer._dispatch(self,message, params)           

        print "---"

