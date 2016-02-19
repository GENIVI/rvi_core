#
# Copyright (C) 2016, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#
# rvi_ws 0.1.0
#  
# This module is the websocket implementation of for RVI services. 

# To use this code you will follow these steps.
# 1. Import the rvi_ws class using "import rvi_ws"
# 2. Instantiate a rvi_ws_client class object specifying the bundle_id, debug params, and host
#     ex. 
#         rvi_client = rvi_ws.rvi_ws_client(bundle_id = <string bundle name>, host = <string host url>, debug = <True|False>)
# 3. Create a dictionary of services and their callback functions. The keys of the dictionary should be the string service name
#    name and the value should be the pointer to the callback function.
#    ex.
#         def hello(...):
#             ...
#         def world(...):
#             ...
#
#         rvi_client.register_services({"hello":hello, "world":world})
#
# 4. Register and run the services by invoking the services_run() function in the rvi_ws_client class. This will block all other code
#    execution until the websocket connection closes in which case will return a None value.
#    ex.
#         rvi_client.services_run()
#
# Optionally there are a few more utility functions included in the rvi_ws_client class mainly setting properties of the class.
#

import websocket
import json
import threading

try:
    import thread
# TODO use Threading instead of _thread in python3
except ImportError:
    import _thread as thread

# rvi_ws_client will be in charge of handling all communication via websockets between the service bundle and RVI.

class rvi_ws_client:

    def __init__(self, bundle_id = None, debug = False, host = "ws://localhost:9008"):

        self.DEBUG = debug
        self.service_bundle_id = bundle_id
        self.callback_funcs = {}
        self.host = host

    # set_ws_debug takes in parameter debug_status which is type bool. Will toggle on or off all websocket related debug messages.    
    def set_ws_debug(self):

        if self.DEBUG:
            websocket.enableTrace(True)
        else:
            websocket.enableTrace(False)

    # on_error will print an error if the websocket application encounters any and prints if debug is toggled on
    def on_error(self,ws, error):
        pass

    # TODO unregister service for clean close of websocket, for time being will just print out debug
    def on_close(self,ws):
        pass
    # What to do on the open of the application. Note we must register_services for this to do anything.
    def on_open(self,ws):

        def run(*args):

            payload = {}
            payload['json-rpc'] = "2.0"
            payload['id'] = "0"
            payload['method'] = "register_service"

            for service_name, callback in self.callback_funcs.items():
                payload['params'] = {"service_name":self.service_bundle_id+"/"+service_name}        
                ws.send(json.dumps(payload))

        opening = threading.Thread(target=run)
        opening.start()

    # on_message will route the message from the websocket to it's corresponding callback function that registered the service
    def on_message(self,ws, message):

        message_dict = json.loads(message)

        try:
            if (message_dict['method'] == 'message') and (message_dict['params']['service_name'][(2+len(self.service_bundle_id)):] in self.callback_funcs):
                self.callback_funcs[message_dict['params']['service_name'][(2+len(self.service_bundle_id)):]](**message_dict['params']['parameters'])
        except:
            pass


    # set_host will expect a string parameter that will change the class variable host which will connect to our websocket server
    def set_host(self,target_host):

        self.host = target_host
        return True

    # register_services will take in a dictionary of services to register. 
    # The keys of the dictionary will be the service name and the value will be the callback function
    # will return success on successfully changing the class' callback function dictionary
    # Please make sure that the service name keys are strings.
    # For example:
    #     services = {
    #         <service_name>:<callback function>,
    #         <service_name>:<callback function>,
    #         ....
    #     }
    def register_services(self,services):

        self.callback_funcs = services
        return True
    # set_service_bundle expects a string parameter that will change the class variable service_bundle_id as the bundle_id tro register
    def set_service_bundle(self, service_bundle):
        self.service_bundle_id = service_bundle
        return True

    # services_run is a callable function for after everything is set to start the websocket client.
    # Will block all remaining code until the websocket connection is broken. In which case will return None 
    def services_run(self):

        if self.service_bundle_id == None:
            raise NameError('No Specified bundle_id')

        self.set_ws_debug()

        ws = websocket.WebSocketApp(self.host,
                                    on_message = self.on_message,
                                    on_error = self.on_error,
                                    on_close = self.on_close)
        ws.on_open = self.on_open

        return ws.run_forever()

