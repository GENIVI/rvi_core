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
# 2. Instantiate a rvi_ws_client class object specifying the bundle_id, debug params, host, and services.
#    services will be a dictionary where the key will be the service_name to register and value will be the service's callback function
#     ex. 
#         rvi_client = rvi_ws.rvi_ws_client(host = <string host url>, bundle_id = <string bundle name>,  services = <dict of callbacks> [,debug = <True|False>])
#
# 3. Register and run the services by invoking the services_run() function in the rvi_ws_client class. This will block all other code
#    execution until the websocket connection closes in which case will return a None value.
#    ex.
#         rvi_client.run_forever()
#

import websocket
import json

# rvi_ws_client will be in charge of handling all communication via websockets between the service bundle and RVI.

class rvi_ws_client(websocket.WebSocketApp):

    def __init__(self, host, bundle_id, services, debug = False):
        super(rvi_ws_client, self).__init__(host)

        self.host = host
        self.DEBUG = debug
        self.service_bundle_id = bundle_id
        self.callback_funcs = services
        self.reg_services = []

        self.on_close = self._on_close
        self.on_open = self._on_open
        self.on_message = self._on_message

        websocket.enableTrace(debug)

        if self.service_bundle_id == None or len(self.service_bundle_id) <= 0:
            raise NameError('rvi_ws_client - No Specified bundle_id')
        if len(self.callback_funcs) <= 0:
            raise EnvironmentError('rvi_ws_client - No services to register')

    # Unregister service for clean close of websocket, for time being will just print out debug
    def _on_close(self, ws):
        unreg_ws = websocket.create_connection(self.host)
        payload = {}
        payload['json-rpc'] = "2.0"
        payload['id'] = "0"
        payload['method'] = "unregister_service"
        
        for unsub in self.reg_services:
            payload['params'] = {
                                'service_name' : unsub
            }
            unreg_ws.send(json.dumps(payload))

    # What to do on the open of the application. Note we must register_services for this to do anything.
    def _on_open(self, ws):

        payload = {}
        payload['json-rpc'] = "2.0"
        payload['id'] = "0"
        payload['method'] = "register_service"

        for service_name, callback in self.callback_funcs.items():
            payload['params'] = {"service_name":self.service_bundle_id+"/"+service_name}        
            self.send(json.dumps(payload))

    # on_message will route the message from the websocket to it's corresponding callback function that registered the service
    def _on_message(self, ws, message):

        message_dict = json.loads(message)

        try:
            if (message_dict['method'] == 'message') and (message_dict['params']['service_name'][(2+len(self.service_bundle_id)):] in self.callback_funcs):
                self.callback_funcs[message_dict['params']['service_name'][(2+len(self.service_bundle_id)):]](**message_dict['params']['parameters'])
            elif message_dict['method'] == 'register_service':
                self.reg_services.append(message_dict['service'])

        except:
            pass

def get_available_services(host):
    ws = websocket.create_connection(host)
    payload = {'json-rpc':'2.0', 'id':0, 'method':'get_available_services', 'params':[]}
    ws.send(json.dumps(payload))
    return json.loads(ws.recv())['services']
