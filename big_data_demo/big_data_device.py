#!/usr/bin/python

#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# GPS Data Collector
# A sample device implementation that collects GPS position data from gpsd
# and sends it to an RVI backend.
#

import sys
import getopt
import os
import time
import threading
import random 
from signal import *
import json
from rvi_json_rpc_server import RVIJSONRPCServer
import jsonrpclib
from urlparse import urlparse
import amb_dbus
import data_logger
import gps_collector 
import traceback
MY_NAME = "Big Data Demo"
    
class RVICallbackServer(threading.Thread):
    """
    RPC server thread responding to incoming callbacks from the RVI framework
    """
    
    def __init__(self, amb, logger, service_edge, callback_url):
        threading.Thread.__init__(self)
        self.logger = logger
        self.service_edge = service_edge
        self.callback_url = callback_url
        url = urlparse(self.callback_url)
        self.localServer =  RVIJSONRPCServer(addr=((url.hostname, url.port)), logRequests=False)
        self.register_services()
        self.amb = amb
        
    def subscribe(self, channels, report_interval):
        for channel in channels:
            self.amb.subscribe(channel, int(report_interval))
            self.logger.add_subscription(channel, int(report_interval))
            
        self.logger.dump_db()
        return {u'status': 0}


    def unsubscribe(self, channels):
        print "unsubscribe(): channels:", channels
        for channel in channels:
            self.logger.delete_subscription(channel)

        return {u'status': 0}

    def register_services(self):
        # register callback functions with RPC server
        self.localServer.register_function(self.subscribe, "/logging/subscribe" )
        self.localServer.register_function(self.unsubscribe, "/logging/unsubscribe" )
       
        # register services with RVI framework
        rvi_dead = True
        services = []
        while rvi_dead:
            try: 
                res = self.service_edge.register_service(service = "/logging/subscribe",
                                                  network_address = self.callback_url)

                services.append(res['service'])
                res = self.service_edge.register_service(service = "/logging/unsubscribe",
                                                  network_address = self.callback_url)
                services.append(res['service'])
                rvi_dead = False
            except Exception, err:
                print "No RVI. Wait and retry..."
                time.sleep(2.0)

        print 'Service registration successful. Services: ', services
        # Retrieve the Vin number from the returned service name
        vin = services[0]
        vin = vin[0:len(vin)-18]
        vin = vin[vin.rindex('/')+1:]
        print "Retrieved VIN:", vin
        self.vin = vin

    def run(self):
        self.localServer.serve_forever()
        
    def shutdown(self):
        self.localServer.shutdown()


 

class DataSender(threading.Thread):
    """
    Sends data from the database to RVI
    """
    
    def __init__(self, destination, vin, rvi_server, logger, send_interval):
        threading.Thread.__init__(self)
        self.destination = destination
        self.vin = vin
        self.rvi_server = rvi_server
        self.logger = logger
        self.send_interval = send_interval
        self.transaction_id = 1

    def shutdown(self):
        self._Thread__stop()
        
    def run(self):
        while True:
            sample = logger.retrieve_next_sample()

            # If no samples are to be had, sleep on it and try again.
            if sample == False:
                print "No samples"
                time.sleep(self.send_interval)
                continue

            ( timestamp, values ) = sample
            print "Sending timestamp: {} - values {}".format(timestamp, values)
            rvi_server.message(calling_service = "/big_data",
                               service_name = self.destination + "/logging/report",
                               transaction_id = self.transaction_id,
                               timeout = int(time.time())+60,
                               parameters = [{ 
                                   u'vin': self.vin,
                                   u'timestamp': timestamp,
                                   u'data': values
                               }])
            # Wipe sample now that we have sent it
            logger.delete_sample(timestamp)
            self.transaction_id += 1
            


def cleanup(*args):
    print "Caught signal:", args[0], "Shutting down..."
    if gps_collector:
        gps_collector.shutdown()
    if rvi_callback:
        rvi_callback.shutdown()
    if logger:
        logger.shutdown()
    if data_sender:
        data_sender.shutdown()
    sys.exit(0)


def usage():
    print "Usage: %s RVI-URL" % sys.argv[0]
    sys.exit(255)

        
if __name__ == "__main__":
    #
    # Setup a localhost URL, using a random port, that we will listen to
    # incoming JSON-RPC publish calls on, delivered by our RVI service
    # edge (specified by rvi_url).
    #
    service_host = 'localhost'
    service_port = random.randint(20001, 59999)
    service_url = 'http://'+service_host + ':' + str(service_port)

    # 
    # Check that we have the correct arguments
    #
    if len(sys.argv) != 2:
        usage()

    # Grab the URL to use
    [ progname, rvi_url ] = sys.argv   

    # Welcome message
    print "RVI Big Data Device"
    print "Outbound URL to RVI:  ", rvi_url
    print "Inbound URL from RVI: ", service_url


    # Setip the logger.
    logger = data_logger.Logger()
    logger.start()
    
    
    # Setup outbound JSON-RPC connection to the RVI Service Edge
    rvi_server = jsonrpclib.Server(rvi_url)
    print "SERVER:", rvi_server

    # Setup AMB DBUS integartion
    amb = amb_dbus.DBUSMonitor(logger)
    amb.start()

    # Setup inbound JSON-RPC server
    rvi_callback = RVICallbackServer(amb, logger, rvi_server, service_url)
    rvi_callback.start()

    # Setup data sender
    data_sender = DataSender("jlr.com/backend", rvi_callback.vin, rvi_url, logger, 3)
    data_sender.start()

    # Retrieve (persistent) subscriptions from
    # the logger and prime the amb dbus 
    # connection with it.
    for (channel, interval) in logger.get_subscriptions():
        amb.subscribe(channel, interval)

    # Start GPS data collection
    interval = 1
    gps_collector = gps_collector.GPSCollector(logger)

    # catch signals for proper shutdown
    for sig in (SIGABRT, SIGTERM, SIGINT):
        signal(sig, cleanup)

    # Let the main thread run the gps collector
    gps_collector.run()

    print "gps_collector.run() exited."
