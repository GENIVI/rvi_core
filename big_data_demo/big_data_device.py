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
from gps import *
import json
from rvi_json_rpc_server import RVIJSONRPCServer
import jsonrpclib
from urlparse import urlparse


MY_NAME = "GPS Collector"

class RVICallbackServer(threading.Thread):
    """
    RPC server thread responding to incoming callbacks from the RVI framework
    """
    
    def __init__(self, service_edge, callback_url):
        self.service_edge = service_edge
        self.callback_url = callback_url
        threading.Thread.__init__(self)
        url = urlparse(self.callback_url)
        self.localServer =  RVIJSONRPCServer(addr=((url.hostname, url.port)), logRequests=False)
        self.register_services()
        
    def register_services(self):
        # register callback functions with RPC server
        self.localServer.register_function(subscribe, "/logging/subscribe" )
        self.localServer.register_function(unsubscribe, "/logging/unsubscribe" )
       
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
            except:
                print "No RVI. Wait and retry..."
                time.sleep(2.0)
        print 'Service registration successful. Services: ', services

    def run(self):
        self.localServer.serve_forever()
        
    def shutdown(self):
        self.localServer.shutdown()

 
class GPSPoller(threading.Thread):
    """
    Polls GPS devices via gpsd
    """
    
    def __init__(self):
        threading.Thread.__init__(self)
        self.session = gps(mode=WATCH_ENABLE)
        
    def shutdown(self):
        self._Thread__stop()
        
    def run(self):
        while True:
            self.session.next()


class GPSCollector:
    """
    """
    
    def __init__(self, destination, vin, interval, nofix=False):
        self.vin = vin
        self.interval = interval
        self.nofix = nofix
        self.last_speed = 1.0
        self.gps_poller = GPSPoller()
        self.destination = destination
        self.transaction_id = 1

    
    def run(self):
        # start GPS polling thread
        self.gps_poller.start()

        # main execution loop
        while True:
            try:
                time.sleep(self.interval)
                
                # process GPS data
                session = self.gps_poller.session
                if (session.fix.mode == MODE_NO_FIX) and not self.nofix:
                    print "Waiting for GPS to fix..."
                    continue

                #time = session.utc
                # location.loc_latitude = session.fix.latitude
                #location.loc_longitude = session.fix.longitude
                #location.loc_altitude = session.fix.altitude
                #location.loc_speed = session.fix.speed
                #location.loc_climb = session.fix.climb
                #location.loc_track = session.fix.track
                
                if not isnan(session.fix.time):
                    if (session.fix.speed < 0.1) and (self.last_speed < 0.1):
                        print "Waiting for speed..."
                        continue

                    self.last_speed = session.fix.speed
                    # if the time is valid the data record is valid

                    print "Location:", session
                    rvi_server.message(calling_service = "/big_data",
                                   service_name = self.destination + "/logging/report",
                                   transaction_id = self.transaction_id,
                                   timeout = int(time.time())+60,
                                   parameters = [{ 
                                       u'vin': self.vin,
                                       u'timestamp': session.utc,
                                       u'data': [
                                           { u'channel': 'waypoint', 
                                             u'value': { 
                                                 u'lat': session.fix.latitude,
                                                 u'lon': session.fix.longitude,
                                                 u'alt': session.fix.altitude
                                              }
                                           },
                                           { u'channel': 'speed',
                                             u'value':   session.fix.speed
                                           },
                                       ]
                                   }])
                    self.transaction_id += 1

                else:
                    print "Invalid location:", session
                    

            except KeyboardInterrupt:
                print ('\n')
                break
            

    def shutdown(self):
        if self.gps_poller:
            self.gps_poller.shutdown()


def subscribe(channels, interval):
    print "subscribe(): channels:", channels
    print "subscribe(): interval:", interval
    return {u'status': 0}


def unsubscribe(channels):
    print "unsubscribe(): channels:", channels
    return {u'status': 0}


def cleanup(*args):
    print "Caught signal:", args[0], "Shutting down..."
    if gps_collector:
        gps_collector.shutdown()
    if rvi_callback:
        rvi_callback.shutdown()
    sys.exit(0)


def usage():
    print "Usage: %s RVI-URL VIN" % sys.argv[0]
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
    if len(sys.argv) != 3:
        usage()

    # Grab the URL to use
    [ progname, rvi_url, vin ] = sys.argv   

    # Welcome message
    print "RVI Big Data Device"
    print "Outbound URL to RVI:  ", rvi_url
    print "Inbound URL from RVI: ", service_url

    # Setup outbound JSON-RPC connection to the RVI Service Edge
    rvi_server = jsonrpclib.Server(rvi_url)
    
    # Setup inbound JSON-RPC server
    rvi_callback = RVICallbackServer(rvi_server, service_url)
    rvi_callback.start()

    # catch signals for proper shutdown
    for sig in (SIGABRT, SIGTERM, SIGINT):
        signal(sig, cleanup)

    # Start GPS data collection
    interval = 5
    gps_collector = None
    nofix = False

    gps_collector = GPSCollector("jlr.com/backend", vin, interval, nofix)

    # Let the main thread run the gps collector
    gps_collector.run()
            


