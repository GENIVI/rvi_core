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
# This tool collects data from the gpsd daemon and stores it in the RVI Backend
# database using the Django ORM.
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


MY_NAME = "GPS Collector"

class GPSPoller(threading.Thread):
    
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
    
    def __init__(self, vin, interval, nofix=False):
        self.vin = vin
        self.interval = interval
        self.nofix = nofix
        self.last_speed = 1.0
        self.gps_poller = GPSPoller()

    def run(self):
        # start GPS polling thread
        self.gps_poller.start()

        # catch signals for proper shutdown
        for sig in (SIGABRT, SIGTERM, SIGINT):
            signal(sig, self.cleanup)

        # main execution loop
        while True:
            try:
                time.sleep(self.interval)
                
                # process GPS data
                session = self.gps_poller.session
                if (session.status == STATUS_NO_FIX) and not self.nofix:
                    print "Waiting for GPS to fix..."
                    continue
                
                if not isnan(session.fix.time):
                    if (session.fix.speed < 0.1) and (self.last_speed < 0.1):
                        print "Waiting for speed..."
                        # continue

                    self.last_speed = session.fix.speed
                    # if the time is valid the data record is valid

                    logger.info("%s: Valid location: %s", MY_NAME, location)
                else:
                    logger.debug("%s: Invalid location: %s", MY_NAME)
                    

            except KeyboardInterrupt:
                print ('\n')
                break

    def cleanup(self, *args):
        logger.info('%s: Caught signal: %d. Shutting down...', MY_NAME, args[0])
        if self.gps_poller:
            self.gps_poller.shutdown()
        sys.exit(0)

def subscribe(channels, interval):
    print "subscribe(): channels:", channels
    print "subscribe(): interval:", interval
    return {u'status': 0}


def unsubscribe(channels, interval):
    print "unsubscribe(): channels:", channels
    return {u'status': 0}


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


    # Setup an outbound JSON-RPC connection to the RVI Service Edeg.
    rvi_server = jsonrpclib.Server(rvi_url)

    service = RVIJSONRPCServer(addr=((service_host, service_port)), logRequests=False)

    #
    # Regsiter callbacks for incoming JSON-RPC calls delivered to
    # this program
    #
        
    service.register_function(subscribe, "/logging/subscribe" )
    service.register_function(subscribe, "/logging/unsubscribe" )

    # Create a thread to handle incoming stuff so that we can do input
    # in order to get new values
    thr = threading.Thread(target=service.serve_forever)
    thr.start()


    # We may see traffic immediately from the RVI node when
    # we register. Let's sleep for a bit to allow the emulator service
    # thread to get up to speed.
    time.sleep(0.5)

    #
    # Register our HVAC emulator service with the vehicle RVI node's Service Edge.
    # We register both services using our own URL as a callback.
    #

    # Repeat registration until we succeeed
    rvi_dead = True
    while rvi_dead:
        try: 
            res = rvi_server.register_service(service = "/logging/subscribe",
                                              network_address = service_url)
            rvi_dead = False
        except:
            print "No rvi. Wait and retry"
            time.sleep(2.0)


    full_subscribe_name = res['service']

    res = rvi_server.register_service(service = "/logging/unsubscribe", network_address = service_url)
    full_unsubscribe_name = res['service']

    res = rvi_server.register_service(service = "/sota/finish", network_address = service_url)
    full_finish_service_name = res['service']

    print "HVAC Emulator."
    print "Vehicle RVI node URL:       ", rvi_url
    print "Emulator URL:               ", service_url
    print "Full subscribe service name :  ", full_subscribe_name
    print "Full unsubscribe service name  :  ", full_unsubscribe_name

    interval = 5
    gps_collector = None
    nofix = False

    gps_collector = GPSCollector(vin, interval, nofix)

    # Let the main thread run the gps collector

    gps_collector.run()
            


