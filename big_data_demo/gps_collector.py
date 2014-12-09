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
# and sends it to a logger object
#

import threading
from gps import *
import time

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
    Collect GPS data
    """
    
    def __init__(self, logger):
        self.last_speed = 1.0
        self.gps_poller = GPSPoller()
        self.logger = logger

    
    def run(self):
        # start GPS polling thread
        self.gps_poller.start()

        # main execution loop
        while True:
            try:
                time.sleep(1)
                
                # process GPS data
                session = self.gps_poller.session
                if session.fix.mode == MODE_NO_FIX:
                    print "Waiting for GPS to fix..."
                    continue

                if isnan(session.fix.time):
                    print "Invalid location:", session
                    continue

#                if (session.fix.speed < 0.1) and (self.last_speed < 0.1):
#                     print "Waiting for speed..."
#                    continue

                self.last_speed = session.fix.speed

                self.logger.add_sample('location', { 
                    u'lat': session.fix.latitude,
                    u'lon': session.fix.longitude,
                    u'alt': session.fix.altitude
                })

                self.logger.add_sample('speed', session.fix.speed)

                # time = session.utc
                # location.loc_latitude = session.fix.latitude
                # location.loc_longitude = session.fix.longitude
                # location.loc_altitude = session.fix.altitude
                # location.loc_speed = session.fix.speed
                # location.loc_climb = session.fix.climb
                # location.loc_track = session.fix.track
                
                # if the time is valid the data record is valid
                

            except KeyboardInterrupt:
                print ('\n')
                break
            

    def shutdown(self):
        if self.gps_poller:
            self.gps_poller.shutdown()

