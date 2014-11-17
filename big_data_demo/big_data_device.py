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
import sqlite3
from urlparse import urlparse
import Queue

MY_NAME = "GPS Collector"
CMD_ADD_SUBSCRIPTION = 1
CMD_DELETE_SUBSCRIPTION = 2
CMD_ADD_SUBSCRIPTION = 3
CMD_ADD_SAMPLE = 4
CMD_RETRIEVE_NEXT_SAMPLE = 5
CMD_DELETE_SAMPLE = 6
CMD_DELETE_ALL_SAMPLES = 7
CMD_DUMP_DATABASE = 8
CMD_SHUTDOWN = 9

class Logger(threading.Thread):
    def __init__(self,db_file = '/var/tmp/big_data_demo.sql'):
        threading.Thread.__init__(self)
        self.db_file = db_file
        self.queue = Queue.Queue()

    # Sqlite commands can only be used from the same thread that
    # created the database connection to begin with.
    # Hence the stupid thread solution
    def run(self):
        self.dbc = sqlite3.connect(self.db_file)
        self.subscriptions = {}

        print "Starting logger at {}".format(self.db_file)

        # Create the table that stores log data and index it on its timestamps
        self.dbc.execute('''CREATE TABLE IF NOT EXISTS log (timestamp, channel, value)''')
        self.dbc.execute('''CREATE INDEX IF NOT EXISTS ts_index on log (timestamp ASC)''')

        # Create a table to store all our subscriptions so that they survive a 
        # system restert.
        self.dbc.execute('''CREATE TABLE IF NOT EXISTS subscriptions (channel, interval)''')

        # Retrieve all our subscriptions so that they are easily accessible
        for subscription in self.dbc.execute('''SELECT channel, interval FROM subscriptions'''):
            (channel, interval) = subscription
            # Interval is the sample interval in sec. 
            # 0 is when the UTC of when last sample was made.
            print "Adding subscription {}. Interval {}".format(channel, interval)
            self.subscriptions[channel] = ( interval, 0 )

        
        while True:
            # Try to get a command sent from a member function
            # call invoked by another thread.
            
            elem = self.queue.get()
            print "Elem:", elem
            ( command, arg ) =  elem

            
            if command == CMD_ADD_SUBSCRIPTION:
                (channel, sample_interval) = arg
                self.__add_subscription(channel, sample_interval)

            elif command == CMD_DELETE_SUBSCRIPTION:
                self.__delete_subscription(arg)
                    
            elif command == CMD_ADD_SAMPLE:
                (channel, value) = arg
                self.__add_sample(channel, value)

            elif command == CMD_RETRIEVE_NEXT_SAMPLE:
                # Arg is a queue object to send back the result over
                self.__retrieve_next_sample(arg)

            elif command == CMD_DELETE_SAMPLE:
                # Arg is timestamp to delete
                self.__delete_sample(arg)

            elif command == CMD_DELETE_ALL_SAMPLES:
                self.__delete_all_sample()

            elif command == CMD_DUMP_DATABASE:
                self.__dump_db()

            elif command == CMD_SHUTDOWN:
                print "Logger:run(): Exiting thread"
                return True

            else:
                print "Logger.run(): Unknown command: {} ignored".format(command)

    
        
    def shutdown(self):
        self.queue.put((CMD_SHUTDOWN, True))
        self.join()

    def add_subscription(self, channel, sample_interval):
        self.queue.put((CMD_ADD_SUBSCRIPTION, (channel, sample_interval)))

    def __add_subscription(self, channel, sample_interval):
        if channel in self.subscriptions:
            print "Called {} already in subscriptions. Ignored".format(channel)
            return False

        print "Adding {} to subscriptions. Interval {}".format(channel, sample_interval)
        # Setup a new channel in the dictionary
        self.subscriptions[channel] = (sample_interval, 0)
        try: 
            self.dbc.execute('''INSERT INTO subscriptions VALUES (?, ?)''', (channel, sample_interval))
            self.dbc.commit()
        except sqlite3.Error as e:
            print "An error occurred:", e.args[0]

        print "3"
        return True

    def delete_subscription(self, channel):
        self.queue.put((CMD_DELETE_SUBSCRIPTION, channel))

    def __delete_subscription(self, channel):
        if not channel in self.subscriptions:
            print "unsubscribe(): Channel {} not in subscriptions. Ignored".format(channel)
            return False

        # Remove from subscriptions
        del self.subscriptions[channel]
        self.dbc.execute('''DELETE FROM subscriptions WHERE channel=?''', (channel,))
        return True

    def add_sample(self, channel, value):
        self.queue.put((CMD_ADD_SAMPLE, (channel, value)))

    def __add_sample(self, channel, value):
        # If the channel is not among our subscriptions, then ignore.
        print "add_sample({}): {}".format(channel, value)
        # [ind for ind, elem in enumerate(self.subscriptions) if v[0] == 53]
     
        if not channel in self.subscriptions:
            print "add_sample({}): Not subscribed to. Ignored".format(channel)
            print "add_sample(): ".format(self.subscriptions)
            return False

        # If it is not time for us to sample the given channel yet, then ignore
        c_time = int(time.time())

        ( sample_interval, last_sample_ts ) = self.subscriptions[channel]

        # Return if we have previously received a sample and
        # the interval to the next sample has yet to elapse.
        if last_sample_ts > 0 and c_time < last_sample_ts + sample_interval:
            print "add_sample({}): c_time < last_sample_ts={} + sample_interval={}. Skipped".format(c_time, last_sample_ts, sample_interval)
            return False
        
        # Store the sample
        # Convert the value dictionary to a string.
        self.dbc.execute('''INSERT INTO log VALUES (?, ?, ?)''', (c_time, channel, str(value)))
        self.dbc.commit()
        
        # Update the last sample timestamp
        print "Updating subscriptions[{}] with ({}, {})".format(channel, sample_interval, c_time)
        self.subscriptions[channel] = ( sample_interval, c_time)
        return True

    # Retrieve all samples for the oldest time stamp in the database
    # Return:
    #  False - no samples
    #  (timestamp, [ ( channel, value), ... ]) - Samples for the given timestamp
    #
    def retrieve_next_sample(self):
        q = Queue.Queue()
        self.queue.put((CMD_RETRIEVE_NEXT_SAMPLE, q))
        # Wait for a reply to come back and return whatever it was
        return q.get()
        
    def __retrieve_next_sample(self, queue):
        # Get the oldest timestamp that we have stored.
        (ts, ) = self.dbc.execute('''SELECT min(timestamp) FROM log''').fetchone()

        # If no timestamp, then we have no data in db.
        if ts == None:
            queue.put(False)
            return False
            
        res = []
        # Retrieve all rows with a matching timestamp[
        for row in self.dbc.execute('''SELECT channel, value FROM log where timestamp=?''', (ts,)):
            # Convert value from string back to dict
            res.append((row[0], eval(row[1])))

        queue.put((ts, res))
        return True

        
    def delete_sample(self, timestamp):
        self.queue.put((CMD_DELETE_SAMPLE, timestamp))

    # Delete samples older than the given time stamp.
    def __delete_sample(self, timestamp):
        self.dbc.execute('''DELETE FROM log WHERE timestamp <= ?''', (timestamp,))

    def delete_all_samples(self):
        self.queue.put((CMD_DELETE_SAMPLE, True))

    # Delete allsamples with the given timestamp.
    def __delete_all_samples(self):
        self.dbc.execute('''DELETE FROM log''')

    def dump_db(self):
        self.queue.put((CMD_DUMP_DATABASE, True))

    def __dump_db(self):
        print "LOG dump:"
        for row in self.dbc.execute('''SELECT timestamp, channel, value FROM log'''):
            print row
        print "---"
        print "Subscription dump:"
        for row in self.dbc.execute('''SELECT * FROM subscriptions'''):
            print row
        print "---"
    
class RVICallbackServer(threading.Thread):
    """
    RPC server thread responding to incoming callbacks from the RVI framework
    """
    
    def __init__(self, logger, service_edge, callback_url):
        threading.Thread.__init__(self)
        self.logger = logger
        self.service_edge = service_edge
        self.callback_url = callback_url
        url = urlparse(self.callback_url)
        self.localServer =  RVIJSONRPCServer(addr=((url.hostname, url.port)), logRequests=False)
        self.register_services()
        
    def subscribe(self, channels, interval):
        for channel in channels:
            self.logger.add_subscription(channel, int(interval))
            
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
            


class GPSCollector:
    """
    Collect GPS data
    """
    
    def __init__(self, logger, gps_interval):
        self.vin = vin
        self.gps_interval = gps_interval
        self.last_speed = 1.0
        self.gps_poller = GPSPoller()
        self.logger = logger
        self.transaction_id = 1

    
    def run(self):
        # start GPS polling thread
        self.gps_poller.start()

        # main execution loop
        while True:
            try:
                time.sleep(self.gps_interval)
                
                # process GPS data
                session = self.gps_poller.session
                if session.fix.mode == MODE_NO_FIX:
                    print "Waiting for GPS to fix..."
                    continue

                if isnan(session.fix.time):
                    print "Invalid location:", session
                    continue

                if (session.fix.speed < 0.1) and (self.last_speed < 0.1):
                    print "Waiting for speed..."
#                    continue

                self.last_speed = session.fix.speed

                logger.add_sample('waypoint', { 
                    u'lat': session.fix.latitude,
                    u'lon': session.fix.longitude,
                    u'alt': session.fix.altitude
                })

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


    # Setip the logger.
    logger = Logger()
    logger.start()

    # Setup outbound JSON-RPC connection to the RVI Service Edge
    rvi_server = jsonrpclib.Server(rvi_url)
    
    # Setup inbound JSON-RPC server
    rvi_callback = RVICallbackServer(logger, rvi_server, service_url)
    rvi_callback.start()

    # Setup data sender
    data_sender = DataSender("jlr.com/backend", vin, rvi_url, logger, 3)
    data_sender.start()

    # catch signals for proper shutdown
    for sig in (SIGABRT, SIGTERM, SIGINT):
        signal(sig, cleanup)

    # Start GPS data collection
    interval = 2
    gps_collector = None

    gps_collector = GPSCollector(logger, interval)

    # Let the main thread run the gps collector
    gps_collector.run()
    print "gps_collector.run() exited."
