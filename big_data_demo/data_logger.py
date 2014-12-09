#!/usr/bin/python

#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#

#
# A generic logger / reporter
#
import sqlite3
import Queue
import threading
import time
import dbus
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
    def __init__(self, db_file = '/var/tmp/big_data_demo.sql'):
        threading.Thread.__init__(self)
        self.db_file = db_file
        self.queue = Queue.Queue()
        self.subscriptions_loaded = False
        self.subscriptions = {}

    # Sqlite commands can only be used from the same thread that
    # created the database connection to begin with.
    # Hence the stupid thread solution
    def run(self):
        self.dbc = sqlite3.connect(self.db_file)

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

        self.subscriptions_loaded = True
        while True:
            # Try to get a command sent from a member function
            # call invoked by another thread.
            
            elem = self.queue.get()
            ( command, arg ) =  elem

            
            if command == CMD_ADD_SUBSCRIPTION:
                (channel, sample_interval) = arg
                self.__add_subscription(channel, sample_interval)

            elif command == CMD_DELETE_SUBSCRIPTION:
                self.__delete_subscription(arg)
                    
            elif command == CMD_ADD_SAMPLE:
                self.__add_sample(arg)

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

    def get_subscriptions(self):
        while self.subscriptions_loaded == False:
            sleep (0.1)
            
        res = []
        for channel in self.subscriptions:
            (interval, tmp) = self.subscriptions[channel]
            res.append((channel, interval))
            
        return res

        
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

    def add_sample(self, values):
        self.queue.put((CMD_ADD_SAMPLE, values))

    def __add_sample(self, values):
        # If the channel is not among our subscriptions, then ignore.
        # [ind for ind, elem in enumerate(self.subscriptions) if v[0] == 53]
     
        # If it is not time for us to sample the given channel yet, then ignore
        c_time = int(time.time())

        print "add_sample({})".format(values)
        for (channel, value) in values:
            if not channel in self.subscriptions:
                # print "add_sample({}): Not subscribed to. Ignored".format(channel)
                continue

            ( sample_interval, last_sample_ts ) = self.subscriptions[channel]

            # Skip if we have previously received a sample and
            # the interval to the next sample has yet to elapse.
            if last_sample_ts > 0 and c_time < last_sample_ts + sample_interval:
                # print "add_sample({}): c_time < last_sample_ts={} + sample_interval={}. Skipped".format(c_time, last_sample_ts, sample_interval)
                continue
        
            print "add_sample({}): {}".format(channel, value)
            # Store the sample
            # Convert the value dictionary to a string.
            self.dbc.execute('''INSERT INTO log VALUES (?, ?, ?)''', (c_time, channel, str(value)))
            self.dbc.commit()
        
            # Update the last sample timestamp
            # print "Updating subscriptions[{}] with ({}, {})".format(channel, sample_interval, c_time)
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
