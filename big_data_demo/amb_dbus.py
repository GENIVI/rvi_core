#!/usr/bin/python

#
# Copyright (C) 2014, Jaguar Land Rover
#
# This program is licensed under the terms and conditions of the
# Mozilla Public License, version 2.0.  The full text of the 
# Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
#


#
# AMB Dbus monitor that reports to the provided logger object
#
import sys
import time
import threading
import dbus
import json

class DBUSMonitor(threading.Thread):
    """
    Retrieves data from databus monitor and sends it to 
    """
    
    def __init__(self, logger):
        threading.Thread.__init__(self)
        self.sysbus = dbus.SystemBus()
        self.mgr_broker = self.sysbus.get_object("org.automotive.message.broker", "/")
        self.mgr_if = dbus.Interface(self.mgr_broker, "org.automotive.Manager")
        self.logger = logger

        # Maps object name (VehicleSpeed) to the corresponding
        # DBUS object path 
        self.name_to_path = {} 

        # Reverse mapping
        self.path_to_name = {} 

        
        # Maps object path to the corresponding sample interval (in
        # seconds)
        self.path_to_interval = {} 


        # Stores a dictionary with element format (next_timeout_utc,
        # object_path_array).
        #
        # Dicitonary is sorted on next_timeout by schedule(), allowing
        # for the run() scheduler to quickly determine when the next
        # timeout occurrs. The dictionary will only have a given
        # object path stored once. A single timeout may store multiple
        # object paths that are to be sampled at that given time,
        # hence the array of object paths
        self.schedule = {}

        available_objects = self.mgr_broker.List("", dbus_interface='org.automotive.Manager')

        for name in available_objects:
            # Get object path for name
            obj_path = self.mgr_if.FindObject(name)[0]
            # obj_path = self.sysbus.get_object("org.automotive.message.broker", name)
            print "add_object({}) -> {}".format(name, obj_path)
            self.name_to_path[name] = obj_path
            self.path_to_name[obj_path] = name
            self.path_to_interval[obj_path] = 0 # Not subscribed to
            

    def unschedule_sample(self, obj_path):
        # Iterate over all time slots in schedule
        for ts in self.schedule:

            # Each slot in schedule is an array of dbus object paths
            # to be sampled at the given time.
            print "unschedule({}): checking ({}, {})".format(obj_path, ts, self.schedule[ts])
            
            # Delete obj_path from the given timeslot in schedule.
            # May throw an exception on index(obj_path) if it does not
            # exist. In that case, we simply move on to the next time stamp
            # in the schedule
            try:
                del self.schedule[ts][self.schedule[ts].index(obj_path)]
            except KeyError:
                #print "unschedule({}):1 Not found in ts {}".format(obj_path, ts)
                continue

            except ValueError:
                # print "unschedule({}):2 Not found in ts {}".format(obj_path, ts)
                continue

            # Deletion was successful
            print "unschedule({}): after delete: {}".format( obj_path, self.schedule[ts])
            return True

        # We got out of loop with nothing found
        return False

    # 
    # Subscribe to a given object, sampling it at every given number
    # of seconds and send up the result to the logger provided to the
    # constructor.
    #
    def schedule_sample(self, obj_path, timestamp):
        # print "schedule_sample({}, {})".format(obj_path, timestamp)

        # Delete any old entries we have for the given object path
        self.unschedule_sample(obj_path)

        # Check if we have already have a slot for the given timestamp.
        # If so, append our object path to the existing array
        # of objects triggered at the given time.
        # If not, initialize a new time slot with our object path.
        if timestamp in self.schedule:
            self.schedule[timestamp].append(obj_path)
        else:
            self.schedule[timestamp] = [obj_path]
            # Re-sort schedule
            sorted(self.schedule)

    def subscribe(self, name, sample_interval):
        try:
            obj_path = self.name_to_path[name]
            self.path_to_interval[obj_path] = sample_interval

        except KeyError:
            print "subscribe_object({}): Not found".format(name)
            return False
            
        print "amb_dbus:subscribe({}, {}): called".format(name, sample_interval)
        # Schedule the object to be sampled sample_interval seconds from now
        self.schedule_sample(obj_path, int(time.time()) + sample_interval)

                
    def sample_and_report(self, obj_path):
        obj_name = self.path_to_name[obj_path]
	prop_if = dbus.Interface(self.sysbus.get_object("org.automotive.message.broker", obj_path),
                                 "org.freedesktop.DBus.Properties")

        # Ugly conversion from dbus types to json back to native python types.
        tmp = eval(json.dumps(prop_if.GetAll("org.automotive."+obj_name)))
        res = {}
        print tmp
        for entry in tmp:
            print entry
            val = tmp[entry]
            # For some reason dbus.Double survives ths conversion above.
            if type(val) is dbus.Double:
                val = float(val)
                                  
            # Should really be recursive into dictionaries and arrays,
            res[entry] = val
            
        print "dumping:", res
        self.logger.add_sample(obj_name, res)
        
    def run(self):
        while True:
            #
            # Stupid way of doing it. 
            # We should sleep either until the first element
            # in the schedule queue  is due, or
            # we get a wakeup signal from schedule()
            #
            time.sleep(1.0)

            # Retrieve a sorted list of all timestamps in self.schedule
            # FIXME: Some sort of ordered dictionary would probably be
            #        smart here.
            sorted_ts = sorted(self.schedule)
            ct = int(time.time())

            # Go through all timestamps that are due for execution
            while len(sorted_ts) > 0 and sorted_ts[0] <= ct:
                # Process the first element of the sorted list
                obj_path_arr = self.schedule[sorted_ts[0]]

                # Delete the time slot from schedule
                del self.schedule[sorted_ts[0]]
                
                # Delete the time slot from sorted time slots
                del sorted_ts[0]
                
                # Go through all retrieved object paths, sample and report them
                for obj_path in obj_path_arr:
                    self.sample_and_report(obj_path)
                    # Reschedule self to the next interval time slot
                    self.schedule_sample(obj_path, int(time.time()) +self.path_to_interval[obj_path])

            
