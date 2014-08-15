Copyright (C) 2014, Jaguar Land Rover

This document is licensed under Creative Commons
Attribution-ShareAlike 4.0 International.

# HVAC DEMO
This document describes the purpose, setup, and launch of the HVAC
demo that is the first milestone of the RVI project.

# BACKGROUND
The remote HVAC control demo is a part of the Automotive Grade Linux
project where the Tizen IVI climate control (HVAC) screen can be
remotely controlled from a mobile device. Updates made to the IVI HVAC
screen are sent in real time to the mobile device, and vice versa.

An initial release of the remote HVAC demo was done using the MQTT
protocol (See http://mqtt.org). The first milestone of the RVI project
will migrate the HVAC demo to the RVI technology.

## READER ASSUMPTIONS

The reader is assumed to be able to:

1. Have a basic understanding of Linux directory structures.
2. Start and stop programs on the RVI-hosting system


# PREREQUISITES

1. Erlang runtime R16B01 or later has to be installed on the hosting system.
2. The ```setup_rvi_node.sh``` tool is available to build a release.
3. The ```rvi_node.sh``` tool is available to execute a release.
4. ```hvac_demo/``` is available with its demo code and configuration files.


# DEMO COMPONENTS

The demo is broken into three components that connect to two RVI
nodes. Please see schematics below:

## SUBSCRIPTION SERVICE
This directory contains a simple subscription service
(subscription\_service.py) replacing the MQTT broker. The
subscription service lets the vehicle and mobile devices setup
subscriptions so that they receive notifications when the HVAC values
for a specific VIN (Vehicle Identification Number) have been
updated. The service is, in effect, a standard publish/subscribe
setup. 

## HVAC EMULATOR
In order to test the RVI milestone 1 and the subscription service
without having to bring up a complete Tizen IVI and mobile device
environment, a simple emulator for these two components have been
provided in hvac\_emulator.py and mobile\_emulator.py

The HVAC emulator simulates the HVAC application running on a head unit
in a vehicle. It uses a VIN number, a part of the service prefix
of the vehicle RVI node, as a unique identifier and subscribes to HVAC
updates, such as fan speed, originating from the mobile device
emulator. 

The HVAC emulator can also have new HVAC values entered on
the command line and send them to any connected mobile device
emulators.

All subscriptions and updates are bounced off the subscription service
connected to the backend RVI node.

## MOBILE DEVICE EMULATOR

The mobile device emulator connects to the backend server and
registers itself as a service where a phone number, provided as a
command line argument, is a part of the service name. The mobile
controls a specific VIN, also provided as a command line argument. 

Just like the HVAC emulator, the all subscriptions and updates are
bounced of the subscription service.


# DEMO SETUP

Two RVI nodes, both hosted by a single machine, will be involved in
the test:

The backend RVI node will host the subscription service
(subscription\_service.py) and the mobile emulator (mobile\_emulator.py).

The vehicle RVI node will host the HVAC emulator (hvac\_emulator.py).

## COMPILE THE RVI SYSTEM
See ../BUILD.md for details.

## CREATE THE VEHICLE DEVELOPMENT RELEASE

*See ../CONFIGURE.md for details on the configuration process.*

From the rvi root directory, setup the vehicle node:

    ./setup_rvi_node.sh vehicle hvac_demo/vehicle.config

The new developer release will be created in a subdiretory named
```vehicle```


## CREATE THE BACKEND DEVELOPMENT RELEASE

In a similar manner, setup the backend node:

    ./setup_rvi_node.sh backend hvac_demo/backend.config


The release will be created in a subdiretory named ```backend```


## LAUNCH THE VEHICLE RVI NODE

In its own window, launch the vehicle RVI node that will serve the HVAC emulator.

	./rvi_node.sh -n vehicle

The VIN number that will be used by the mobile device are the digits
at the end of the "Node Service Prefix" printed out at the end of the
launch process. 

By default, the ```hvac_demo/vehicle.config``` file has its
```node_service_prefix``` entry set to ```jlr.com/vin/1234/```,
yielding a VIN of "1234". 

# DEMO LAUNCH


## LAUNCH THE BACKEND RVI NODE

In its own window, launch the backend rvi node that the subscription
service and mobile device emulator will connect to.

	./rvi_node.sh -n backend
	
Make a note of the Service Edge URL address printed out by the logging service.


## LAUNCH THE SUBSCRIPTION SERVICE

In its own window, launch the subscription service and specify the URL
of the backend RVI node's Service Edge:

    cd hvac_demo
    ./subscription_service.py http://127.0.0.1:8801 
	
Modify the ```http://127.0.0.1:8801``` to match the URL reported by
the backend RVI node.


## LAUNCH THE HVAC EMULATOR

In its own window, launch the HVAC emulator and have it connect to
the vehicle RVI node.

    cd hvac_demo
    ./hvac_emulator.py http://127.0.0.1:8811
	
Modify the ```http://127.0.0.1:8811``` to match the Service Edge URL
reported by the vehicle RVI node.

At startup the HVAC emulator will report the VIN number that it is
associacted with. (Default 1234)

The HVAC emulator accepts \[key\] \[value\] input lines that are
distributed to all mobile devices controlling the VIN of the vehicle RVI
node.


## LAUNCH THE MOBILE DEVICE EMULATOR

In its own window, launch the mobile device emulator and have it connect to
the backend RVI node.

    cd hvac_demo
    ./mobile_emulator.py  http://127.0.0.1:8801 +1941231234 1234
	
Modify the ```http://127.0.0.1:8801``` to match the Service Edge URL
reported by the backend RVI node.

The phone number (+19491231234) is arbitrary, but has to be unique
across all running mobile device emulators.

The VIN number (1234) is the VIN number reported by the HVAC emulator at startup.

The mobile device emulator accepts \[key\] \[value\] input lines that
are distributed to the vehicle with the matching VIN.


# DEMO EXECUTION

## SEND VALUES FROM MOBILE TO HVAC EMULATOR

Any \[key\] \[value\] entered in the mobile device emulator
will be sent up to the subscription service and forwarded to 
the HVAC emulator.


## SEND VALUES FROM HVAC TO MOBILE EMULATOR

Any \[key\] \[value\] entered in the HVAC emulator
will be sent up to the subscription service and forwarded to 
the mobile emulator.


## EXIT PROGRAMS

Terminate the mobile device emulator by entering "q" followed by enter.

Terminate the HVAC emulator by entering "q" followed by enter.

Terminate the subscription service by pressing Ctrl-c.

Terminate the vehicle RVI node by pressing Ctrl-c Ctrl-c.

Terminate the backend RVI node by pressing Ctrl-c Ctrl-c.


