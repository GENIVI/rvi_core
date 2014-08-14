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
protocol (mqtt.org). The first milestone of the RVI project will
migrate the HVAC demo to the RVI technology.

## READER ASSUMPTIONS

The reader is assumed to be able to:

1. Have a basic understanding of Linux directory structures.
2. Start and stop programs on the RVI-hosting system


# PREREQUISITES

1. Erlang runtime R16B01 or later has to be installed on the hosting system.
2. The ```setup_rvi_node.sh``` tool is available to build a release.
3. ```priv/ivi.config``` is available to build the IVI RVI node from.
4. ```priv/backend.config``` is available to build the backend RVI node from.
5. ```hvac_demo/``` is available with its demo code.


# DEMO COMPONENTS

## SUBSCRIPTION SERVICE
This directory contains a simple subscription service
(hvac\_subscription\_service.py) replacing the MQTT broker. The
subscription service lets the IVI and mobile devices setup
subscriptions so that they receive notifications when the HVAC values
for a specific VIN (Vehicle Identification Number) have been
updated. The service is, in effect, a standard publish/subscribe
setup. 

## IVI EMULATOR
In order to test the RVI milestone 1 and the subscription service
without having to bring up a complete Tizen IVI and mobile device
environment, a simple emulator for these two components have been
provided in hvac\_ivi\_emulator.py and hvac\_mobile\_emulator.py

The IVI emulator simulates the HVAC application running on a head unit
in a vehicle. It uses a VIN number, a part of the node service prefix
of the IVI RVI node, as a unique identifier and subscribes to HVAC
updates, such as fan speed, originating from the mobile device
emulator. 

The IVI emulator can also have new HVAC values entered on
the command line and send them to any connected mobile device
emulators.

All subscriptions and updates are bounced off the subscription service
connected to the backend RVI node.

## MOBILE DEVICE EMULATOR

The mobile device emulator connects to the backend server and
registers itself as a service where a phone number, provided as a
command line argument, is a part of the service name. The mobile
controls a specific VIN, also provided as a command line argument. 

Just like the IVI emulator, the all subscriptions and updates are
bounced of the subscription service.


# DEMO SETUP

Two RVI nodes, both hosted by a single machine, will be involved in
the test:

The backend node will host the subscription service
(hvac\_subscription\_service.py), and will also receive requests from
the mobile device emultor (hvac_emulator.py).

The IVI node will host the IVI emulator only.


## COMPILE THE RVI SYSTEM
See ../BUILD.md for details.

## CREATE THE IVI DEVELOPMENT RELEASE

*See ../CONFIGURE.md for details on the configuration process.*

From the rvi root directory, setup the IVI node:

    ./setup_rvi_node.sh ivi priv/ivi.config

The new developer release will be created in a subdiretory named
```ivi```


## CREATE THE BACKEND DEVELOPMENT RELEASE

In a similar manner, setup the backend node:

    ./setup_rvi_node.sh backend priv/backend.config

The result will be two new directories, ivi and backend, created under
the rvi root directory.

## LAUNCH THE IVI RVI NODE

In its own window, launch the IVI rvi node that will serve the IVI emulator.

	./rvi_node.sh -n ivi

The VIN number that will be used by the mobile device are the digits
at the end of the "Node Service Prefix" printed out at the end of the
launch process. 

By default, the ```priv/ivi.config``` file has its
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
    ./hvac_subscription_service.py http://127.0.0.1:8801 
	
Modify the ```http://127.0.0.1:8801``` to match the URL reported by
the backend RVI node.


## LAUNCH THE IVI EMULATOR

In its own window, launch the ivi emulator and have it connect to
the IVI RVI node.

    cd hvac_demo
    ./hvac_ivi_emulator.py  http://127.0.0.1:8811
	
Modify the ```http://127.0.0.1:8811``` to match the Service Edge URL
reported by the IVI RVI node.

At startup the IVI emulator will report the VIN number that it is
associacted with. (Default 1234)

The IVI emulator accepts \[key\] \[value\] input lines that are
distributed to all mobile devices controlling the VIN of the IVI RVI
node.


## LAUNCH THE MOBILE DEVICE EMULATOR

In its own window, launch the mobile device emulator and have it connect to
the backend RVI node.

    cd hvac_demo
    ./hvac_mobile_emulator.py  http://127.0.0.1:8801 +1941231234 1234
	
Modify the ```http://127.0.0.1:8801``` to match the Service Edge URL
reported by the backend RVI node.

The phone number (+19491231234) is arbitrary, but has to be unique
across all running mobile device emulators.

The VIN number (1234) is the VIN number reported by the IVI emulator at startup.

The mobile device emulator accepts \[key\] \[value\] input lines that
are distributed to the IVI with the matching VIN.


# DEMO EXECUTION

## SEND VALUES FROM MOBILE TO IVI EMULATOR

Any \[key\] \[value\] entered in the mobile device emulator
will be sent up to the subscription service and forwarded to 
the IVI emulator.


## SEND VALUES FROM IVI TO MOBILE EMULATOR

Any \[key\] \[value\] entered in the IVI emulator
will be sent up to the subscription service and forwarded to 
the mobile emulator.


## EXIT PROGRAMS

Terminate the mobile device emulator by entering "q" followed by enter.

Terminate the IVI emulator by entering "q" followed by enter.

Terminate the subscription service by pressing Ctrl-c.

Terminate the IVI RVI node by pressing Ctrl-c Ctrl-c.

Terminate the backend RVI node by pressing Ctrl-c Ctrl-c.


