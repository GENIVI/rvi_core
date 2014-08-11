# HVAC DEMO


Copyright (C) 2014, Jaguar Land Rover

This program is licensed under the terms and conditions of the Mozilla
Public License, version 2.0.  The full text of the Mozilla Public
License is at https://www.mozilla.org/MPL/2.0/

# BACKGROUND
The remote HVAC control demo is a part of the Automotive Grade Linux
project where the Tizen IVI climate control (HVAC) screen can be
remotely controlled from a mobile device. Updates made to the IVI HVAC
screen are sent in real time to the mobile device, and vice versa.

An initial release of the remote HVAC demo was done using the MQTT
protocol (mqtt.org). The first milestone of the RVI project will
migrate the HVAC demo to the RVI technology.

# CONTENT
This directory contains a simple subscription service
(hvac\_subscription\_service.py) replacing the MQTT broker. The
subscription service lets the IVI and mobile devices setup
subscriptions so that they receive notifications when the HVAC values
for a specific VIN (Vehicle Identification Number) have been
updated. The service is, in effect, a standard publish/subscribe
setup. 

In order to test the RVI milestone 1 and the subscription service
without having to bring up a complete Tizen IVI and mobile device
environment, a simple emulator for these two components have been
providedin hvac_emulator.py.

The emulator can be started in IVI or mobile device mode. Please run
./hvac_emulator.py for usage details.

# TEST SETUP

Two RVI nodes will be involved in the test:

The backend node will host the subscription service (hvac\_subscription\_service.py), and will also
receive requests from the mobile device emultor (hvac_emulator.py). 

The IVI node will host the IVI emulator.

## CREATE THE MOBILE DEVICE NODE

From the rvi root directory, setup the mobile device node:

    ./setup_rvi_node ivi priv/setup_ivi.config

In a similar manner, setup the IVI backend node:

    ./setup_rvi_node backend priv/setup_backend.config

The result will be two new directories, ivi and backend, created under the rvi root directory.

Since both rvi nodes will execute on the same machine, each node will have to specify their
own port range, which is handled through the -p 


In antoer window, launch the backend node:


	./rvi_node -n backend -p 23070 
	

Launch the IVI rvi node in its own window:

	./rvi_node -n ivi -p 23050 -b localhost:23070


