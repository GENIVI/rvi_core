Copyright (C) 2014, Jaguar Land Rover

This document is licensed under Creative Commons
Attribution-ShareAlike 4.0 International.


# RVI PYTHON INTEGRATION

This directory contains the ```rvilib.py``` file necessary to have
your python code send and receive RVI service invocations.

Also included are two test programs that use ```rvilib.py``` :

1. ```rvi_service.py```<br>
Registers a single service with an RVI node and then prints out
a message when that service is invoked.

2. ```rvi_call.py```<br>
Invokes a service registered in an RVI network.

These two commands can be used to quickly test connectivity between
various RVI nodes.

# SETUP
In order for ```rvilib.py``` to work, it needs jsonrplib installed from

[https://github.com/joshmarshall/jsonrpclib](https://github.com/joshmarshall/jsonrpclib).

Install by using one of the following commands:

    easy_install jsonrpclib

or

    pip install jsonrpclib


You can use rvilib.py by simply copying it into your project.

# CALLING A SERVICE

See ```rvi_call.py``` for how to invoke RVI-based services.

# IMPLEMENTING A SERVICE

See ```rvi_service.py``` for how to implement and register RVI-based services.


