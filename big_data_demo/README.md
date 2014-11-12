# TRACKING DEMO #


# RVI COMMANDS #


## SUBSCRIBE ##

Subscribe commands are sent from the backend server to a vehicle in order
to setup a regular reporting of specific data from vehicle to server.

Reporting will be done through the ```report``` command.

Multiple subscribe commands can be sent, where all indicated channels are
reported 

     {
		 "jsonrpc": "2.0",
	     "id": 1,
	     "method": "message",
		 "params": {
			 "service": "jlr.com/vin/123456/tracking/subscribe",
		     "channels":, ["location", "odometer", "speed"],
			 "reporting_interval": 5000
		 }
	 } 

### PARAMETERS ###
+ channels<br>
Specifies the channels that we want reported back to the server.

+ reporting_interval<br>
Specifies the number of milliseconds between each data sample that is
to be sent back end server.  If the reporting interval is a negative
integer, the channel's value will be reported at the given (absolute
value) interval, or when the value changes, whatever happens first.


## UNSUBSCRIBE ##

Unubscribe commands are sent from the backend server to a vehicle in
order to stop reporting of one or more data channels previously setup
through a ```subscribe``` command.

     {
		 "jsonrpc": "2.0",
	     "id": 2,
	     "method": "message",
		 "params": {
			 "service": "jlr.com/vin/123456/tracking/unsubscribe",
		     "channels":, ["location", "odometer", "speed"]
		 }
	 } 

### PARAMETERS ###
+ channels<br>
Specifies the channels to stop reporting. If a channel has been
specified several times by multiple subcribe commands, all
subscriptions to the channel are removed.

## REPORT ##

Publish commands are sent from the device to the backend server to report
a batch of values for channels previously subscribed to by a ```subscribe``` command.
Multiple values for a single channel can be provided in a single report.

Each channel is reported with its channel name, its value, and the UTC
msec timestamp when the value was sample.

     {
		 "jsonrpc": "2.0",
	     "id": 3,
	     "method": "message",
		 "params": {
			 "service": "jlr.com/backend/tracking/report",
	         "timestamp":  1415143459110,
		     "data":, [
				 { "channel": "odo", "value": 10022 },
				 { "channel": "odo", "value": 10023 },
				 { "channel": "speed", "value": 113 },
				 { "channel": "waypoint",
				   "value": { "lat": 39.0319, "lon": 125.7538 } } 		 
	         ]
		 }
	 } 


### PARAMETERS ###
+ data<br>
Contains an array of all reported data points.

+ channel<br>
Contains the name of the channel a data point is reporetd for. Matches
the channel name in a previously issued ```subscribe``` command.

+ value<br>
Specifies the value of the given channel at the given time. The actual
value can be a string, a double, or a JSON object, and is implicitly
defined by the channel name.

+ timestamp<br>
Specifies the timestamp in millisecond UTC when the value was sampled
from the vehicle.
