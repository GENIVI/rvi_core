The HVAC service is a test service that registers with the backend server to handle a very simple pub/sub setup.

Accepted commands are:

subscribe(vin, subscriber) -> Any updates to the given vin should be sent as publish command to the given subscriber (a service running on a device).
unsubscribe(vin, subscriber) -> Remove subscription conenction between subscriber and vin.

publish(vin, key, value) -> A given vin is updating a key with a new value. All subscribers to the vin will be notified.

Dependencies:
pip install jsonrpclib
