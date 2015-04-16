# RELEASE NOTES #

# v0.1 #
Initial demo version. Works with the hvac_demo and its hvac / mobile emulators.

# v0.2 #
Various improvements

# v0.3.0 #
* <b>Auto connect static links</b><br>
  When data link is lost to a statically configured node (See
  CONFIGURE STATIC NODES in [configuration](CONFIGURE.md) document, a
  reconnect attempt will be made every five seconds.

* <b>Intra-component communication speedup</b><br>
  Components inside an RVI node can now choose if they want to use
  JSON-RPC (compatbility mode) or Erlang Genserver calls to
  communicate.

* <b>List available services</b><br>
  Added get\_available\_services, allowing a service to query its RVI node
  of which other services are on-line and available for invocation.


* <b>New python tutorial code</b>
  Created top level ```python``` directory with rvilib.py and three
  new apps that can be used as tutorials on how to interface RVI. The
  apps are:

  ```rvi_service.py``` registers a service with an RVI node and prints
  out information when it is invoked by RVI.

  ```rvi_call.py``` invokes a service in an RVI network.


  ```rvi_get_services.py``` connects to a local RVI node and prints out
  a list of all callable services accessible throughout that node.

  ```rvilib.py``` has been cleaned up and simplified.


* <b>Service availability notifications</b><br>
  Added real-time notifications from an RVI to its locally connected services
  when a new service is added or deleted somewhere in the network. This
  allows for the implementation of "vehicle online" indicators and other
  real-time sensitive behavior.

* <b>Big data demo moved</b><br>
Big data demo moved to its own repo at https://github.com/PDXostc/rvi_bigdata

* <b>SOTA demo moved</b><br>
  SOTA demo moved to its own repo at https://github.com/PDXostc/rvi_sota_demo

# v0.3.1 #

* <b>Don't send service availablity notifications over websockets</b><br>
  For unknown reasons this crashes the RVI node intermittently

# v0.3.2 #
* <b>Re-enable service availability notifications over websockets</b><br>
  Issue 15 was an artifact of issue 14, thus we can turn notifications back on
  now that 15 is fixed.

* <b>Implement datalinkbert_rpc pings</b><br>
  This fixes issue #14 where a rebooted tizen box did not shut down
  the data link tcp connection correctly, which left it dangling on
  the server. A periodic tcp (5 min default) will trigger a
  kernel-originated shutdown when the remote address cannot be
  reached.

* <b>Filter out resurrected services when a remote data link disappears</b><br>
  This fixes issue #14 where a rebooted tizen box would register its
  services while its old (pre-reboot) dangling tcp connection was
  still alive from the server's point of view. When the danlging tcp
  connection died, the RVI would delete all services associated with
  it, thus deleting active services registered over the new connection
  after the tizen box rebooted.

  We now filter any services associated with a dead tcp connection
  against identically named services registered over other
  connections.
