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
