# SETTING UP NODE AUTHENTICATION AND AUTHPRIZATION

This document describes the process of setting up root keys, device
keys, and certificates.


## TERMINOLOGY AND COMPONENTS

### Certificate issuer
A certificate issuer is an entity that signs device keys and
certificates with a private root key. Devices with the corresponding
public root key will be able to authenticate signed device keys and
authorize signed certificates.

### Root key 
A root key, a 2048+ bit RSA key pair, is generated once for an issuer
of certificates.  The private key is stored in the certificate
issuer's servers and is not shared.  The public key is manually
installed on all RVI nodes that are to trust certificates from the
certificate issuer.

### Device key
A device key is a per-RVI node 2048+ bit RSA key pair. The private part of
the device key is stored on a host (server, embedded device, mobile device, etc)
and is not shared. The public part of the key is used in two ways:

1. **To prove the identify of an RVI node**<br>
   When two RVI nodes locate each other over a data link (WiFi, 3G,
   Bluetooth, etc), they exchange an authenticate ("au") packet to
   prove their identity. This packet has the public part of the device
   key encoded as a JSON Web Token (JWT - RFC7519) token signed by the
   private part of the root key.<br> The receiver can use its locally
   stored public key to validate that the received public device is
   signed by the private root key of a trusted certificate issuer.

2. **To prove ownership of certificates.**<br>
   Embdded in the authenticate packet are one or more certificates
   proving the sending RVI node's right to register and invoke
   services. The certificate, signed by the private root key of the
   issuer, contains the public key of the sending device encoded as 
   JWT structure. This public device key can be used by a
   receiver to verify the signature of a service invocation requests
   sent by the remote RVI node.

### Certificate 

A certificate is a JSON Web Token, signed by the private root key of
the certificate issuer, that proves that the RVI node with a given
public device key has the right to invoke a specific set of services
and to register another set of services.

The certificate is encoded as a JSON Web Token (JWT) signed
by the privet root key.  The decoded payload has the following JSON
elements.

Command line parameters to ```rvi_create_certificate.py``` given in
parenthesis. Items marked with '*' ar slated for name changes to
better reflect JWT practises and RVI semantics.

1. **```iss``` Issuer (```--issuer```)**<br>
   A domain name identifying the issuer. Currently supported but not
   used.

2. **```create_timestamp```* - Creation time stamp**<br>
   Unix time when the certificate was created.
   <br><i>Will be renamed ```iat``` to comply with JWT</i>

3. **```sources```* - Right to register (```--invoke```)**<br>
   A list of full service names that the certificate grants the right to
   register, allowing other, credentialed RVI nodes to invoke these
   services.
   <br><i>Will be renamed ```register``` to better comply with semantics.</i>

4. **```destinations```* Right to invoke (```--register```)**<br>
   A list of full service names that the certificate grants the right
   to invoke on other RVI nodes who have registered them
   <br><i>Will be renamed ```invoke``` to better comply with semantics.</i>

5. **```keys``` Public device keys (```--device_key```)**<br>
   Contains one or more (currently only one) public device keys in JSON
   Web Key (RFC7517) format. The receiver will use this key to validate
   subsequent service invocations through the signatures submitted with
   the invocations.

6. **```start```* Start time of validity period (```--start```)**<br>
   Stored under the ```validity``` JSON element and specifies the Unix
   time stamp when the certificate becomes valid. The receiving RVI node
   will check that the current time is not before the ```start``` time stamp
   of the certificate.
   <br><i>Will be renamed ```nbf``` to comply with JWT.</i>

7. **```stop```* Stop time of validity period (```--stop```)**<br>
   Stored under the ```validity``` JSON element and specifies the Unix
   time stamp when the certificae expires. The receiving RVI node will
   check that the current time is not after the ```stop``` time stamp
   of the certificate.
   <br><i>Will be renamed ```exp``` to comply with JWT.</i>


## SETTING UP AN RVI NETWORK SECURITY - GENERAL FLOW

The general flow of events for setting up security are as follows:

1. **Create root key pair ```rvi_create_root_key.sh```**<br>
   A single root key is created by the certificate issuer. Two PEM
   files are created in this process. One PEM file with the
   private/public key that never leaves the issuer's trusted server,
   and one public-only PEM file that is installed on every RVI node
   that is to accept certificates from the issuer.

2. **Create device key pairs ```rvi_create_device_key.py```**<br>
   Each RVI node need to have its own device key pair. The device key
   script will create a private/public key PEM file that never leaves
   the device, a public-only PEM file that is embedded into
   certificates, and a JWT file with the public device key encoded as
   a JSON Web Key (JWK - RFC 7159) signed by the private root key
   generated in step 1.

3. **Create certificates ```rvi_create_certificate.py```**<br>
   Certificates are generated to allow a specific RVI node (with a
   given device key) tor register (setup) services that it wants other
   RVI nodes to invoke, and to invoke serivces registered by other RVI
   nodes The certificate script creates a JWT file, signed by the root
   key, that decodes into the certificate describe in the
   [Certificate](#Certificate) chapter.  Certificates are stored on
   the credentialed RVI node.


### Provisioning a root key pair

#### Creating the root key PEM files

The root key, consisting of a private/public RSA256 key PEM file, and
a second PEM file with only the public portion of the key, is created
by the following command:

    rvi_create_root_key.sh -b 2048 -o my_root_key

* **```-b 2048```**<br>
  Specifies the number of bits in the key.

* **```-o my_root_key```**<br>
  Specifies the file name prefix of the two created key files.

Once executed, three files will be created:

1. **```my_root_key_priv.pem```**<br>
   This file contains the private/public key pair that must never leave
   the credit issuer's trusted environment. It will be used to sign the
   JWT formatted device key and all certificates created by the
   certificate issuer.

2. **```my_root_key_pub.pem``**`<br>
   This file contains the public-only key that is to be installed on
   every RVI node that is to accept device keys and certificates signed
   by the certificate issuer.

### Configuring RVI to use a public root key
Only ```rvi_create_device_key.py``` and ```rvi_create_certificate.py``` use the
private root key stored in ```my_root_key_priv.pem```, generated above.
The RVI node itself is never aware of that file.

The RVI node does need the public root key, stored in ```my_root_key_pub.pem```,
is referenced from the RVI's configuration file stored 
as ```{ rvi_core, { provisioning_key, "..../my_root_key_pub.pem" }}```.



### Provisioning a device key pair

#### Creating the device key PEM files
A device key, consisting of a private/public RSA256 key PEM file, a
second PEM file with only the public portion of the key, and a third
JWT is created by the following command:

    rvi_create_device_key.py -p my_root_key_priv.pem -o my_device_key -b 2048

* **```-b 2048```**<br>
Specifies the number of bits in the device key.<br>

* **```-p my_root_key_priv.pem```**<br>
Specifies the private root key to sign the device key with when it is
stored in the JWT file (see below). The root key is created by the
```rvi_create_root_key.sh``` script.<br>

* **```-o my_device_key``**<br>
Specifies the file name prefix of the three created device key files.
created key files.

Once executed, three files will be created:

1. **```my_device_key_priv.pem```**<br>
   This file contains the private/public key pair that must never leave
   the device's trusted environment. It will be used to sign 
   outgoing service invocation request.

2. **```my_device_key_pub.pem```**<br>
   This file contains the public-only key that is to be added to 
   certificates issued for the device by a certificate issuer.
   
3. **```my_device_key_pub_sign.jwt```**<br>
   This file contains the public-only key, signed by the root key,
   that is to be provided as authentication when an RVI node identifies
   itself toward another. The file is stored in JSON Web Token format.


### Configuring RVI to use a device key 

The RVI needs the device private/public key root key, stored in
```my_device_key_priv.pem```, is referenced from the RVI's configuration
file in ```{ rvi_core, { key_pair, "..../my_device_key_priv.pem" }}```.


### Provisioning a certificate

#### Creating the certificate file A certificate, consisting of a
A certificate is a JWT-formatted JSON structure signed by the root
private key, is stored on an RVI node to be presented to remote node
as evidence that the sender has the right to invoke and register the
specified services.

The certificate is created by the following command

    ./rvi_create_certificate.py --id=my_cert_id \
                            --device_key=my_device_key_pub.pem \
                            --start='2015-12-01 00:00:00' \
                            --stop='2015-12-31 23:59:59' \
                            --root_key=my_root_key_priv.pem \
                            --register='jlr.com/vin/abc/unlock jlr.com/vin/abc/lock' \
                            --invoke='jlr.com/backend/report jlr.com/backend/set_state' \
                            --jwt_out=my_cert.jwt \
							--cert_out=my_cert.json \
                            --issuer=jaguarlandrover.com

The following arguments are provided
* **```--id=my_cert_id```**<br>
  System-wide unique ID to be assigned to this certificate.

* **```--device_key=my_device_key_pub.pem```**<br>
  Specifies that the public device key, generated by ```create_device_key.py```
  shall be embedded into the generated certificate as the certificate owner.

* **```--root_key=my_root_key_priv.pem```**<br>
  Specifies that the certificate shall be signed by the private root
  key generated by ```create_root_key.sh```.

* **```--invoke='jlr.com/backend/report jlr.com/backend/set_state'```**<br>
  Gives the device with the certificate-embedded public key the right to invoke
  the services ```jlr.com/backend/report``` and ```jlr.com/backend/set_state```.

* **```--register='jlr.com/vin/abc/unlock jlr.com/vin/abc/lock'```**<br>
  Gives the device with the certificate-embedded public key the right to register
  the services ```jlr.com/backend/report``` and ```jlr.com/backend/set_state```.

* **```--start='2015-12-01 00:00:00'```**<br>
  Specifies that the certificate shall become valid Dec 1, 2015 at
  midnight.

* **```--stop='2015-12-31 23:59:59'```**<br>
  Specifies that the certificate shall expire valid Dec 31, 2015 at
  11:59:59 PM.

* **```--jwt_out=my_cert.jwt```**<br>
  Specifies the name of the JWT file that is to be written with the
  certificate signed by the root key in ```my_root_key_priv.pem```.

* **```--cert_out=my_cert.json```**<br>
  Specifies a file to write a JSON-formatted copy of the certificate into.
  This file is for human inspection only and is not used by RVI or any other
  scropts.

* **```--issuer=jaguarlandrover.com```**<br>
  Specifies that the certificate issuer is ```jaguarlandrover.com```.
  This value is currently not used.


Once executed, one mandatory and one optional file will be created:

1. **```my_cert.jwt```**<br>
   This file contains the generated certificate, signed by the
   private root key specified by ```--root_key=```. The content
   of this file will be provided by an RVI node to prove its righ
   to register and invoke services toward remote RVI nodes 

	
2. **```my_cert.json```**<br>
   Only created if ```--cert_out=``` has been give. Contains a human
   readable JSON form of the generated root key.


### Configuring RVI to use a certificate
The RVI needs the certificates to prove its right to register and invoke
services toward remote nodes. The generated
certificate file, ```my_cert.jwd```, is placed in a directory with other
certificates owned by the device.

The certificate directory itself is referenced from the RVI's
configuration file in ```{ rvi_core, { cert_dir, "...." }}```.




## SETTING UP A DEVICE THROUGH ONE-TIME TOKENS

This chapter describes a yet-to-be-implemented procedure
for provisioning new devices 

### Initial provisioning at app install
An device-specific key pair is generated by device and stored locally.

The app has one pre-provisioned node certificate, signed by the
root server, allowing it to invoke ```jlr.com/provisioning/init_setup``` 
and ```jlr.com/provisioning/request_provisioning```. The certificate also
provides the right to register ```jlr.com/mobile/*/dm/cert_provision``` 
and ```jlr.com/mobile/*/dm/key_provision``` 
The certificate keys section, normally holding public device
keys, is empty.

The device has the IP address of its provisioning server.

### Device setup process

1. Device connects to provisioning server<br>
   The app is started for the first time and connects to the
   provisioning server.

2. Device sends authenticate to server<br>
   The command contains no key, only a single pre-provisioned node certificate giving
   the device the right to invoke and register the functions listed in
   above.<br>
                        
3. Server sends authenticate to device<br>
   The server's public device key, signed by the root private key, is
   sent together with no node certificates, thus giving the server no
   rights to register or invoke services with the device.

4. Device sends a service announce to server<br>
   After validating server authenticate package, the device
   sends a service announce to the server.
   The command contains the services ```jlr.com/mobile/1234/dm/cert_provision```
   and  ```jlr.com/mobile/1234/dm/key_provision```,
   which can be invoked by the provisioning service to install a new
   certificate and signed public device key on the device. 

5. Server sends a service announce to device<br>
   The announcement contains the services ```jlr.com/provisioning/init_setup```
   and```jlr.com/provisioning/request_provisioning``` .

6. Device invokes ```jlr.com/provisioning/init_setup``` on server<br>
   The sole argument is the device ID, e.g. 1234.  The command is
   validated by the server through the pre-provisioned cert. Since
   the cert contains no device public key, any device can invoke it.

7. Sideband token transmission from provisioning service to device<br>
   The provsioning server transmits a 128 bit random token to the device
   using a sideband channel such as SMS or similar.

8. Device invokes ```jlr.com/provisioning/request_provisioning``` on server<br>
   The device provides its public key, and the token received in step 7 as
   arguments to the call.

9. Provisioning service signs device public key<br>
   The public key provided in step 8 is signed by the root private key.

10. Provisioning service creates node certificates<br>
    The created cert gives the holder the right to invoke ```jlr.com/vin/ABCD/unlock```.<br>
    The certificate also gives the holder the right to register ```jlr.com/mobile/1234/status.```<br>
    The certificate includes the device public key provided in step 8.
    The certificate is signed by the private root key.<br>

11. Provisioning service invokes ```jlr.com/mobile/1234/dm/key_provision```<br>
    The provisioning service invokes key provisioning service on
    the device, announced by the device to the service in step 4, to
    install the signed public device key on the device.<br>
	The key, signed in step 9, is provided as a single argument.
	The device matches the key with its existing key.<br>
	The device validates the signature using the pre-provisioned public root key.<br>
	The device stores the signed public key to be used in future authentication messages.

12. Provisioning service invokes ```jlr.com/mobile/1234/dm/cert_provision```<br>
    The provisioning service invokes certificate provisioning service on
    the device, announced by the device to the service in step 4, to
    install the certificate created in step 10.<br>
	The device matches the public key of the certificate against its own public key<br>
	The device validates the signature using the pre-provisioned public root key.<br>
	The device stores the signed certificate to be used in future authentication messages.


## DEVICE - VEHICLE SESSION USE CASE

1. Device connects to vehicle ABCD<br>
   Connection is done over bluetooth, with no Internet connection.

2. Device sends authenticate to vehicle<br>
   The command contains the root-signed public device key from step 11 in the previous chapter.<br>
   The command contains the root-signed certificate from step 12 in the previous chapter.<br>
   The vehicle verifies the public device key signature using the pre-provisioned public root key.<br>
   The vehicle verifies the certificate signature using the pre-provisioned public root key.<br>
   The vehicle marks the device as being allowed to invoke ```jlr.com/vin/ABCD/unlock```<br>
   The vehicle marks the device as being allowed to register ```jlr.com/mobile/1234/status```<br>

3. Vehicle sends authenticate to device<br>
   The command contains a root-signed public device key for the vehicle
   The command contains a root-signed certificate, allowing the
   vehicle to invoke ```jlr.com/vin/*/status```, and register ```jlr.com/vin/ABCD/unlock```.<br>
   The device verifies the public device key signature using the pre-provisioned public root key.<br>
   The device verifies the certificate signature using the pre-provisioned public root key.<br>
   The device marks the vehicle as being allowed to invoke ```jlr.com/mobile/1234/status```<br>
   The device marks the vehicle as being allowed to register ```jlr.com/vin/ABCD/unlock```<br>


4. Device sends service announce to vehicle<br>
   The command contains ```jlr.com/mobile/1234/status```.<br>
   Vehicle validates that the vehicle has the right to register this
   service against the certificate received in step 2.

5. Vehicle sends service announce to device<br>
   The command contains the service ```jlr.com/vin/ABCD/unlock```.<br>
   Device validates the registration against right to register services
   listed in certificate received in step 3.

6. Device sends service announce to vehicle<br>
   The command contains the service ```jlr.com/mobile/1234/status```.<br>
   Vehicle validates the registration against right to register services
   listed in certificate received in step 2.

7. Vehicle invokes ```jlr.com/mobile/1234/status``` on device<br>
   The command, signed by the vehicle private key, contains current
   state (locked/unlocked, etc) that is used to update device UI.<br>
   The device validates the signature using the public key in
   the certificate transmitted in step 3.<br>
   The device updates its status with the received state.


8. Device invokes ```jlr.com/vin/ABCD/unlock``` on vehicle<br>
   The command, signed by the device private key, tells the
   vehicle to unlock its doors.<br>
   The vehicle validates the signature using the public key in
   the certificate transmitted in step 2.<br>
   The vehicle unlocks the doors.

### Thwarting malicious RVI nodes - Illegal service invocation

1. [standard session setup]<br>

2. Device sends authenticate command to server<br>
   The command contains the device key together with a certificate showing
   that the device has the right to register register ```jlr.com/mobile/1234/receive_bitcoin```.

3. [server validates and responds with its own authenticate]<br>

4. Device sends false service announce to server<br>
   The commands contains the service ```jlr.com/mobile/9999/receive_bitcoin```.

5. Server rejects the service announce<br>
   Since the announced service does not match the service in the
   certificate received in step 2, the announcement is rejected and no
   invocations to ```jlr.com/mobile/9999/receive_bitcoin``` will be routed to
   device.

### Thwarting malicious RVI nodes - Stolen certificates
1. [standard session setup]<br>

2. Device sends authenticate command to server<br>
   The command contains the root-signed public device key together
   with a *stolen* certificate, also root signed, showing that the device has the right
   to register register ```jlr.com/mobile/1234/receive_bitcoin```.<br>

3. Server fails to validate certificate<br>
   Server tries to match public key in stolen, root signed certificate against the
   root signed public key in the authenticate, and fails.<br>
   Server disconnects.
