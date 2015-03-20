/*
  Copyright (C) 2014, Jaguar Land Rover

  This program is licensed under the terms and conditions of the
  Mozilla Public License, version 2.0.  The full text of the 
  Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
*/

/*
  Header file for native C RVI node.

  --------------
  We will start off this C code as a wrapper around JSON-RPC so that
  we can talk to a full-fledged erlang RVI node.

  Once we mature, we will upgrade this code to become a complete,
  stripped-down RVI node in its own right.  The documentation below
  describes the complete, end goal C implementation.
  --------------

  This header file describes the C API for an RVI node (C-RVI) with
  reduced functionality. The following functionality are stripped out
  of (C-RVI) in order to reduce its foot print.

  1) Scheduling
  No store and forward is available. If a message is sent to a service
  that is currently not available, an error message is returned to the caller.

  2) Single protocol
  Only one protocol at the time is supported at data link level.
  A protocol is loaded from an shared object (or is linked in to the library).
  This protocol is the only one supported for as long as the library is in use.

*/
#include <stdint.h>
#include <time.h>

/* Return codes */
#define RVI_OK 0
#define RVI_ARRAY_OVERFLOW 1
#define RVI_PARAMETER 2

struct rvi_t;


/* Init the rvi library 
 
   PARAMETERS:
   N/A

   RETURNS:
   RVI_OK  Call successful

*/
    // IMPLEMENT
int32_t rvi_init(void);

/* Cleanup the rvi library after the last rvi_t* struct has been deleted 
 
   PARAMETERS:
   N/A

   RETURNS:
   RVI_OK  Call successful
   
*/
    // IMPLEMENT
int32_t rvi_cleanup(void);


/* Initialize a new rvi_t struct and return it.

   'service_prefix' is a string that will be used
   to prefix all services registered with rvi_register_service().

   If the service_prefix string is set to "jlr.com/vin/1234/", 
   and a service named "sota/install_package" is registered, the full
   service name will be "jlr.com/vin/1234/sota/install_package"

   Call rvi_delete() to delete the struct that is created and returned
   by rvi_new().

   PARAMETERS:
   service_prefix          The string to prefix all registered services with.

   service_available_cb    Callback to invoke when a new service
                           becomes available for invocation through
                           rvi_send_message().

   service_unavailable_cb  Callback to invoke when a previously available service
                           becomes unavailable for invocation through
                           rvi_send_message().

   RETURNS:
   Pointer to new rvi_t struct. 
*/
    // IMPLEMENT - But ignore callbacks for now.
struct rvi_t* rvi_new(char* service_prefix,
		      void (*service_available_cb)(struct rvi_t* rvi, char* service_name),
		      void (*service_unavailable_cb)(struct rvi_t* rvi, char* service_name));


/* Delete a previously created rvi_t struct

   Frees all resources occupied by 'rvi', including keys, configs, and
   certificates.

   'rvi' cannot be referenced after this function returns.

   PARAMETERS:
   rvi     The RVI struct, returned by rvi_new(), that is to be deleted.

   RETURNS:
   RVI_OK  Call successful

*/
    // IMPLEMENT
int32_t rvi_delete(struct rvi_t* rvi);


/* Set user data for an rvi_t struct.

   Associate the pointer in 'user_data' with 'rvi'. The provided pointer
   can be retrieved with get_user_data().

   PARAMETERS:
   rvi        The RVI struct, returned by rvi_new(), that is to have user
              data associated with it.

   user_data  The data to associate with 'rvi'

   RETURNS:
   RVI_OK   Call successful

*/
    // IMPLEMENT
int32_t rvi_set_user_data(struct rvi_t* rvi, void* user_data);

/* Get user data for an rvi_t struct.


   Return the pointer, previously registered with rvi_set_user_data(), associated
   with 'rvi'.  If rvi_set_user_data() has not been called for 'rvi', NULL will
   be returned.

   PARAMETERS:
   rvi      The RVI struct, returned by rvi_new(), that is to have user
            data associated with it.


   RETURNS:
   User data, or NULL if rvi_set_user_data() has not been called for 'rvi'


*/
    // IMPLEMENT
void* rvi_get_user_data(struct rvi_t* rvi);


/* Add configuration data to the rvi_t struct
      
   The given key/value pair is processed and integrated into
   the configuration data of 'rvi'.
   
   Ownership of 'key' and 'value' arguments are *not* transfered to
   the 'rvi'. These two arguments must be freed by the caller.

   PARAMETERS:
   rvi        The RVI struct returned by rvi_new().

   key        The name of the configuration entry to add.

   value      The value of the configuration entry to add.

   RETURNS:
   RVI_OK         Call successful.
   RVI_PARAMETER  One or more config entries could not be processed.

*/
    // IMPLEMENT
    // Things that we need to configure for JSON-RPC wrapper:
    // The URL that we listen to incoming JSON-RPC calls on (maybe just a port)
    // The URL of the RVI node that we are talking to
int32_t rvi_add_configuration(struct rvi_t* rvi, char* key, char* value);


/* Add a public key to the rvi_t struct
      
   The provided public key is processed and integrated into 'rvi'. The
   key will be used to validate the certificates attached to incoming
   transactions.

   Ownership of 'public_key' argument is *not* transfered to the
   'rvi'. This argument must be freed by the caller.

   PARAMETERS:
   rvi                   The RVI struct returned by rvi_new().

   id                    A system-wide unique ID for the public key. 

   public_key            Public key string.


   RETURNS:
   RVI_OK         Call successful.
   RVI_PARAMETER  One or more public keys could not be processed.
*/
    // DO NOT IMPLEMENT
int32_t rvi_add_public_key(struct rvi_t* rvi, char* id, char* public_key);

/* Add a certificate to the rvi_t structnn
      
   The certificate is added as a clear text JSON string, signed by the
   appropriate private key. See RVI HLD, rev A, chapter 7.2 for details
   on the certificate format.

   The signature for the certificate is created using a private key where
   the corresponsing public key has been added to the system
   using the rvi_add_public_key() function. 

   None of the provided arguments to rvi_add_certificates() will have
   their ownership transfered to 'rvi'. All arguments must be freed by
   the caller.

   PARAMETERS:
   rvi            The RVI struct returned by rvi_new().

   certificate    The JSON-RPC string containing the certificate. 

   signature      The signature, created by the private

   public_key_id  The ID of the public key, registered with
                  rvi_add_public_key(), whose private counterpart
		  created the signature.

   RETURNS:
   RVI_OK         Call successful.
   RVI_PARAMETER  One or more certificates could not be processed.
*/
    // DO NOT IMPLEMENT
int32_t rvi_add_certificates(struct rvi_t* rvi, 
			     char* certificate,
			     char* signature,
			     char *public_key_id);


/* Register a service available for callback 

  
   The 'service_name' will be appended to the service prefix specified
   for 'rvi' and then announced to the rest of the RVI network
   as being available to handle transactions.

   The full service name (prefix + name) is returned in 'full_service_name'.
   
   When a message is received for the given service,
   'process_message_cb' will be invoked with the following paramters:

     rvi           Same rvi struct as that provided to rvi_register_service()

     service_name  Full service name that triggered the callback.

     trans_id      If 0, no reply expected. If != 0, a reply is
                   expected, with the given trans_id provided,
                   in rvi_send_reply().
     
     data          The data to provide as parameter to the service.

     data_sz       Number of bytes in data.

   'process_message_cb' is expected to return something from the
   RVI_XXX result suite.
   

   PARAMETERS:
   rvi       Te RVI struct returned by rvi_new().
     
   local_service_name    Local service name to append to the service name
                         prefix for 'rvi'.
 
   full_service_name     Will be filled out with the full service name
                         of the registered service.
 
   full_service_name_sz  Maximum number of bytes to write to
                         full_service_name.
                        
   trans_id              If 0, no reply expected. If != 0, a reply is expected,
                         with the given trans_id provided, in rvi_send_reply().
     
   data                  The data to provide as parameter to the service.

   data_sz               Number of bytes in data.


   
   
   RETURNS:
   RVI_OK              Call succesful.

   RVI_ARRAY_OVERFLOW  The full serivce name is longer than the number of bytes
                       specified by 'full_service_name_sz'.

*/
    // IMPLEMENT - But ignore trans_id
int32_t rvi_register_service(struct rvi_t* rvi, char* local_name, 
			     char* full_service_name, uint32_t full_service_name_sz,
			     int32_t (*process_message_cb)(struct rvi_t*, char* service_name, 
							   uint32_t trans_id,
							   uint8_t* data, uint32_t data_sz));



/* Activate an rvi_t struct

   This function is called to activate 'rvi' once it has been configured with
   rvi_register_service(),  rvi_add_certificates(), rvi_add_public_keys(), 
   and rvi_add_configuration().
   
   When this call is returns, the callbacks provided during the setup
   of 'rvi' may be invoked at any time.

   PARAMETERS:
   rvi         The RVI struct returned by rvi_new().

   RETURNS:
   RVI_OK       Call successful.

   RVI_NETOWRK  A network error ocurred during activation. Check
                errno.
*/
    // IMPLEMENT - Fire up listening socket and accept incoming
    //             JSON-RPC traffic from RVI node
int32_t rvi_activate(struct rvi_t* rvi);




/* Dectivate an rvi_t struct

   This function is called to deactivate 'rvi', previously activated through
   rvi_activate().
   
   When this call is returns, no more callbacks will be invoked
   by the 'rvi', and any active network ports will be closed.

   PARAMETERS:
   rvi      The RVI struct returned by rvi_new().

   RETURNS:
   RVI_OK   Call successful.

*/
    // IMPLEMENT
int32_t rvi_deactivate(struct rvi_t* rvi);



/* Return the file descriptor associated with an rvi_t struct.

   This function will set the provided 'descriptor' parameter
   that was created by rvi_activate().
   
   The returned descriptor should be rolled into a
   poll()/epoll()/select() loop for read ready. (We will do non
   blocking write ready later on.) 

   When given descriptor is marked as read ready, rvi_process()
   should be invoked as soon as possible to read and process
   incoming data from other RVI nodes.

   Please note that this rvi_get_descriptor() must be called after
   every rvi_activate() invocation for the given 'rvi' struct to get
   the fresh descriptor created by the activation.

   PARAMETERS:
   rvi           The RVI struct returned by rvi_new().

   descriptor    Pointer to the integer to set to the descriptor.
   
   RETURNS:
   RVI_OK        Call successful.

   RVI_INACTIVE  The 'rvi' struct has not been activated with
                 rvi_activate().

*/
    // IMPLEMENT
int32_t rvi_get_descriptor(struct rvi_t* rvi, int32_t *descriptor);



/* Process any pending network traffic for the RVI node

   This call should only be invokd after the descriptor returned
   by an rvi_get_descriptor() invocation is marked as ready to be read.
   If that is not done, rvi_process() will wait until data is received.

   Any incoming data processed by rvi_process() may trigger callbacks
   to the function pointers provided to rvi_new() and rvi_register_service().
   Once these functions returns, rvi_process() will return to its caller as well.

   The callbacks are, in other words, synchronous and processed byu the 
   rvi_process()-calling thread.
   
   
*/
    // IMPLEMENT
int32_t rvi_process(struct rvi_t* rvi);



/* Send a message to a service

   This call will invoke the given service executing on the local or a
   remote RVI node. If 'reply_cb' is set, a reply is expected by the
   invoked (remote) service. The reply will be delivered by invoking the 
   'reply_cb' with the following arguments:

   rvi       Same rvi struct as that provided to rvi_send_message()

   result    RVI_OK - Reply was delivered. Check data/data_sz.
             RVI_TIMEOUT - Transaction timed out.
             RVI_UNKNOWN - Unknown error

   trans_id  Same value as the argument provided to
             rvi_send_message().
     
   data      The data sent back by the invoked service with the reply

   data_sz   Number of bytes in data.



   PARAMETERS:
   rvi       The RVI struct returned by rvi_new().

   service   Full name of the service to invoke.

   timeout   UTC for when this message must be delivered by. If that does not 
   happen, the message is dropped. In that case, if reply_cb is set,
   it will be invoked with the given 'rvi' and 'trans_id' with
   'result' set to 'timeout'.

   trans_id  Id to send back with 'reply_cb' when it is invoked.

   data      Data to provide to the invoked service.

   data_sz   Number of bytes in 'data'.

   reply_cb  Callback to invoke on error or reply. Set to 0 if no
             reply is wanted.

   RETURNS:
   RVI_OK          Call successful.

   RVI_PARAMETER   One or more config entries could not be processed.
     
*/
    // IMPLEMENT - But ignore trans_id and reply_cb.
int32_t rvi_send_message(struct rvi_t*rvi, 
			 char* service, 
			 time_t timeout, 
			 uint32_t trans_id, 
			 uint8_t* data, uint32_t data_sz,
			 void (*reply_cb)(struct rvi_t*rvi, 
					  uint32_t trans_id,
					  int32_t result,
					  uint8_t* data, 
					  uint32_t data_sz));


/* Send a reply back to an originating service

   This function, which should only be invoked by the function 
   pointed to by the 'process_message_cb' argument provided to
   rvi_register_service(), will send back a reply to the service
   originating a message.
   
   
   PARAMETERS:

   rvi        The same rvi_t struct as provided to the 'process_message_cb'
              function.

   trans_id   The same transaction id as provided to the
              'process_message_cb' function.

   data       Data to send back with the reply

   data_sz    Number of bytes in 'data'.

   RETURNS:
   RVI_OK     Call successful.

*/
    // DO NOT IMPLEMENT
int32_t rvi_reply(struct rvi_t*rvi, uint32_t trans_id,  
		  uint8_t* data, uint32_t data_sz);

