/*
  Copyright (C) 2014, Jaguar Land Rover

  This program is licensed under the terms and conditions of the
  Mozilla Public License, version 2.0.  The full text of the 
  Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
*/
#include "rvi.h"

struct rvi_t {
    uint32_t placeholder; /* Not yet implemented */
};

int32_t rvi_init(void)
{
    return RVI_OK;
}

int32_t rvi_cleanup(void)
{
    return RVI_OK;
}



struct rvi_t* rvi_new(char* service_prefix,
		      void (*service_available_cb)(struct rvi_t* rvi, char* service_name),
		      void (*service_unavailable_cb)(struct rvi_t* rvi, char* service_name))
{
    return RVI_OK;
}



int32_t rvi_delete(struct rvi_t* rvi)
{
    return RVI_OK;
}



int32_t rvi_set_user_data(struct rvi_t* rvi, void* user_data)
{
    return RVI_OK;
}


void* rvi_get_user_data(struct rvi_t* rvi)
{
    return RVI_OK;
}



int32_t rvi_add_configuration(struct rvi_t* rvi, char* key, char* value)
{
    return RVI_OK;
}


int32_t rvi_add_public_key(struct rvi_t* rvi, char* id, char* public_key)
{
    return RVI_OK;
}


int32_t rvi_add_certificates(struct rvi_t* rvi, 
			     char* certificate,
			     char* signature,
			     char *public_key_id)
{
    return RVI_OK;
}



int32_t rvi_register_service(struct rvi_t* rvi, char* local_name, 
			     char* full_service_name, uint32_t full_service_name_sz,
			     int32_t (*process_message_cb)(struct rvi_t*, char* service_name, 
							     uint32_t trans_id,
							     uint8_t* data, uint32_t data_sz))
{
    return RVI_OK;
}



int32_t rvi_activate(struct rvi_t* rvi)
{
    return RVI_OK;
}

int32_t rvi_deactivate(struct rvi_t* rvi)
{
    return RVI_OK;
}


int32_t rvi_get_descriptor(struct rvi_t* rvi, int32_t *descriptor)
{
    return RVI_OK;
}


int32_t rvi_process(struct rvi_t* rvi)
{
    return RVI_OK;
}


int32_t rvi_send_message(struct rvi_t*rvi, 
			 char* service, 
			 time_t timeout, 
			 uint32_t trans_id, 
			 uint8_t* data, uint32_t data_sz,
			 void (*reply_cb)(struct rvi_t*rvi, 
					  uint32_t trans_id,
					  int32_t result,
					  uint8_t* data, 
					  uint32_t data_sz))
{
    return RVI_OK;
}


int32_t rvi_reply(struct rvi_t*rvi, uint32_t trans_id,  
		  uint8_t* data, uint32_t data_sz)
{
    return RVI_OK;
}



