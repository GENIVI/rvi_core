#ifndef __BT_SUB_H__
#define __BT_SUB_H__

#include <stdlib.h>
#include <stdint.h>

#include "dthread/include/ddata.h"

typedef enum { 
    INQUIRY,
    REMOTE_NAME,
    CONNECT,
    SDP_QUERY,
    SDP,
    RFCOMM,
    RFCOMM_LISTEN,
    L2CAP,
    L2CAP_LISTEN
} subscription_type_t;

typedef struct _subscription_t
{
    uint32_t              ref;       // ref count
    subscription_type_t   type;      // type of subscription
    uint32_t              id;        // subscription id
    uint32_t              cmdid;     // current async cmdid (if any)
    void*                 handle;    // Bluetooth object handle
    void*                 opaque;    // subscription data
    ddata_t*              out;       // send buffer
    void (*cleanup)(struct _subscription_t* s); // clean up callback
    struct _subscription_t* accept;  // if on accept list
} subscription_t;

typedef struct _subscription_link_t
{
    struct _subscription_list_t* list;
    struct _subscription_link_t* next;
    struct _subscription_link_t* prev;
    subscription_t* s;
} subscription_link_t;

typedef struct _subscription_list_t
{
    subscription_link_t* first;
    subscription_link_t* last;
    size_t length;
} subscription_list_t;

extern char* format_subscription(subscription_t* s);
extern subscription_t* new_subscription(subscription_type_t type,
					uint32_t id, uint32_t cmdid,
					void* handle,
					void (*cleanup)(subscription_t* s));
extern subscription_list_t* new_subscription_list(void);
extern subscription_t* retain_subscription(subscription_t* s);
extern subscription_link_t* new_subscription_link(subscription_list_t* list,
						  subscription_t* s);
extern void free_subscription(subscription_t* s);
extern subscription_t* release_subscription(subscription_t* s);
extern subscription_link_t* insert_after_link(subscription_link_t* link,
					      subscription_link_t* after_link);
extern subscription_link_t* insert_before_link(subscription_link_t* link,
					       subscription_link_t* before_link);
extern int insert_link_first(subscription_link_t* link);
extern int insert_first(subscription_list_t* list, subscription_t* s);
extern int insert_link_last(subscription_link_t* link);
extern int insert_last(subscription_list_t* list, subscription_t*s);
extern subscription_link_t* find_subscription_link(subscription_list_t* list,
						   subscription_type_t type, 
						   uint32_t sid);
extern subscription_t* find_subscription(subscription_list_t* list,
					 subscription_type_t type, 
					 uint32_t sid);
extern void unlink_subscription(subscription_link_t* link);
extern int remove_subscription(subscription_list_t* list,
			       subscription_type_t type, uint32_t sid);





#endif
