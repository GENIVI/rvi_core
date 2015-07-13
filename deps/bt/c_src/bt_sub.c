//
// Subscription handling
//

#include <stdio.h>
#include <string.h>

#include "dthread/include/dlog.h"
#include "bt_sub.h"

#define alloc_type(type) calloc(1, sizeof(type))

static char* format_subscription_type(subscription_type_t type)
{
    switch(type) {
    case INQUIRY: return "INQUIRY";
    case REMOTE_NAME: return "REMOTE_NAME";
    case CONNECT: return "CONNECT";
    case SDP_QUERY: return "SPD_QUERY";
    case SDP: return "SDP";
    case RFCOMM: return "RFCOMM";
    case RFCOMM_LISTEN: return "RFCOMM_LISTEN";
    case L2CAP: return "LPCAP";
    case L2CAP_LISTEN: return "LPCAP_LISTEN";
    default: return "????";
    }
}

char* format_subscription(subscription_t* s)
{
    static char format_buf[128];

    snprintf(format_buf, sizeof(format_buf), 
	     "%s id=%u,handle=%p ref=%u", 
	     format_subscription_type(s->type),
	     s->id, s->handle, s->ref);
    return format_buf;
}


subscription_t* new_subscription(subscription_type_t type,
				 uint32_t id, uint32_t cmdid,
				 void* handle,
				 void (*cleanup)(subscription_t* s))
{
    subscription_t* s = alloc_type(subscription_t);
    s->type = type;
    s->id = id;
    s->cmdid = cmdid;
    s->handle = handle;
    s->opaque = NULL;
    s->ref = 0;
    s->accept = NULL;
    s->cleanup = cleanup;
    return s;
}

subscription_list_t* new_subscription_list()
{
    subscription_list_t* list = alloc_type(subscription_list_t);
    return list;
}

subscription_t* retain_subscription(subscription_t* s)
{
    if (s == NULL) {
	DEBUGF("retain_subscription: NULL");
    }
    else {
	DEBUGF("retain_subscription: %s", format_subscription(s));
	s->ref++;
    }
    return s;
}

subscription_link_t* new_subscription_link(subscription_list_t* list,
					   subscription_t* s)
{
    subscription_link_t* link = alloc_type(subscription_link_t);
    if (link) {
	link->list = list;
	link->s    = retain_subscription(s);
    }
    return link;
}

subscription_t* release_subscription(subscription_t* s)
{
    DEBUGF("release_subscription: %s", format_subscription(s));
    if (s->ref <= 1) {
	if (s->cleanup)
	    (*s->cleanup)(s);
	if (s->opaque != NULL)
	    free(s->opaque);
	free(s);
	return NULL;
    }
    else  {
	s->ref--;
	return s;
    }
}

subscription_link_t* insert_after_link(subscription_link_t* link,
				       subscription_link_t* after_link)
{
    subscription_list_t* list = after_link->list;
    link->next = after_link->next;
    link->prev = after_link;
    link->list = list;
    after_link->next = link;
    if (link->next == NULL)
        list->last = link;
    else
        link->next->prev = link;
    list->length++;
    return link;
}

subscription_link_t* insert_before_link(subscription_link_t* link,
					subscription_link_t* before_link)
{
    if (before_link->prev != NULL)
        return insert_after_link(link, before_link->prev);
    else {
	subscription_list_t* list = before_link->list;
        link->next = before_link;
	link->list = list;
        before_link->prev = link;
        list->first = link;
	list->length++;
    }
    return link;
}

int insert_link_first(subscription_link_t* link)
{
    if (link != NULL) {
	subscription_list_t* list = link->list;
	link->next = list->first;
	link->prev = NULL;
	if (list->first != NULL)
	    list->first->prev = link;
	else
	    list->last = link;
	list->first = link;
	list->length++;
	return 0;
    }
    return -1;
}

int insert_first(subscription_list_t* list, subscription_t* s)
{
    subscription_link_t* link = new_subscription_link(list,s);
    return insert_link_first(link);
}

int insert_link_last(subscription_link_t* link)
{
    if (link != NULL) {
	subscription_list_t* list = link->list;
	link->next = NULL;
	link->prev = list->last;
	if (list->last)
	    list->last->next = link;
	else
	    list->first = link;
	list->last = link;
	list->length++;
	return 0;
    }
    return -1;
}


int insert_last(subscription_list_t* list, subscription_t*s)
{
    subscription_link_t* link = new_subscription_link(list,s);
    return insert_link_last(link);
}

subscription_link_t* find_subscription_link(subscription_list_t* list,
					    subscription_type_t type, 
					    uint32_t sid)
{
    subscription_link_t* p = list->first;
    DEBUGF("find_subscription_link: %s id=%u",
	   format_subscription_type(type), sid);
    while(p) {
	if ((p->s->id == sid) && (p->s->type == type))
	    return p;
	p = p->next;
    }
    return NULL;
}

subscription_t* find_subscription(subscription_list_t* list,
				  subscription_type_t type, 
				  uint32_t sid)
{
    subscription_link_t* p = find_subscription_link(list, type, sid);
    if (p != NULL)
	return p->s;
    return NULL;
}

void unlink_subscription(subscription_link_t* link)
{
    subscription_list_t* list = link->list;
    DEBUGF("unlink_subscription: %s", format_subscription(link->s));
    if (link->prev != NULL)
	link->prev->next = link->next;
    else
	list->first = link->next;

    if (link->next != NULL)
	link->next->prev = link->prev;
    else
	list->last = link->prev;
    list->length--;
    release_subscription(link->s);
    free(link);
}

int remove_subscription(subscription_list_t* list,
			subscription_type_t type, uint32_t sid)
{
    subscription_link_t* link;

    if ((link = find_subscription_link(list, type, sid)) != NULL) {
	unlink_subscription(link);
	return 1;
    }
    return 0;
}
