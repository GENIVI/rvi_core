#ifndef __BT_POLL_H__
#define __BT_POLL_H__

#include <poll.h>

typedef void (*event_cb_t)(struct pollfd*, void*);

extern int bt_poll_add(int fd, short events, event_cb_t cb, void* data);
extern int bt_poll_del(int fd);

extern int bt_poll_set_events(int fd, short events);
extern int bt_poll_set_cb(int fd, event_cb_t cb);
extern int bt_poll_set_data(int fd, void* data);

extern int bt_poll(int timeout);

#endif
