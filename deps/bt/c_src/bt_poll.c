//
//  Utility to handle multiple poll and callbacks
//

#include <stdio.h>
#include <stdlib.h>

#include "dthread/include/dlog.h"
#include "bt_poll.h"

static int            poll_sz  = 0;     // allocated size
static int            poll_n = 0;       // used len
static int            poll_fdmapsz = 0; // size of fdmap
static struct pollfd* poll_fds   = NULL;
static event_cb_t*    poll_cb    = NULL;
static void**         poll_data  = NULL;
static int*           poll_fdmap = NULL;  // fd -> poll_fds index

static inline int fd_to_pollix(int fd) 
{
    if ((fd < 0) || (fd >= poll_fdmapsz))
	return -1;
    return poll_fdmap[fd];
}

// change pollfd data
int bt_poll_set_events(int fd, short events)
{
    int i;
    if ((i = fd_to_pollix(fd)) < 0) return -1;
    poll_fds[i].events = events;
    return 0;
}

int bt_poll_set_cb(int fd, event_cb_t cb)
{
    int i;
    if ((i = fd_to_pollix(fd)) < 0) return -1;
    poll_cb[i] = cb;
    return 0;
}

int bt_poll_set_data(int fd, void* data)
{
    int i;
    if ((i = fd_to_pollix(fd)) < 0) return -1;
    poll_data[i] = data;
    return 0;
}

int bt_poll_add(int fd, short events,
		void (*cb)(struct pollfd* pfd,void* data), 
		void* data)
{
    int i = poll_n;
    if (i == poll_sz) {
	size_t new_sz = 2*poll_sz+1;
	poll_fds  = realloc(poll_fds, new_sz*sizeof(struct pollfd));
	if (poll_fds == NULL) return -1;
	poll_cb   = realloc(poll_cb, new_sz*sizeof(event_cb_t));
	if (poll_cb == NULL) return -1;
	poll_data = realloc(poll_data, new_sz*sizeof(void*));
	if (poll_data == NULL) return -1;
	poll_sz = new_sz;
    }
    if (fd >= poll_fdmapsz) {
	int new_sz = fd+64;  // add some extra
	int j;
	poll_fdmap = realloc(poll_fdmap, new_sz*sizeof(int));
	if (poll_fdmap == NULL) return -1;
	for (j = poll_fdmapsz; j < new_sz; j++)
	    poll_fdmap[j] = -1;
	poll_fdmapsz = new_sz;
    }
    poll_fdmap[fd] = i;
    poll_cb[i] = cb;
    poll_data[i] = data;
    poll_fds[i].fd = fd;
    poll_fds[i].events = events;
    poll_n++;
    return 0;
}

static void bt_poll_swap(int i, int j)
{
    if (i != j) {
	int fdi = poll_fds[i].fd;
	int fdj = poll_fds[j].fd;
	struct pollfd fds = poll_fds[i];
	event_cb_t cb     = poll_cb[i];
	void* data        = poll_data[i];

	poll_fds[i]  = poll_fds[j];
	poll_cb[i]   = poll_cb[j];
	poll_data[i] = poll_data[j];
	
	poll_fds[j]  = fds;
	poll_cb[j]   = cb;
	poll_data[j] = data;
	// remap indices
	poll_fdmap[fdi] = j;
	poll_fdmap[fdj] = i;
    }
}

// find and remove the pollfd
int bt_poll_del(int fd)
{
    int i;

    if ((i = fd_to_pollix(fd)) < 0) return -1;
    bt_poll_swap(i, poll_n-1);
    poll_n--;
    poll_fdmap[fd] = -1;  // not mapped any more
    return 0;
}

int bt_poll(int timeout)
{
    int r,r0;

    r = r0 = poll(poll_fds, poll_n, timeout);
    if (r > 0) {
	int i = 0;
	while((r > 0) && (i < (int)poll_n)) {
	    if ((poll_fds[i].revents & poll_fds[i].events) != 0) {
		int j = poll_n-1;
		// swap i and the last item, this makes the poll set less
		// sensitive to starvation, the bt_poll_del may also be
		// called in the callback without wory, note that 
		// you can not del any fd in a callback and expect to 
		// get a poll on all fds in the same loop (yet)
		bt_poll_swap(i, j);
		(*poll_cb[j])(&poll_fds[j], poll_data[j]);
		r--;
	    }
	    else
		i++;
	}
    }
    return r0;
}
