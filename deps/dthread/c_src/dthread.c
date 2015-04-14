/****** BEGIN COPYRIGHT *******************************************************
 *
 * Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
 *
 * This software is licensed as described in the file COPYRIGHT, which
 * you should have received as part of this distribution. The terms
 * are also available at http://www.rogvall.se/docs/copyright.txt.
 *
 * You may opt to use, copy, modify, merge, publish, distribute and/or sell
 * copies of the Software, and permit persons to whom the Software is
 * furnished to do so, under the terms of the COPYRIGHT file.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied.
 *
 ****** END COPYRIGHT ********************************************************/
/*
 * Reusable? API to handle commands from an
 * Erlang drivers in a thread. This thread can be used for
 * blocking operations towards operating system, replies go
 * either through port to process or straight to calling process depending
 * on SMP is supported or not.
 */

#include "../include/dthread.h"
#include "../include/dlog.h"


#ifdef __WIN32__
#include <windows.h>
#else
#include <unistd.h>
#include <sys/socket.h>
#endif

#include <stddef.h>
#include <stdio.h>
#include <ctype.h>

static ErlDrvTermData am_data;
static ErlDrvTermData am_ok;
static ErlDrvTermData am_error;

void dthread_lib_init()
{
    dterm_lib_init();
    am_data = driver_mk_atom("data");
    am_ok = driver_mk_atom("ok");
    am_error = driver_mk_atom("error");
}

void dthread_lib_finish()
{
    dterm_lib_finish();
}

/******************************************************************************
 *
 *   Messages
 *
 *****************************************************************************/

dmessage_t* dmessage_alloc(size_t n)
{
    size_t sz = sizeof(dmessage_t) + n;
    dmessage_t* mp = DZALLOC(sz);
    if (mp) {
	mp->buffer = mp->data;
	mp->used = 0;
	mp->size = n;
    }
    return mp;
}

void dmessage_free(dmessage_t* mp)
{
    if (mp->release)
	(*mp->release)(mp);
    if ((mp->buffer < mp->data) || (mp->buffer > mp->data+mp->size))
	DFREE(mp->buffer);
    DFREE(mp);
}

// create a message with optional dynamic buffer
dmessage_t* dmessage_create_r(int cmd, 
			      void (*release)(dmessage_t*),
			      void* udata,
			      char* buf, size_t len)
{
    dmessage_t* mp;
    if (release) {
	if ((mp = dmessage_alloc(0))) {
	    mp->cmd    = cmd;
	    mp->udata  = udata;
	    mp->release = release;
	    mp->buffer = buf;
	    mp->size   = len;
	    mp->used   = len;
	}
    }
    else {
	if ((mp = dmessage_alloc(len+8))) {
	    mp->cmd = cmd;
	    mp->udata = udata;
	    mp->buffer += 8;
	    memcpy(mp->buffer, buf, len);
	    mp->used = len;
	}
    }
    return mp;
}

// simple version of dmessage_create_r
dmessage_t* dmessage_create(int cmd,char* buf, size_t len)
{
    return dmessage_create_r(cmd, NULL, NULL, buf, len);
}

/******************************************************************************
 *
 *   Threads
 *
 *****************************************************************************/

extern void dthread_event_close(ErlDrvEvent event)
{
    INFOF("event_close: %d", DTHREAD_EVENT(event));
    DTHREAD_CLOSE_EVENT(event);
}

void dthread_signal_init(dthread_t* thr)
{
    thr->iq_signal[0] = (ErlDrvEvent) DTHREAD_INVALID_EVENT;
    thr->iq_signal[1] = (ErlDrvEvent) DTHREAD_INVALID_EVENT;
}

void dthread_signal_select(dthread_t* thr, int on)
{
    DEBUGF("dthread_signal_select: fd=%d", 
	   DTHREAD_EVENT(thr->iq_signal[0]));
#ifdef __WIN32__
    driver_select(thr->port,thr->iq_signal[0],ERL_DRV_READ,on);
#else
    driver_select(thr->port,thr->iq_signal[0],ERL_DRV_READ,on);
#endif
}

void dthread_signal_use(dthread_t* thr, int on)
{
#ifdef __WIN32__
    driver_select(thr->port,thr->iq_signal[0],ERL_DRV_USE,on);
#else
    driver_select(thr->port,thr->iq_signal[1],ERL_DRV_USE,on);
    driver_select(thr->port,thr->iq_signal[0],ERL_DRV_USE,on);
#endif
}


int dthread_signal_set(dthread_t* thr)
{
#ifdef __WIN32__
    DEBUGF("dthread_signal_set: handle=%d", DTHREAD_EVENT(thr->iq_signal[0]));
    SetEvent(DTHREAD_EVENT(thr->iq_signal[0]));
    return 1;
#else
    DEBUGF("dthread_signal_set: fd=%d", DTHREAD_EVENT(thr->iq_signal[1]));
    return write(DTHREAD_EVENT(thr->iq_signal[1]), "!", 1);
#endif
}

// consume wakeup token
int dthread_signal_reset(dthread_t* thr)
{
#ifdef __WIN32__
    DEBUGF("dthread_signal_reset: handle=%d", DTHREAD_EVENT(thr->iq_signal[0]));
    ResetEvent(DTHREAD_EVENT(thr->iq_signal[0]));
    return 0;
#else
    {
	char buf[1];
	DEBUGF("dthread_signal_reset: fd=%d", DTHREAD_EVENT(thr->iq_signal[0]));
	return read(DTHREAD_EVENT(thr->iq_signal[0]), buf, 1);
    }
#endif
}

void dthread_signal_finish(dthread_t* thr, int and_close)
{
    if (thr->iq_signal[0] != (ErlDrvEvent)DTHREAD_INVALID_EVENT) {
	if (and_close) {
	    DEBUGF("dthread_signal_finish: close iq_signal[0]=%d",
		   DTHREAD_EVENT(thr->iq_signal[0]));
	    DTHREAD_CLOSE_EVENT(thr->iq_signal[0]);
	}
	thr->iq_signal[0] = (ErlDrvEvent)DTHREAD_INVALID_EVENT;
    }

    if (thr->iq_signal[1] != (ErlDrvEvent)DTHREAD_INVALID_EVENT) {
	if (and_close) {
	    DEBUGF("dthread_signal_finish: iq_signal[1]=%d",
		   DTHREAD_EVENT(thr->iq_signal[1]));
	    DTHREAD_CLOSE_EVENT(thr->iq_signal[1]);
	}
	thr->iq_signal[1] = (ErlDrvEvent)DTHREAD_INVALID_EVENT;
    }
}


// Cleanup the mess
void dthread_finish(dthread_t* thr)
{
    dmessage_t* mp;

    if (thr->iq_mtx) {
	erl_drv_mutex_destroy(thr->iq_mtx);
	thr->iq_mtx = NULL;
    }
    mp = thr->iq_front;
    while(mp) {
	dmessage_t* tmp = mp->next;
	dmessage_free(mp);
	mp = tmp;
    }
    thr->iq_front = thr->iq_rear = NULL;
    dthread_signal_finish(thr, 0);
}

// Initialize thread structure
int dthread_init(dthread_t* thr, ErlDrvPort port)
{
    ErlDrvSysInfo sys_info;

    memset(thr, 0, sizeof(dthread_t));
    dthread_signal_init(thr);
    driver_system_info(&sys_info, sizeof(ErlDrvSysInfo));
    // smp_support is used for message passing from thread to
    // calling process. if SMP is supported the message will go
    // directly to sender, otherwise it must be sent to port 
    thr->smp_support = sys_info.smp_support;
    thr->port = port;
    thr->dport = driver_mk_port(port);
    thr->owner = driver_connected(port);

    if (!(thr->iq_mtx = erl_drv_mutex_create("iq_mtx")))
	return -1;
#ifdef __WIN32__
    // create a manual reset event
    if (!(thr->iq_signal[0] = (ErlDrvEvent)
	  CreateEvent(NULL, TRUE, FALSE, NULL))) {
	dthread_finish(thr);
	return -1;
    }
    DEBUGF("dthread_init: handle=%d", DTHREAD_EVENT(thr->iq_signal[0]));
#else
    {
	int pfd[2];
	if (pipe(pfd) < 0) {
	    dthread_finish(thr);
	    return -1;
	}
	DEBUGF("dthread_init: pipe[0]=%d,pidp[1]=%d", pfd[0], pfd[1]);
	thr->iq_signal[0] = (ErlDrvEvent) ((long)pfd[0]);
	thr->iq_signal[1] = (ErlDrvEvent) ((long)pfd[1]);
	INFOF("pipe: %d,%d", pfd[0], pfd[1]);
    }
#endif
    return 0;
}

dthread_t* dthread_start(ErlDrvPort port,
			 void* (*func)(void* arg),
			 void* arg, int stack_size)
{
    ErlDrvThreadOpts* opts = NULL;
    dthread_t* thr = NULL;

    if (!(thr = DALLOC(sizeof(dthread_t))))
	return 0;

    if (dthread_init(thr, port) < 0)
	goto error;

    if (!(opts = erl_drv_thread_opts_create("dthread_opts")))
	goto error;

    opts->suggested_stack_size = stack_size;
    thr->arg = arg;

    if (erl_drv_thread_create("dthread", &thr->tid, func, thr, opts) < 0)
	goto error;
    erl_drv_thread_opts_destroy(opts);
    return thr;

error:
    dthread_finish(thr);
    if (opts)
        erl_drv_thread_opts_destroy(opts);
    dthread_finish(thr);
    DFREE(thr);
    return 0;
}

int dthread_stop(dthread_t* target, dthread_t* source, 
		 void** exit_value)
{
    dmessage_t* mp;
    int r;

    if (!(mp = dmessage_create(DTHREAD_STOP, NULL, 0)))
	return -1;
    dthread_send(target, source, mp);

    DEBUGF("dthread_stop: wait to join");
    r = erl_drv_thread_join(target->tid, exit_value);
    DEBUGF("dthread_stop: thread_join: return=%d, exit_value=%p", r, *exit_value);

    dthread_signal_finish(target, 1);
    dthread_finish(target);
    DFREE(target);
    return 0;
}

void dthread_exit(void* value)
{
    erl_drv_thread_exit(value);
}

//
// Poll dthread queue and optionally other INPUT! events given by events
// number of input events are given in *nevents and number of ready
// events are also outputed there. 
//
// return -1  on error
//         0  timeout | no messages in queue (maybe in in nevents)
//         n  number of messages ready to be read (0 also means timeout!)
//
#ifdef __WIN32__

int dthread_poll(dthread_t* thr, dthread_poll_event_t* events, size_t* nevents, 
		 int timeout)
{
    HANDLE handles[MAXIMUM_WAIT_OBJECTS];
    int    eindex[MAXIMUM_WAIT_OBJECTS];
    DWORD  nCount = 0;
    DWORD  iq_len = 0;
    DWORD  nready = 0;
    DWORD  dwMilliseconds;
    DWORD  res;
    int    i,n;

    if (timeout < 0)
	dwMilliseconds = INFINITE;
    else
	dwMilliseconds = (DWORD) timeout;

    // install handles to wait for
    if (DTHREAD_EVENT(thr->iq_signal[0]) != DTHREAD_INVALID_EVENT) {
	eindex[nCount] = -1;  // -1 == signal queue event
	handles[nCount] = DTHREAD_EVENT(thr->iq_signal[0]);
	nCount++;
    }

    if (events && nevents && *nevents) {
	n = (DWORD) (*nevents);
	for (i = 0; (nCount < MAXIMUM_WAIT_OBJECTS) && (i < n); i++) {
	    events[i].revents = 0; // clear here in case of timeout etc
	    if (events[i].events) {
		eindex[nCount]  = i;  // index in event array
		handles[nCount] = DTHREAD_EVENT(events[i].event);
		nCount++;
	    }
	}
    }

    DEBUGF("WaitForMultipleObjects nCount=%d, timeout=%d", nCount, 
	   dwMilliseconds);
    // wait for first event that is signaled
    res = WaitForMultipleObjects(nCount, handles, FALSE, dwMilliseconds);
    DEBUGF("WaitForMultipleObjects result=%d", res);
    
    if (res == WAIT_TIMEOUT)
	return 0;
    else if (res == WAIT_FAILED)
	return -1;
    else if ((res >= WAIT_OBJECT_0) && (res < (WAIT_OBJECT_0+nCount))) {
	DWORD j = res - WAIT_OBJECT_0;
	
	if ((i = eindex[j]) < 0) {
	    erl_drv_mutex_lock(thr->iq_mtx);
	    iq_len = thr->iq_len;
	    erl_drv_mutex_unlock(thr->iq_mtx);
	}
	else if (events != NULL) {
	    events[i].revents |= ERL_DRV_READ;  // event is ready
	    nready++;
	}
	j++;

	// must scan rest of the events as well, else starvation may occure 
	while (j < nCount) {
	    if (WaitForSingleObject(handles[j], 0) == WAIT_OBJECT_0) {
		if ((i = eindex[j]) < 0) {
		    erl_drv_mutex_lock(thr->iq_mtx);
		    iq_len = thr->iq_len;
		    erl_drv_mutex_unlock(thr->iq_mtx);
		}
		else if (events != NULL) {
		    events[i].revents |= ERL_DRV_READ;  // event is ready
		    nready++;
		}		
	    }
	    j++;
	}
    }
    if (nevents)
	*nevents = nready;
    return iq_len;
}
#else

int dthread_poll(dthread_t* thr, dthread_poll_event_t* events, size_t* nevents, 
		 int timeout)
{
    struct timeval tm;
    struct timeval* tp;
    fd_set readfds;
    fd_set writefds;
    fd_set errorfds;
    int fd,nfds = 0;
    int ready;
    int i,n,iq_len=0;

    if (timeout < 0)
	tp = NULL;
    else {
	tm.tv_sec = timeout / 1000;
	tm.tv_usec = (timeout - tm.tv_sec*1000) * 1000;
	tp = &tm;
    }
    FD_ZERO(&readfds);
    FD_ZERO(&writefds);
    FD_ZERO(&errorfds);

    if ((fd = DTHREAD_EVENT(thr->iq_signal[0])) >= 0) {
	FD_SET(fd, &readfds);
	FD_SET(fd, &errorfds);
	DEBUGF("FD_SET: iq_signal[0] = %d", fd);
	if (fd > nfds) nfds = fd;
    }

    if (events && nevents && *nevents) {
	n = (int) (*nevents);
	for (i = 0; i < n; i++) {
	    events[i].revents = 0;  // clear here in case of timeout etc
	    if (events[i].events) {
		fd = DTHREAD_EVENT(events[i].event);
		if (events[i].events & ERL_DRV_READ) {
		    FD_SET(fd, &readfds);
		    FD_SET(fd, &errorfds);
		}
		if (events[i].events & ERL_DRV_WRITE)
		    FD_SET(fd, &writefds);
		if (fd > nfds) nfds = fd;
	    }
	}
    }

    DEBUGF("select nfds=%d, tp=%p", nfds, tp);
    ready = select(nfds+1, &readfds, &writefds, &errorfds, tp);
    DEBUGF("select result r=%d", ready);
    if (ready <= 0) {
	if (nevents)
	    *nevents = 0;
	return ready;
    }

    // check queue !
    fd = DTHREAD_EVENT(thr->iq_signal[0]);
    if (FD_ISSET(fd, &readfds)) {
	erl_drv_mutex_lock(thr->iq_mtx);
	iq_len = thr->iq_len;
	erl_drv_mutex_unlock(thr->iq_mtx);
	ready--;
    }

    // check io events
    if (ready && events && nevents && *nevents) {
	size_t nready = 0;
	n = (int) (*nevents);
	for (i = 0; ready && (i < n); i++) {
	    size_t fd_ready = 0;
	    fd = DTHREAD_EVENT(events[i].event);
	    if (FD_ISSET(fd, &readfds) || FD_ISSET(fd, &errorfds)) {
		events[i].revents |= ERL_DRV_READ;
		if (FD_ISSET(fd, &errorfds))
		    events[i].revents |= ERL_DRV_EXCEP;
		fd_ready = 1;
	    }
	    if (FD_ISSET(fd, &writefds)) {
		events[i].revents |= ERL_DRV_WRITE; 
		fd_ready = 1;
	    }
	    nready += fd_ready;
	    ready--;
	}
	*nevents = nready;
    }
    return iq_len;
}
#endif


int dthread_send(dthread_t* thr, dthread_t* source, dmessage_t* mp)
{
    dmessage_t* mr;
    int len;
    int r = 0;

    erl_drv_mutex_lock(thr->iq_mtx);

    mp->next = NULL;
    mp->source = source;

    if ((mr = thr->iq_rear) != NULL)
	mr->next = mp;
    else
	thr->iq_front = mp;
    thr->iq_rear = mp;
    len = ++thr->iq_len;
    if (len == 1)
	r = dthread_signal_set(thr);
    erl_drv_mutex_unlock(thr->iq_mtx);
    DEBUGF("dthread_send: iq_len=%d", len);
    return r;
}

dmessage_t* dthread_recv(dthread_t* thr, dthread_t** source)
{
    dmessage_t* mp;
    
    erl_drv_mutex_lock(thr->iq_mtx);
    if ((mp = thr->iq_front) != NULL) {
	if (!(thr->iq_front = mp->next))
	    thr->iq_rear = NULL;
	thr->iq_len--;
	if (thr->iq_len == 0)
	    dthread_signal_reset(thr);
    }
    erl_drv_mutex_unlock(thr->iq_mtx);

    if (mp && source)
	*source = mp->source;
    return mp;
}


int dthread_control(dthread_t* thr, dthread_t* source,
		    int cmd, char* buf, int len)
{
    dmessage_t* mp;

    if (!(mp = dmessage_create(cmd, buf, len)))
	return -1;
    mp->from = source->caller;
    mp->ref  = ++source->ref;
    return dthread_send(thr, source, mp);
}

int dthread_output(dthread_t* thr, dthread_t* source,
		   char* buf, int len)
{
    return dthread_control(thr, source, DTHREAD_OUTPUT, buf, len);
}

static void release_xptr_func(dmessage_t* mp)
{
    DEBUGF("release_xptr_func called");
    DFREE(mp->udata);
}

int dthread_port_send_term(dthread_t* thr, dthread_t* source, 
			   ErlDrvTermData target,
			   ErlDrvTermData* spec, int len)
{
    if (thr->smp_support)
	return DSEND_TERM(thr, target, spec, len);
    else {
	dmessage_t* mp;
	int xsz = dterm_dyn_size(spec, len);
	if (xsz < 0)
	    return -1;
	mp = dmessage_create(DTHREAD_SEND_TERM,(char*)spec,
			     len*sizeof(ErlDrvTermData));
	if (xsz > 0) {
	    char* xptr = DALLOC(xsz);
	    if ((dterm_dyn_copy((ErlDrvTermData*)mp->buffer, 
				len, xptr)) == NULL)
		return -1;
	    mp->udata = xptr;
	    mp->release = release_xptr_func;
	}
	mp->to = target;
	// dterm_dump(stderr, (ErlDrvTermData*) mp->buffer, mp->used / sizeof(ErlDrvTermData));
	return dthread_send(thr, source, mp);
    }
}

int dthread_port_output_term(dthread_t* thr, dthread_t* source, 
			     ErlDrvTermData* spec, int len)
{
    return dthread_port_send_term(thr, source, thr->owner, spec, len);
}

int dthread_port_send_dterm(dthread_t* thr, dthread_t* source, 
			    ErlDrvTermData target, dterm_t* p)
{
    return dthread_port_send_term(thr, source, target,
				  dterm_data(p),
				  dterm_used_size(p));
}

int dthread_port_output_dterm(dthread_t* thr, dthread_t* source, dterm_t* p)
{
    return dthread_port_send_dterm(thr, source, thr->owner, p);
}

// send {Ref, ok}
int dthread_port_send_ok(dthread_t* thr, dthread_t* source, 
			 ErlDrvTermData target, ErlDrvTermData ref)
{
    dterm_t t;
    dterm_mark_t m;
    int r;

    dterm_init(&t);
    dterm_tuple_begin(&t, &m); {
	dterm_int(&t, ref);
	dterm_atom(&t, am_ok);
    }
    dterm_tuple_end(&t, &m);
    
    r = dthread_port_send_term(thr, source, target,
			       dterm_data(&t), dterm_used_size(&t));
    dterm_finish(&t);
    return r;
}


static ErlDrvTermData error_atom(int err)
{
    char errstr[256];
    char* s;
    char* t;

    for (s = erl_errno_id(err), t = errstr; *s; s++, t++)
	*t = tolower(*s);
    *t = '\0';
    return driver_mk_atom(errstr);
}

// send {Ref, {error,Reason}}
int dthread_port_send_error(dthread_t* thr, dthread_t* source, 
			    ErlDrvTermData target,
			    ErlDrvTermData ref, int error)
{
    dterm_t t;
    dterm_mark_t m,e;
    int r;

    dterm_init(&t);
    dterm_tuple_begin(&t, &m); {
	dterm_int(&t, ref);
	dterm_tuple_begin(&t, &e); {
	    dterm_atom(&t, am_error);
	    dterm_atom(&t, error_atom(error));
	}
	dterm_tuple_end(&t,&e);
    }
    dterm_tuple_end(&t,&m);
	
    r = dthread_port_send_term(thr, source, target,
			       dterm_data(&t), dterm_used_size(&t));
    dterm_finish(&t);
    return r;    
}

//
// Generate output that looks like port data 
//
int dthread_port_output(dthread_t* thr, dthread_t* source, 
			char* buf, int len)
{
    // generate {Port, {data, Data}}
    ErlDrvTermData spec[11];
    
    spec[0] = ERL_DRV_PORT;
    spec[1] = thr->dport;
    spec[2] = ERL_DRV_ATOM;
    spec[3] = am_data;
    spec[4] = ERL_DRV_STRING; // ERL_DRV_BUF2BINARY;
    spec[5] = (ErlDrvTermData) buf;
    spec[6] = (ErlDrvTermData) len;
    spec[7] = ERL_DRV_TUPLE;
    spec[8] = 2;
    spec[9] = ERL_DRV_TUPLE;
    spec[10] = 2;

    return dthread_port_send_term(thr, source, thr->owner, spec, 11);
}

//
// Generate output that looks like port data 
//
int dthread_port_output2(dthread_t* thr, dthread_t* source, 
			 char* hbuf, int hlen,
			 char* buf, int len)

{
    // generate {Port, {data, Data}}
    int i = 0;
    ErlDrvTermData spec[16];
    
    spec[i++] = ERL_DRV_PORT;
    spec[i++] = thr->dport;
    spec[i++] = ERL_DRV_ATOM;
    spec[i++] = am_data;
    if (len == 0) {
	spec[i++] = ERL_DRV_STRING; // ERL_DRV_BUF2BINARY;
	spec[i++] = (ErlDrvTermData) hbuf;
	spec[i++] = (ErlDrvTermData) hlen;
    }
    else {
	spec[i++] = ERL_DRV_STRING; // ERL_DRV_BUF2BINARY;
	spec[i++] = (ErlDrvTermData) buf;
	spec[i++] = (ErlDrvTermData) len;
	if (hlen) {
	    spec[i++] = ERL_DRV_STRING_CONS;
	    spec[i++] = (ErlDrvTermData) hbuf;
	    spec[i++] = (ErlDrvTermData) hlen;
	}
    }
    spec[i++] = ERL_DRV_TUPLE;
    spec[i++] = 2;
    spec[i++] = ERL_DRV_TUPLE;
    spec[i++] = 2;

    return dthread_port_send_term(thr, source, thr->owner, spec, i);
}

//
// Generate output that looks like port data 
//
int dthread_port_output_binary(dthread_t* thr, dthread_t* source, 
			       char* hbuf, ErlDrvSizeT hlen,
			       ErlDrvBinary* bin, ErlDrvSizeT offset,
			       ErlDrvSizeT len)
{
    // generate {Port, {data, Data}}
    int i = 0;
    ErlDrvTermData spec[16];
    char* buf = bin->orig_bytes + offset;
    
    spec[i++] = ERL_DRV_PORT;
    spec[i++] = thr->dport;
    spec[i++] = ERL_DRV_ATOM;
    spec[i++] = am_data;
    if (len == 0) {
	spec[i++] = ERL_DRV_STRING;
	spec[i++] = (ErlDrvTermData) hbuf;
	spec[i++] = (ErlDrvTermData) hlen;
    }
    else {
	// FIXME select binary|list here!
        // ERL_DRV_STRING | ERL_DRV_BUF2BINARY;
	spec[i++] = ERL_DRV_STRING; 
	spec[i++] = (ErlDrvTermData) buf;
	spec[i++] = (ErlDrvTermData) len;
	if (hlen) {
	    spec[i++] = ERL_DRV_STRING_CONS;
	    spec[i++] = (ErlDrvTermData) hbuf;
	    spec[i++] = (ErlDrvTermData) hlen;
	}
    }
    spec[i++] = ERL_DRV_TUPLE;
    spec[i++] = 2;
    spec[i++] = ERL_DRV_TUPLE;
    spec[i++] = 2;
    return dthread_port_send_term(thr, source, thr->owner, spec, i);
}


