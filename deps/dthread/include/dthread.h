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
#ifndef __DTHREAD_H__
#define __DTHREAD_H__

struct _dthread_t;

#include "erl_driver.h"
#include "dterm.h"

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

#if (ERL_DRV_EXTENDED_MAJOR_VERSION > 2) || ((ERL_DRV_EXTENDED_MAJOR_VERSION == 2) && (ERL_DRV_EXTENDED_MINOR_VERSION >= 1))
#define DOUTPUT_TERM(thr, message, len) erl_drv_output_term((thr)->dport,(message),(len))
#define DSEND_TERM(thr, to, message, len) erl_drv_send_term((thr)->dport,(to),(message),(len))

#else
#define DOUTPUT_TERM(thr, message, len) driver_output_term((thr)->port,(message),(len))
#define DSEND_TERM(thr, to, message, len) driver_send_term((thr)->port,(to),(message),(len))
#endif


#ifdef __WIN32__
#include <windows.h>
#define DTHREAD_EVENT(e) ((HANDLE)(e))
#define DTHREAD_INVALID_EVENT  ((HANDLE)(-1))
#define DTHREAD_CLOSE_EVENT(e) CloseHandle(DTHREAD_EVENT(e))
#else
#define DTHREAD_EVENT(e) ((int)((long)(e)))
#define DTHREAD_INVALID_EVENT  ((long)(-1))
#define DTHREAD_CLOSE_EVENT(e) close(DTHREAD_EVENT(e))
#endif

// Builtin commands are negative, user command must > 0

#define DTHREAD_STOP          -1
#define DTHREAD_SEND_TERM     -2
#define DTHREAD_OUTPUT_TERM   -3
#define DTHREAD_OUTPUT        -4

typedef struct _dmessage_t
{
    struct _dmessage_t*  next;  // next message in queue
    int                   cmd;  // same as ctl 
    struct _dthread_t* source;  // sender thread
    void (*release)(struct _dmessage_t*);  // release hook
    ErlDrvTermData       from;  // sender pid
    ErlDrvTermData        to;   // receiver pid (if any)
    ErlDrvTermData        ref;  // sender ref
    void* udata;                // user data
    size_t size;          // total allocated size of buffer
    size_t used;          // total used part of buffer
    char*  buffer;        // points to data or allocated
    char   data[0];
} dmessage_t;

typedef struct _dthread_t {
    ErlDrvTid      tid;         // thread id
    void*          arg;         // thread init argument
    ErlDrvPort     port;        // port controling the thread
    ErlDrvTermData dport;       // the port identifier as DriverTermData
    ErlDrvTermData owner;       // owner process pid 
    ErlDrvTermData caller;      // last caller (driver_caller)
    ErlDrvTermData    ref;      // last sender ref
    int       smp_support;      // SMP support or not

    // Input queue
    ErlDrvMutex*   iq_mtx;       // message queue lock
    int            iq_len;       // message queue length
    dmessage_t*    iq_front;     // get from front
    dmessage_t*    iq_rear;      // put to rear
    
    ErlDrvEvent    iq_signal[2]; // event signaled when items is enqueued
} dthread_t;

#define ERL_DRV_EXCEP  (1 << 7)

typedef struct _dthread_poll_event_t {
    ErlDrvEvent event;
    int events;          // ERL_DRV_READ | WRITE
    int revents;         // ERL_DRV_READ | WRITE
} dthread_poll_event_t;

extern void dthread_lib_init(void);
extern void dthread_lib_finish(void);

extern dmessage_t* dmessage_alloc(size_t n);
extern void dmessage_free(dmessage_t* mp);
extern dmessage_t* dmessage_create_r(int cmd, 
				     void (*release)(dmessage_t*),
				     void* udata,
				     char* buf, size_t len);
extern dmessage_t* dmessage_create(int cmd,char* buf, size_t len);

extern void dthread_event_close(ErlDrvEvent);
extern int dthread_signal_set(dthread_t* thr);
extern int dthread_signal_reset(dthread_t* thr);

extern void dthread_signal_init(dthread_t* thr);
extern void dthread_signal_select(dthread_t* thr, int on);
extern void dthread_signal_use(dthread_t* thr, int on);
extern void dthread_signal_finish(dthread_t* thr, int and_close);

extern int dthread_poll(dthread_t* thr,
			dthread_poll_event_t* events,
			size_t* npevs, int timeout);

extern int dthread_send(dthread_t* thr, dthread_t* source,
			dmessage_t* mp);

extern int dthread_control(dthread_t* thr, dthread_t* source,
			   int cmd, char* buf, int len);
extern int dthread_output(dthread_t* thr, dthread_t* source,
			  char* buf, int len);

extern int dthread_port_send_dterm(dthread_t* thr, dthread_t* source, 
				   ErlDrvTermData target, dterm_t* p);
extern int dthread_port_output_dterm(dthread_t* thr, dthread_t* source, 
				     dterm_t* p);

extern int dthread_port_send_term(dthread_t* thr, dthread_t* source,
				  ErlDrvTermData target,
				  ErlDrvTermData* spec, int len);
extern int dthread_port_output_term(dthread_t* thr, dthread_t* source,
				    ErlDrvTermData* spec, int len);
extern int dthread_port_output(dthread_t* thr, dthread_t* source,
			       char* buf, int len);
extern int dthread_port_output2(dthread_t* thr, dthread_t* source,
				char* buf, int len,
				char* buf2, int len2);
extern int dthread_port_output_binary(dthread_t* thr, dthread_t* source,
				      char *hbuf, ErlDrvSizeT hlen,
				      ErlDrvBinary* bin,
				      ErlDrvSizeT offset, ErlDrvSizeT len);

// send {Ref::uint32(), ok}
extern int dthread_port_send_ok(dthread_t* thr, dthread_t* source,
				ErlDrvTermData target, ErlDrvTermData ref);
// send {Ref::uint32(), {error,Reason::atom()}}
extern int dthread_port_send_error(dthread_t* thr, dthread_t* source,
				   ErlDrvTermData target,
				   ErlDrvTermData ref, int error);


extern dmessage_t* dthread_recv(dthread_t* self, dthread_t** source);

extern int dthread_init(dthread_t* thr, ErlDrvPort port);
extern void dthread_finish(dthread_t* thr);
extern dthread_t* dthread_start(ErlDrvPort port,
				void* (*func)(void* arg),
				void* arg, int stack_size);
extern int dthread_stop(dthread_t* target, dthread_t* source, 
			void** exit_value);
extern void dthread_exit(void* value);

#endif
