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
//
// Test driver using dthread library
//

#include <stdio.h>

#include "erl_driver.h"
#include "../include/dlog.h"
#include "../include/dthread.h"

#include <ctype.h>
#include <stdint.h>
#include <memory.h>


#define DTHREAD_OK       0
#define DTHREAD_ERROR    1

typedef struct _drv_ctx_t
{
    dthread_t self;             // me
    dthread_t* other;           // the thread
} drv_ctx_t;

ErlDrvEntry dthread_drv_entry;

#ifdef DEBUG
#include <stdarg.h>

void emit_error(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    va_start(ap, line);
    fmt = va_arg(ap, char*);

    fprintf(stderr, "%s:%d: ", file, line); 
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\r\n");
    va_end(ap);
}
#endif

/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, char* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
	ptr = driver_alloc(len+1);
	*rbuf = ptr;
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}

//
// Main thread function
//
void* dthread_dispatch(void* arg)
{
    dthread_t* self = (dthread_t*) arg;
    dterm_t tsender;

    DEBUGF("dthread_drv: dthread_dispatch started");

    dterm_init(&tsender);
    
    while(1) {
	int r;

	r = dthread_poll(self, NULL, NULL, -1);
	if (r < 0) {
	    DEBUGF("dthread_drv: dthread_dispatch select failed=%d", r);
	    continue;
	}
	else if (r == 0) {
	    DEBUGF("dthread_drv: dthread_dispatch timeout");
	    continue;
	}
	else {
	    dmessage_t* mp;

	    DEBUGF("dthread_drv: dthread_dispatch r=%d", r);
	    if ((mp = dthread_recv(self, NULL)) == NULL) {
		DEBUGF("dthread_drv: message was NULL");
		continue;
	    }

	    switch(mp->cmd) {
	    case DTHREAD_STOP:
		DEBUGF("dthread_drv: dthread_dispatch STOP");
		dmessage_free(mp);
		dterm_finish(&tsender);
		dthread_exit(0);
		break;

	    case DTHREAD_OUTPUT:
		DEBUGF("dthread_drv: dthread_dispatch OUTPUT");
		break;

	    case 1:
		DEBUGF("dthread_drv: dthread_dispatch cmd=1");
		dthread_port_output(mp->source, self, "HELLO WORLD", 11);
		break;

	    case 2: {
		DEBUGF("dthread_drv: dthread_dispatch cmd=2");
		dterm_put2(&tsender, ERL_DRV_PORT, self->dport);
		dterm_put2(&tsender, ERL_DRV_ATOM, driver_mk_atom("data"));
		dterm_put3(&tsender, ERL_DRV_STRING,
			   (ErlDrvTermData) "NEW WORLD", (ErlDrvTermData) 9);
		dterm_put2(&tsender, ERL_DRV_TUPLE, 2);
		dterm_put2(&tsender, ERL_DRV_TUPLE, 2);

		dthread_port_send_dterm(mp->source, self, mp->from, &tsender);
		dterm_reset(&tsender);
		break;
	    }

	    case 3: {
		DEBUGF("dthread_drv: dthread_dispatch cmd=3");
		dterm_put2(&tsender, ERL_DRV_ATOM, driver_mk_atom("x"));
		dterm_put2(&tsender, ERL_DRV_ATOM, driver_mk_atom("y"));
		dterm_put2(&tsender, ERL_DRV_ATOM, driver_mk_atom("z"));
		dterm_put2(&tsender, ERL_DRV_TUPLE, 3);
		dthread_port_output_dterm(mp->source, self, &tsender);
		dterm_reset(&tsender);
		break;
	    }

	    case 100: {
		DEBUGF("dthread_dispatch cmd=100");
		if (mp->used == 4) {
		    uint8_t* ptr = (uint8_t*)mp->buffer;
		    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) |
			(ptr[2]<<8) | (ptr[3]<<0);
		    
		    // usleep(10);  unix only
		    dterm_put2(&tsender, ERL_DRV_UINT, mp->ref);
		    dterm_put2(&tsender, ERL_DRV_UINT, value + 1);
		    dterm_put2(&tsender, ERL_DRV_TUPLE, 2);
		    
		    dthread_port_send_dterm(mp->source,self,mp->from,&tsender);
		    dterm_reset(&tsender);
		}
		break;
	    }
		
	    default:
		DEBUGF("dthread_drv: dthread_dispatch cmd=%d", mp->cmd);
		break;
	    }
	    dmessage_free(mp);
	}
    }    
}


// setup global object area
// load atoms etc.

static int dthread_drv_init(void)
{
    DEBUGF("dthread_drv: driver init");
    dthread_lib_init();
    return 0;
}

// clean up global settings
static void dthread_drv_finish(void)
{
    DEBUGF("dthread_drv: finish");
    dthread_lib_finish();
}

static ErlDrvData dthread_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    drv_ctx_t* ctx;

    DEBUGF("dthread_drv: start");

    ctx = DALLOC(sizeof(drv_ctx_t));
    
    dthread_init(&ctx->self, port);

    ctx->other = dthread_start(port, dthread_dispatch, ctx, 4096);

    dthread_signal_use(&ctx->self, 1);
    dthread_signal_select(&ctx->self, 1);

    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

    return (ErlDrvData) ctx;
}


static void dthread_drv_stop(ErlDrvData d)
{
    drv_ctx_t* ctx = (drv_ctx_t*) d;
    void* value;

    DEBUGF("dthread_drv: stop");

    dthread_stop(ctx->other, &ctx->self, &value);

    dthread_signal_use(&ctx->self, 0);

    dthread_finish(&ctx->self);
    DFREE(ctx);
}

static ErlDrvSSizeT dthread_drv_control(ErlDrvData d, unsigned int cmd,
					char* buf, ErlDrvSizeT len,
					char** rbuf, ErlDrvSizeT rsize)
{
    drv_ctx_t* ctx = (drv_ctx_t*) d;
    char ref_buf[sizeof(uint32_t)];
    uint32_t r;

    DEBUGF("dthread_drv: ctl: cmd=%u, len=%d", cmd, len);

    ctx->self.caller = driver_caller(ctx->self.port);
    dthread_control(ctx->other, &ctx->self, cmd, buf, len);

    r = (uint32_t) ctx->self.ref;
    ref_buf[0] = r >> 24;
    ref_buf[1] = r >> 16;
    ref_buf[2] = r >> 8;
    ref_buf[3] = r;
    return ctl_reply(DTHREAD_OK, ref_buf, sizeof(ref_buf), rbuf, rsize);
}

static void dthread_drv_output(ErlDrvData d, char* buf, ErlDrvSizeT len)
{
    drv_ctx_t*   ctx = (drv_ctx_t*) d;

    DEBUGF("dthread_drv: output");

    ctx->self.caller = driver_caller(ctx->self.port);
    dthread_output(ctx->other, &ctx->self, buf, len);
}

static void dthread_drv_timeout(ErlDrvData d)
{
    (void) d;
    // drv_ctx_t* ctx = (drv_ctx_t*) d;
    DEBUGF("dthread_drv: output");
}

static void dthread_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
    drv_ctx_t* ctx = (drv_ctx_t*) d;

    if (ctx->self.iq_signal[0] == e) { // got input !
	dmessage_t* mp;

	DEBUGF("dthread_drv: ready_input handle=%d", 
	       DTHREAD_EVENT(ctx->self.iq_signal[0]));

	if (!(mp = dthread_recv(&ctx->self, NULL))) {
	    DEBUGF("dthread_drv: ready_input signaled with no event! handle=%d",
		   DTHREAD_EVENT(ctx->self.iq_signal[0]));
	    return;
	}

	switch(mp->cmd) {
	case DTHREAD_OUTPUT_TERM:
	    DEBUGF("dthread_drv: ready_input (OUTPUT_TERM)");
	    DOUTPUT_TERM(&ctx->self, (ErlDrvTermData*) mp->buffer,
			 mp->used / sizeof(ErlDrvTermData));
	    break;
	case DTHREAD_SEND_TERM:
	    DEBUGF("dthread_drv: ready_input (SEND_TERM)");
	    DSEND_TERM(&ctx->self, mp->to, /* orignal from ! */
		       (ErlDrvTermData*) mp->buffer,
		       mp->used / sizeof(ErlDrvTermData)); 
	    break;
	case DTHREAD_OUTPUT:
	    DEBUGF("dthread_drv: ready_input (OUTPUT)");
	    driver_output(ctx->self.port, mp->buffer, mp->used);
	    break;
	default:
	    DEBUGF("dthread_drv: read_input cmd=%d not matched",
		   mp->cmd);
	    break;
	}
	dmessage_free(mp);
    }
    else {
	DEBUGF("dthread_drv: ready_input (NO MATCH)");
    }
}

static void dthread_drv_ready_output(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
    DEBUGF("dthread_drv: read_output");
}

static void dthread_drv_stop_select(ErlDrvEvent event, void* arg)
{
    (void) arg;
    DEBUGF("dthread_drv: stop_select");
    DEBUGF("dthread_drv: close event=%d", DTHREAD_EVENT(event));
    dthread_event_close(event);
}


DRIVER_INIT(dthread_drv)
{
    ErlDrvEntry* ptr = &dthread_drv_entry;

    DEBUGF("DRIVER_INIT");

    memset(ptr, 0, sizeof(ErlDrvEntry));

    ptr->init  = dthread_drv_init;
    ptr->start = dthread_drv_start;
    ptr->stop  = dthread_drv_stop;
    ptr->output = dthread_drv_output;
    ptr->ready_input  = dthread_drv_ready_input;
    ptr->ready_output = dthread_drv_ready_output;
    ptr->finish = dthread_drv_finish;
    ptr->driver_name = "dthread_drv";
    ptr->control = dthread_drv_control;
    ptr->timeout = dthread_drv_timeout;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = dthread_drv_stop_select;
    return ptr;
}
