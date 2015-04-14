//
// uart_drv.c
//
//   Windows/Unix uart driver
//
//
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>

#include "uart_drv.h"

typedef struct _drv_ctx_t
{
    dthread_t self;             // me
    dthread_t* other;           // the thread
} drv_ctx_t;

static int  uart_drv_init(void);
static void uart_drv_finish(void);
static void uart_drv_stop(ErlDrvData);
static void uart_drv_output(ErlDrvData, char*, ErlDrvSizeT);
static void uart_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void uart_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData uart_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT uart_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**, ErlDrvSizeT);
static void uart_drv_timeout(ErlDrvData);
static void uart_drv_stop_select(ErlDrvEvent, void*);

ErlDrvTermData am_dtr;
ErlDrvTermData am_rts;
ErlDrvTermData am_cts;
ErlDrvTermData am_cd;
ErlDrvTermData am_ri;
ErlDrvTermData am_dsr;
ErlDrvTermData am_sw;

ErlDrvTermData am_device;
ErlDrvTermData am_baud;
ErlDrvTermData am_ibaud;
ErlDrvTermData am_obaud;
ErlDrvTermData am_csize;
ErlDrvTermData am_bufsz;
ErlDrvTermData am_buftm;
ErlDrvTermData am_stopb;
ErlDrvTermData am_parity;
ErlDrvTermData am_iflow;
ErlDrvTermData am_oflow;
ErlDrvTermData am_xoffchar;
ErlDrvTermData am_xonchar;
ErlDrvTermData am_eolchar;
ErlDrvTermData am_active;
ErlDrvTermData am_delay_send;
ErlDrvTermData am_deliver;
ErlDrvTermData am_mode;
ErlDrvTermData am_header;
ErlDrvTermData am_packet;
ErlDrvTermData am_packet_size;
ErlDrvTermData am_high_watermark;
ErlDrvTermData am_low_watermark;
ErlDrvTermData am_send_timeout;
ErlDrvTermData am_send_timeout_close;
ErlDrvTermData am_buffer;
ErlDrvTermData am_exit_on_close;
ErlDrvTermData am_debug;
ErlDrvTermData am_ptypkt;
ErlDrvTermData am_none;
ErlDrvTermData am_odd;
ErlDrvTermData am_even;
ErlDrvTermData am_mark;
ErlDrvTermData am_space;
ErlDrvTermData am_true;
ErlDrvTermData am_false;
ErlDrvTermData am_once;
ErlDrvTermData am_port;
ErlDrvTermData am_term;
ErlDrvTermData am_list;
ErlDrvTermData am_binary;
ErlDrvTermData am_size;
ErlDrvTermData am_line;
ErlDrvTermData am_basic_0710;
ErlDrvTermData am_advanced_0710;
ErlDrvTermData am_gsm_0710;


ErlDrvTermData am_ok;
ErlDrvTermData am_uart;
ErlDrvTermData am_error;
ErlDrvTermData am_uart_async;
ErlDrvTermData am_uart_reply;
ErlDrvTermData am_timeout;
ErlDrvTermData am_closed;
ErlDrvTermData am_uart_closed;
ErlDrvTermData am_uart_error;
ErlDrvTermData am_undefined;

static ErlDrvEntry uart_drv_entry;

/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, char* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
	ErlDrvBinary* bin = driver_alloc_binary(len+1);
	if (bin == NULL)
	    return -1;
	ptr = bin->orig_bytes;
	*rbuf = (char*)ptr;
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}


// setup global object area
// load atoms etc.

static int uart_drv_init(void)
{
    dlog_set_debug(DLOG_DEFAULT);
    DEBUGF("uart_driver_init");
    dthread_lib_init();
    dlog_set_debug(DLOG_DEFAULT);

    INIT_ATOM(dtr);
    INIT_ATOM(rts);
    INIT_ATOM(cts);
    INIT_ATOM(cd);
    INIT_ATOM(ri);
    INIT_ATOM(dsr);
    INIT_ATOM(sw);

    INIT_ATOM(device);
    INIT_ATOM(baud);
    INIT_ATOM(ibaud);
    INIT_ATOM(obaud);
    INIT_ATOM(csize);
    INIT_ATOM(bufsz);
    INIT_ATOM(buftm);
    INIT_ATOM(stopb);
    INIT_ATOM(parity);
    INIT_ATOM(iflow);
    INIT_ATOM(oflow);
    INIT_ATOM(xoffchar);
    INIT_ATOM(xonchar);
    INIT_ATOM(eolchar);
    INIT_ATOM(active);
    INIT_ATOM(delay_send);
    INIT_ATOM(deliver);
    INIT_ATOM(mode);
    INIT_ATOM(header);
    INIT_ATOM(packet);
    INIT_ATOM(packet_size);
    INIT_ATOM(high_watermark);
    INIT_ATOM(low_watermark);
    INIT_ATOM(send_timeout);
    INIT_ATOM(send_timeout_close);
    INIT_ATOM(buffer);
    INIT_ATOM(exit_on_close);
    INIT_ATOM(ptypkt);

    INIT_ATOM(none);
    INIT_ATOM(odd);
    INIT_ATOM(even);
    INIT_ATOM(mark);
    INIT_ATOM(space);

    INIT_ATOM(true);
    INIT_ATOM(false);
    INIT_ATOM(once);

    INIT_ATOM(port);
    INIT_ATOM(term);

    INIT_ATOM(list);
    INIT_ATOM(binary);

    INIT_ATOM(size);
    INIT_ATOM(line);
    INIT_ATOM(basic_0710);
    INIT_ATOM(advanced_0710);
    INIT_ATOM(gsm_0710);
    INIT_ATOM(debug);


    INIT_ATOM(ok);
    INIT_ATOM(uart);
    INIT_ATOM(error);
    INIT_ATOM(uart_async);
    INIT_ATOM(uart_reply);
    INIT_ATOM(timeout);
    INIT_ATOM(closed);
    INIT_ATOM(uart_closed);
    INIT_ATOM(uart_error);
    INIT_ATOM(undefined);

    return 0;
}

// clean up global settings
static void uart_drv_finish(void)
{
    // cleanup global stuff!
    dthread_lib_finish();
}

#ifdef HAVE_FTDI
extern void* uart_ftdi_main(void* arg);
#endif

#ifdef __WIN32__
extern void* uart_win32_main(void* arg);
#else
extern void* uart_unix_main(void* arg);
#endif


static ErlDrvData uart_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    drv_ctx_t* ctx = NULL;

    INFOF("memory allocated: %ld", dlib_allocated());
    INFOF("total memory allocated: %ld", dlib_total_allocated());

    ctx = DZALLOC(sizeof(drv_ctx_t));

    dthread_init(&ctx->self, port);

    if (strncmp(command, "uart_drv", 8) == 0)
	command += 8;
    if (*command == ' ')
	command++;
    DEBUGF("uart_drv: start (%s)", command);


    if (strcmp(command, "ftdi") == 0) {
#ifdef HAVE_FTDI
	ctx->other = dthread_start(port, uart_ftdi_main, &ctx->self, 4096);
	DEBUGF("uart_drv: ftdi thread = %p", ctx->other);
#endif
    }
    else {
#ifdef __WIN32__
	if ((*command == '\0') || (strcmp(command, "win32") == 0)) {
	    ctx->other = dthread_start(port, uart_win32_main, &ctx->self, 4096);
	    DEBUGF("uart_drv: win32 thread = %p", ctx->other);
	}
#else 
	if ((*command == '\0') || (strcmp(command, "unix") == 0)) {
	    ctx->other = dthread_start(port, uart_unix_main, &ctx->self, 4096);
	    DEBUGF("uart_drv: unix thread = %p", ctx->other);
	}
#endif
    }
    if (ctx->other == NULL) {
	dthread_finish(&ctx->self);
	DFREE(ctx);
	return ERL_DRV_ERROR_BADARG;
    }

    dthread_signal_use(&ctx->self, 1);
    dthread_signal_select(&ctx->self, 1);

    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return (ErlDrvData) ctx;
}

static void uart_drv_stop(ErlDrvData d)
{
    drv_ctx_t* ctx = (drv_ctx_t*) d;
    void* value;    

    DEBUGF("uart_drv_stop: called");
    dthread_stop(ctx->other, &ctx->self, &value);

    DEBUGF("uart_drv_stop: signal_use=0");
    dthread_signal_use(&ctx->self, 0);

    DEBUGF("uart_drv_stop: dthread_finish");
    dthread_finish(&ctx->self);
    DFREE(ctx);
    INFOF("memory allocated: %ld", dlib_allocated());
    INFOF("total memory allocated: %ld", dlib_total_allocated());
}

static char* format_command(int cmd)
{
    switch(cmd) {
    case UART_CMD_OPEN:      return "open";
    case UART_CMD_HANGUP:    return "hangup";
    case UART_CMD_CLOSE:     return "close";
    case UART_CMD_FLOW:      return "flow";
    case UART_CMD_BREAK:     return "break";
    case UART_CMD_SETOPTS:   return "setopts";
    case UART_CMD_GETOPTS:   return "getopts";
    case UART_CMD_SENDCHAR:  return "sendchar";
    case UART_CMD_SEND:      return "send";
    case UART_CMD_GET_MODEM: return "get_modem";
    case UART_CMD_SET_MODEM: return "set_modem";
    case UART_CMD_CLR_MODEM: return "clr_modem";
    case UART_CMD_UNRECV:    return "unrecv";
    case UART_CMD_RECV:      return "recv";
    default: return "????";
    }
}

static ErlDrvSSizeT uart_drv_ctl(ErlDrvData d, 
				 unsigned int cmd, char* buf, ErlDrvSizeT len,
				 char** rbuf, ErlDrvSizeT rsize)
{
    drv_ctx_t* ctx = (drv_ctx_t*) d;
    char ref_buf[sizeof(uint32_t)];

    DEBUGF("uart_drv: ctl: cmd=%u(%s), len=%d", cmd, format_command(cmd), len);

    ctx->self.caller = driver_caller(ctx->self.port);
    dthread_control(ctx->other, &ctx->self, cmd, buf, len);
    
    put_uint32((unsigned char*)ref_buf, (uint32_t) ctx->self.ref);
    return ctl_reply(UART_OK, ref_buf, sizeof(ref_buf), rbuf, rsize);
}


static void uart_drv_output(ErlDrvData d, char* buf, ErlDrvSizeT len)
{
    drv_ctx_t*   ctx = (drv_ctx_t*) d;

    DEBUGF("uart_drv: output");

    ctx->self.caller = driver_caller(ctx->self.port);
    dthread_output(ctx->other, &ctx->self, buf, len);
}

// NOTE: when SMP is enabled the messages go straight to the caller
// This code is here to allow non SMP emulator with the same code base.
static void uart_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
    drv_ctx_t* ctx = (drv_ctx_t*) d;

    DEBUGF("uart_drv: ready_input called");

    if (ctx->self.iq_signal[0] == e) { // got input !
	dmessage_t* mp;

	DEBUGF("uart_drv: ready_input handle=%d", 
	       DTHREAD_EVENT(ctx->self.iq_signal[0]));

	if ((mp = dthread_recv(&ctx->self, NULL)) == NULL) {
	    DEBUGF("uart_drv: ready_input signaled with no event! handle=%d",
		   DTHREAD_EVENT(ctx->self.iq_signal[0]));
	    return;
	}

	switch(mp->cmd) {
	case DTHREAD_OUTPUT_TERM:
	    DEBUGF("uart_drv: ready_input (OUTPUT_TERM)");
	    DOUTPUT_TERM(&(ctx->self), 
			 (ErlDrvTermData*) mp->buffer,
			 mp->used / sizeof(ErlDrvTermData));
	    break;
	case DTHREAD_SEND_TERM:
	    DEBUGF("uart_drv: ready_input (SEND_TERM)");
	    // dterm_dump(stderr, (ErlDrvTermData*) mp->buffer,
	    //   mp->used / sizeof(ErlDrvTermData));
	    DSEND_TERM(&(ctx->self), mp->to, /* orignal from ! */
		       (ErlDrvTermData*) mp->buffer,
		       mp->used / sizeof(ErlDrvTermData)); 
	    break;
	case DTHREAD_OUTPUT:
	    DEBUGF("uart_drv: ready_input (OUTPUT)");
	    driver_output(ctx->self.port, mp->buffer, mp->used);
	    break;
	default:
	    DEBUGF("uart_drv: read_input cmd=%d not matched",
		   mp->cmd);
	    break;
	}
	dmessage_free(mp);
    }
    else {
	DEBUGF("uart_drv: ready_input (NO MATCH)");
    }
}

static void uart_drv_ready_output(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
    DEBUGF("dthread_drv: ready_output");
}

// operation timed out
static void uart_drv_timeout(ErlDrvData d)
{
    (void) d;
    DEBUGF("uart_drv: timeout");
}


static void uart_drv_stop_select(ErlDrvEvent event, void* arg)
{
    (void) arg;
    DEBUGF("uart_drv: stop_select event=%d", DTHREAD_EVENT(event));
    dthread_event_close(event);
}

DRIVER_INIT(uart_drv)
{
    ErlDrvEntry* ptr = &uart_drv_entry;

    DEBUGF("driver_init");

    ptr->init  = uart_drv_init;
    ptr->start = uart_drv_start;
    ptr->stop  = uart_drv_stop;
    ptr->output = uart_drv_output;
    ptr->ready_input  = uart_drv_ready_input;
    ptr->ready_output = uart_drv_ready_output;
    ptr->finish = uart_drv_finish;
    ptr->driver_name = "uart_drv";
    ptr->control = uart_drv_ctl;
    ptr->timeout = uart_drv_timeout;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = uart_drv_stop_select;
    return ptr;
}
