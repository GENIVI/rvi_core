//
// Implements message formating and sending
//

#include <ctype.h>

#include "uart_drv.h"

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

/* send message:
**     {uart_async, Port, Ref, ok} 
*/
int uart_async_ok(uart_ctx_t* ctx,ErlDrvTermData port,ErlDrvTermData recipient)
{
    dterm_t t;
    dterm_mark_t m;
    int r;

    dterm_init(&t);
    dterm_tuple_begin(&t, &m); {
	dterm_atom(&t, am_uart_async);
	dterm_port(&t, port);
	dterm_uint(&t, ctx->ref);
	dterm_atom(&t, am_ok);
    }
    dterm_tuple_end(&t, &m);

    r = dthread_port_send_dterm(ctx->other, ctx->self, recipient, &t);
    dterm_finish(&t);
    return r;
}
//
// send message:
//      {uart_async, port, Ref, {error,Reason}}
//
int uart_async_error_am(uart_ctx_t* ctx, ErlDrvTermData port,
			ErlDrvTermData recipient, ErlDrvTermData Reason)
{
    dterm_t t;
    dterm_mark_t m;
    int r;

    dterm_init(&t);
    dterm_tuple_begin(&t, &m); { 
	dterm_mark_t m1;
	dterm_atom(&t, am_uart_async);
	dterm_port(&t, port);
	dterm_uint(&t,  ctx->ref);
	dterm_tuple_begin(&t, &m1); { 
	    dterm_atom(&t, am_error);
	    dterm_atom(&t, Reason);
	}
	dterm_tuple_end(&t, &m1);
    }
    dterm_tuple_end(&t, &m);
	    
    r = dthread_port_send_dterm(ctx->other, ctx->self, recipient, &t);
    dterm_finish(&t);
    return r;
}

int uart_async_error(uart_ctx_t* ctx, ErlDrvTermData port,
		     ErlDrvTermData recipient, int err)

{
    return uart_async_error_am(ctx, port, recipient, error_atom(err));
}

// 
//  {Port,{data,Binary}} |
//  {Port,{data,[H1,..Hn|Binary]}} |
//  {Port,{data,[B1,..Bn]}}
//
int uart_port_data(uart_ctx_t* ctx, const char* buf, int len)
{
    unsigned int hsz = ctx->option.hsz;

    DEBUGF("uart_port_data(%ld): len = %d", (long)ctx->port, len);

    if ((ctx->option.mode == UART_MODE_LIST) || ((int)hsz > len))
	return dthread_port_output2(ctx->other, ctx->self,
				    (char*)buf, len, NULL, 0);
    else if (hsz > 0)
	return dthread_port_output2(ctx->other, ctx->self,
				    (char*)buf, hsz, (char*)buf+hsz, len-hsz);
    else
	return driver_output(ctx->port, (char*)buf, len);
}

// construct the data list [H1...,Hsz | Data]
static void put_data_list(uart_ctx_t* ctx, dterm_t* t, const char* buf, int len)
{
    unsigned int hsz = ctx->option.hsz;
    
    if ((ctx->option.mode == UART_MODE_LIST) || ((int)hsz > len))
	dterm_string(t, buf, len); 
    else {
	int sz = len - hsz;
	dterm_buf_binary(t, buf+hsz, sz);
	if (hsz > 0)
	    dterm_string_cons(t, buf, hsz);
    }
}

// construct the data list [H1...,Hsz | Data] from binary
static void put_data_bin(uart_ctx_t* ctx, dterm_t* t, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = ctx->option.hsz;
    if ((ctx->option.mode == UART_MODE_LIST) || ((int)hsz > len))
	dterm_string(t, bin->orig_bytes+offs, len);
    else {
	int sz = len - hsz;
	dterm_binary(t, bin, offs+hsz, sz);
	if (hsz > 0)
	    dterm_string_cons(t, bin->orig_bytes+offs, hsz);
    }
}

// 
// Deliver port data from binary (for an active mode socket)
//
int uart_port_binary_data(uart_ctx_t* ctx, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = ctx->option.hsz;

    DEBUGF("uart_port_binary_data(%ld): offs=%d, len = %d",
	   (long)ctx->port, offs, len);

    if ((ctx->option.mode == UART_MODE_LIST) || ((int)hsz > len)) 
	return dthread_port_output2(ctx->other, ctx->self,
				    bin->orig_bytes+offs, len, NULL, 0);
    else 
	return dthread_port_output_binary(ctx->other, ctx->self,
					  bin->orig_bytes+offs, hsz,
					  bin, offs+hsz, len-hsz);
}

// 
// {uart, S, [H1,...Hsz | Data]}
//
int uart_message(uart_ctx_t* ctx, const char* buf, int len)
{
    dterm_t t;
    dterm_mark_t m;
    int r;

    DEBUGF("uart_message(%ld): len = %d", (long)ctx->port, len);

    dterm_init(&t);
    dterm_tuple_begin(&t, &m); {
	dterm_atom(&t, am_uart);
	dterm_port(&t, ctx->dport);
	put_data_list(ctx, &t, buf, len);
    }
    dterm_tuple_end(&t, &m);

    r=dthread_port_output_dterm(ctx->other, ctx->self, &t);
    dterm_finish(&t);
    return r;
}

//
//
// {uart, S, [H1,...Hsz | Data]}
//
int uart_binary_message(uart_ctx_t* ctx, ErlDrvBinary* bin, int offs, int len)
{
    dterm_t t;
    dterm_mark_t m;
    int r;

    DEBUGF("uart_binary_message(%ld): len = %d", (long)ctx->port, len); 

    dterm_init(&t);
    dterm_tuple_begin(&t, &m); {
	dterm_atom(&t, am_uart);
	dterm_port(&t, ctx->dport);
	put_data_bin(ctx, &t, bin, offs, len);
    }
    dterm_tuple_end(&t, &m);
    r=dthread_port_output_dterm(ctx->other, ctx->self, &t);
    dterm_finish(&t);
    return r;
}

//
// {uart_closed, S}
//
int uart_closed_message(uart_ctx_t* ctx)
{
    DEBUGF("uart_closed_message(%ld):", (long)ctx->port); 

    if (!(ctx->flags & UART_F_CLOSE_SENT)) {
	dterm_t t;
	dterm_mark_t m;
	int r;

	ctx->flags |= UART_F_CLOSE_SENT;
	dterm_init(&t);
	dterm_tuple_begin(&t, &m); {
	    dterm_atom(&t, am_uart_closed);
	    dterm_port(&t, ctx->dport);
	}
	dterm_tuple_end(&t, &m);
	r=dthread_port_output_dterm(ctx->other, ctx->self, &t);
	dterm_finish(&t);
	return r;
    } 
    return 0;
}

//
// {uart_error, S, Error}
//
int uart_error_message(uart_ctx_t* ctx, int err)
{
    ErlDrvTermData e = error_atom(err);
    dterm_t t;
    dterm_mark_t m;
    int r;

    DEBUGF("uart_error_message(%ld): %d", (long)ctx->port, err); 
    
    dterm_init(&t);
    dterm_tuple_begin(&t, &m); {
	dterm_atom(&t, am_uart_error);
	dterm_port(&t, ctx->dport);
	dterm_atom(&t, e);
    }
    dterm_tuple_end(&t, &m);
    r=dthread_port_output_dterm(ctx->other, ctx->self, &t);
    dterm_finish(&t);
    return r;
}

// 
//  {uart_async, U, Ref, {ok,[H1,...Hsz | Data]}}
//
int uart_async_data(uart_ctx_t* ctx, const char* buf, int len)
{
    dterm_t t;
    dterm_mark_t m;
    int r;

    DEBUGF("uart_async_data(%ld): len = %d", (long)ctx->port, len);

    dterm_init(&t);
    dterm_tuple_begin(&t, &m); {
	dterm_atom(&t, am_uart_async);
	dterm_port(&t, ctx->dport);
	dterm_uint(&t, ctx->ref);
	put_data_list(ctx, &t, buf, len);
    }
    dterm_tuple_end(&t, &m);
    r=dthread_port_output_dterm(ctx->other, ctx->self, &t);
    dterm_finish(&t);
    return r;
}


// 
//  {uart_async, U, Ref, {ok,[H1,...Hsz | Data]}}
//
int uart_async_binary_data(uart_ctx_t* ctx, 
			   ErlDrvBinary* bin, int offs, int len)
{
    dterm_t t;
    dterm_mark_t m;
    int r;

    DEBUGF("uart_async_binary_data(%ld): len = %d", (long)ctx->port, len);

    dterm_init(&t);
    dterm_tuple_begin(&t, &m); {
	dterm_atom(&t, am_uart_async);
	dterm_port(&t, ctx->dport);
	dterm_uint(&t, ctx->ref);
	put_data_bin(ctx, &t, bin, offs, len);
    }
    dterm_tuple_end(&t, &m);
    r=dthread_port_output_dterm(ctx->other, ctx->self, &t);
    dterm_finish(&t);
    return r;
}

int uart_reply_data(uart_ctx_t* ctx, char* buf, int len)
{
    int code;
    const char* body = buf;
    int bodylen = len;

    if ((ctx->option.htype & UART_PB_TYPE_MASK) == UART_PB_N) {
	unsigned n = (ctx->option.htype & UART_PB_BYTES_MASK) >> 8;
	body    += n;
	bodylen -= n;
    }
    if (ctx->option.deliver == UART_DELIVER_PORT) {
        code = uart_port_data(ctx, body, bodylen);
    }
    else {
        if (ctx->option.active == UART_PASSIVE)
            return uart_async_data(ctx, body, bodylen);
        else
            code = uart_message(ctx, body, bodylen);
    }
    if (code < 0)
	return code;
    if (ctx->option.active == UART_ONCE)
	ctx->option.active = UART_PASSIVE;
    return code;
}

int uart_reply_binary_data(uart_ctx_t* ctx, ErlDrvBinary* bin, int offs, int len)
{
    int code;
    const char* buf = bin->orig_bytes + offs;
    const char* body = buf;
    int bodylen = len;

    if ((ctx->option.htype & UART_PB_TYPE_MASK) == UART_PB_N) {
	unsigned n = (ctx->option.htype & UART_PB_BYTES_MASK) >> 8;
	body    += n;
	bodylen -= n;
    }

    offs = body - bin->orig_bytes; /* body offset now */

    if (ctx->option.deliver == UART_DELIVER_PORT)
        code = uart_port_binary_data(ctx, bin, offs, bodylen);
    else {
        if (ctx->option.active == UART_PASSIVE)
            return uart_async_binary_data(ctx, bin, offs, bodylen);
        else
            code = uart_binary_message(ctx, bin, offs, bodylen);
    }
    if (code < 0)
	return code;
    if (ctx->option.active == UART_ONCE)
	ctx->option.active = UART_PASSIVE;
    return code;
}
