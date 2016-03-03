/****** BEGIN COPYRIGHT *******************************************************
 *
 * Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 ****** END COPYRIGHT ********************************************************/
//
// Netlink driver
//

#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <asm/types.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>

#include "erl_driver.h"

#define ATOM(NAME) am_ ## NAME
#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

#if (ERL_DRV_EXTENDED_MAJOR_VERSION > 2) || ((ERL_DRV_EXTENDED_MAJOR_VERSION == 2) && (ERL_DRV_EXTENDED_MINOR_VERSION >= 1))
#define SEND_TERM(ctx, to, message, len) erl_drv_send_term((ctx)->dport,(to),(message),(len))
#else
#define SEND_TERM(ctx, to, message, len) driver_send_term((ctx)->port,(to),(message),(len))
#endif

#ifndef SOL_NETLINK
#define SOL_NETLINK 270
#endif

#define PORT_CONTROL_BINARY

#define INT_EVENT(ptr)   ((int)((long)(ptr)))

typedef struct _nl_ctx_t {
    ErlDrvPort       port;
    ErlDrvTermData   dport;
    ErlDrvTermData   owner;
    ErlDrvEvent      fd;       // netlink socket
    int              protocol;  // netlink protocol
    int              active;    
    int              is_selecting;
    int              is_sending;
    void*            nlbuf;
    size_t           nlbuf_len;
} nl_ctx_t;

#define CMD_ADD_MEMBERSHIP    1
#define CMD_DROP_MEMBERSHIP   2
#define CMD_ACTIVE            3
#define CMD_DEBUG             4
#define CMD_SET_RCVBUF        5
#define CMD_SET_SNDBUF        6
#define CMD_GET_RCVBUF        7
#define CMD_GET_SNDBUF        8
#define CMD_GET_SIZEOF        9


#define CTL_OK     0
#define CTL_INT    1
#define CTL_PAIR   2
#define CTL_BIN    3
#define CTL_LIST   4
#define CTL_ERR    255
#define CTL_STRERR 254

#define MIN_NL_BUFSIZE (32*1024)
#define MAX_NL_BUFSIZE (512*1024)

#define MAX_VSIZE 16

ErlDrvTermData am_ok;
ErlDrvTermData am_error;
ErlDrvTermData am_undefined;
ErlDrvTermData am_true;
ErlDrvTermData am_false;
ErlDrvTermData am_nl_data;

#define push_atom(atm) do {			\
	message[i++] = ERL_DRV_ATOM;		\
	message[i++] = (atm);			\
    } while(0)

#define push_port(prt) do {			\
	message[i++] = ERL_DRV_PORT;		\
	message[i++] = (prt);			\
    } while(0)

#define push_pid(pid) do {			\
	message[i++] = ERL_DRV_PID;		\
	message[i++] = (pid);			\
    } while(0)

#define push_bin(buf,len) do {			\
	message[i++] = ERL_DRV_BUF2BINARY;	\
	message[i++] = (ErlDrvTermData)(buf);	\
	message[i++] = (ErlDrvTermData)(len);	\
    } while(0)

#define push_nil() do {			\
	message[i++] = ERL_DRV_NIL;	\
    } while(0)

#define push_string(str) do {			\
	message[i++] = ERL_DRV_STRING;		\
	message[i++] = (ErlDrvTermData) (str);	\
	message[i++] = strlen(str);		\
    } while(0)

#define push_int(val) do {			\
	message[i++] = ERL_DRV_INT;		\
	message[i++] = (val);			\
    } while(0)

#define push_tuple(n) do {			\
	message[i++] = ERL_DRV_TUPLE;		\
	message[i++] = (n);			\
    } while(0)

#define push_list(n) do {			\
	message[i++] = ERL_DRV_LIST;		\
	message[i++] = (n);			\
    } while(0)


ErlDrvEntry nl_drv_entry;

static inline uint32_t get_uint32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return value;
}

static inline int32_t get_int32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return (int32_t) value;
}

static inline uint16_t get_uint16(uint8_t* ptr)
{
    uint16_t value = (ptr[0]<<8) | (ptr[1]<<0);
    return value;
}

static inline uint8_t get_uint8(uint8_t* ptr)
{
    return ptr[0];
}

static inline int8_t get_int8(uint8_t* ptr)
{
    return (int8_t) ptr[0];
}

static inline void put_uint16(uint8_t* ptr, uint16_t v)
{
    ptr[0] = v>>8;
    ptr[1] = v;
}

static inline void put_uint32(uint8_t* ptr, uint32_t v)
{
    ptr[0] = v>>24;
    ptr[1] = v>>16;
    ptr[2] = v>>8;
    ptr[3] = v;
}

static int        nl_drv_init(void);
static void       nl_drv_finish(void);
static void       nl_drv_stop(ErlDrvData);
static void       nl_drv_output(ErlDrvData,char*,ErlDrvSizeT);
#if 0
static void       nl_drv_outputv(ErlDrvData, ErlIOVec*);
#endif
static void       nl_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void       nl_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData nl_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT nl_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**,ErlDrvSizeT);
static void       nl_drv_timeout(ErlDrvData);
static void       nl_drv_stop_select(ErlDrvEvent event, void* arg);

#define DLOG_DEBUG     7
#define DLOG_INFO      6
#define DLOG_NOTICE    5
#define DLOG_WARNING   4
#define DLOG_ERROR     3
#define DLOG_CRITICAL  2
#define DLOG_ALERT     1
#define DLOG_EMERGENCY 0
#define DLOG_NONE     -1

#ifndef DLOG_DEFAULT
#define DLOG_DEFAULT DLOG_NONE
#endif

#define DLOG(level,file,line,args...) do {				\
	if (((level) == DLOG_EMERGENCY) ||				\
	    ((debug_level >= 0) && ((level) <= debug_level))) {		\
	    emit_log((level),(file),(line),args);			\
	}								\
    } while(0)

#define DEBUGF(args...) DLOG(DLOG_DEBUG,__FILE__,__LINE__,args)
#define INFOF(args...)  DLOG(DLOG_INFO,__FILE__,__LINE__,args)
#define NOTICEF(args...)  DLOG(DLOG_NOTICE,__FILE__,__LINE__,args)
#define WARNINGF(args...)  DLOG(DLOG_WARNING,__FILE__,__LINE__,args)
#define ERRORF(args...)  DLOG(DLOG_ERROR,__FILE__,__LINE__,args)
#define CRITICALF(args...)  DLOG(DLOG_CRITICAL,__FILE__,__LINE__,args)
#define ALERTF(args...)  DLOG(DLOG_ALERT,__FILE__,__LINE__,args)
#define EMERGENCYF(args...)  DLOG(DLOG_EMERGENCY,__FILE__,__LINE__,args)

static int debug_level = DLOG_DEFAULT;

static void emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((debug_level >= 0) && (level <= debug_level))) {
	int save_errno = errno;
	va_start(ap, line);
	fmt = va_arg(ap, char*);
	fprintf(stderr, "%s:%d: ", file, line); 
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\r\n");
	va_end(ap);
	errno = save_errno;
    }
}

/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, void* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
#ifdef PORT_CONTROL_BINARY
	ErlDrvBinary* bin = driver_alloc_binary(len+1);
	if (bin == NULL) 
	    return -1;
	ptr = bin->orig_bytes;	
	*rbuf = (char*) bin;
#else
	if ((ptr = driver_alloc(len+1)) == NULL)
	    return -1;
	*rbuf = ptr;
#endif
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}

static void* nl_realloc_buffer(nl_ctx_t* ctx, size_t len)
{
    if (ctx->nlbuf_len < len) {
	ctx->nlbuf = driver_realloc(ctx->nlbuf, NLMSG_SPACE(len));
	ctx->nlbuf_len = len;
    }
    return ctx->nlbuf;
}

static int nl_drv_init(void)
{
    INIT_ATOM(ok);
    INIT_ATOM(error);
    INIT_ATOM(undefined);
    INIT_ATOM(true);
    INIT_ATOM(false);
    INIT_ATOM(nl_data);
    return 0;
}

static void nl_drv_finish(void)
{
}

static void nl_drv_stop(ErlDrvData d)
{
    nl_ctx_t* ctx = (nl_ctx_t*) d;

    if (ctx) {
	if (ctx->is_selecting)
	    driver_select(ctx->port, ctx->fd, ERL_DRV_READ, 0);
	if (ctx->is_sending)
	    driver_select(ctx->port, ctx->fd, ERL_DRV_WRITE, 0);
	driver_select(ctx->port, ctx->fd, ERL_DRV_USE, 0);
	driver_free(ctx);
    }
}

static void nl_drv_output(ErlDrvData d, char* buf,ErlDrvSizeT len)
{
    nl_ctx_t* ctx = (nl_ctx_t*) d;
    int n;
    struct nlmsghdr* nlh = (struct nlmsghdr*) buf;

    if (!NLMSG_OK(nlh, len)) {
	DEBUGF("netlink_drv: data not OK");
    }
    else {
	DEBUGF("netlink_drv: output len=%d, type=%d, seq=%d, pid=%d",
	       nlh->nlmsg_len, nlh->nlmsg_type, nlh->nlmsg_seq, nlh->nlmsg_pid);
	if ((n = driver_sizeq(ctx->port)) > 0) {
	    driver_enq(ctx->port, buf, len);
	    DEBUGF("netlink_drv_output: put on queue pending=%d", n+len);
	}
	else  {  // try send directly
	    struct sockaddr_nl dest_addr;
	    struct iovec iov;
	    struct msghdr msg;

	    memset(&msg, 0, sizeof(msg));
	    memset(&dest_addr, 0, sizeof(dest_addr));

	    dest_addr.nl_family = AF_NETLINK;
	    dest_addr.nl_pid = 0;    // to kernel
	    dest_addr.nl_groups = 0; // unicast

	    iov.iov_base = (void*) buf;
	    iov.iov_len  = len;

	    msg.msg_name = (void*) &dest_addr;
	    msg.msg_namelen = sizeof(dest_addr);
	    msg.msg_iov = &iov;
	    msg.msg_iovlen = 1;
    
	    n = sendmsg(INT_EVENT(ctx->fd), &msg, 0);
	    if (n < 0) {
		ERRORF("write error=%s", strerror(errno));
		if ((errno == EAGAIN) || (errno = ENOBUFS)) {
		    DEBUGF("netlink_drv: put on queue", n);
		    driver_enq(ctx->port, buf, len);
		    driver_select(ctx->port, ctx->fd, ERL_DRV_WRITE, 1);
		    ctx->is_sending = 1;
		}
	    }
	}
    }
}


// netlink socket triggered process data
static void nl_drv_ready_input(ErlDrvData d, ErlDrvEvent event)
{
    nl_ctx_t* ctx = (nl_ctx_t*) d;
    struct sockaddr_nl src_addr;
    struct iovec     iov;
    struct msghdr    msg;
    struct nlmsghdr* nlh;
    int n;
    int part = 0;
    int recv_count = 10;

    DEBUGF("nl_drv_ready_input");

again:
    if (!--recv_count)
	return;
    memset(&iov, 0, sizeof(iov));
    memset(&msg, 0, sizeof(msg));
    memset(&src_addr, 0, sizeof(src_addr));
    
    nlh = nl_realloc_buffer(ctx, MIN_NL_BUFSIZE);

    iov.iov_base = (void*) nlh;
    iov.iov_len  = ctx->nlbuf_len;
    msg.msg_name = (void*) &src_addr;
    msg.msg_namelen = sizeof(src_addr);
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;

    if ((n = recvmsg(INT_EVENT(ctx->fd), &msg, 0)) < 0) {
	if (errno == EAGAIN)
	    return;
	// send error to erlang ?
	WARNINGF("nl_drv_read_input: read error=%s", strerror(errno));
    }
    else if (n == 0) {
	WARNINGF("nl_drv_read_input: read eof?");
    }
    else {
	ErlDrvTermData message[16];

	while(NLMSG_OK(nlh, n)) {
	    int i = 0;
	    // {nl_data, <port>, <data>}
	    push_atom(ATOM(nl_data));
	    push_port(ctx->dport);
	    push_bin((char*)nlh, nlh->nlmsg_len);
	    push_tuple(3);
	    DEBUGF("nl_drv_read_input: part = %d", part);
	    part++;
	    if (ctx->active) {
		SEND_TERM(ctx, ctx->owner, message, i);
		if (ctx->active > 0) {
		    ctx->active--;
		    if (ctx->active == 0) {
			ctx->is_selecting = 0;
			driver_select(ctx->port, ctx->fd, ERL_DRV_READ, 0);
		    }
		}
	    }
	    nlh = NLMSG_NEXT(nlh, n);
	}
	goto again;
    }
}

static void nl_drv_ready_output(ErlDrvData d, ErlDrvEvent event)
{
    nl_ctx_t* ctx = (nl_ctx_t*) d;
    struct sockaddr_nl dest_addr;
    int vsize;
    SysIOVec* iovp;
    struct msghdr msg;
    int n;

    DEBUGF("nl_drv_ready_output called");

    if (ctx->fd != event) {
	DEBUGF("nl_drv_ready_output bad event");
	return;
    }

    if ((iovp = driver_peekq(ctx->port, &vsize)) == NULL) {
	driver_select(ctx->port, ctx->fd, ERL_DRV_WRITE, 0);
	ctx->is_sending = 0;
	return;
    }
    vsize = vsize > MAX_VSIZE ? MAX_VSIZE : vsize;

    memset(&msg, 0, sizeof(msg));
    memset(&dest_addr, 0, sizeof(dest_addr));

    dest_addr.nl_family = AF_NETLINK;
    dest_addr.nl_pid = 0;    // to kernel
    dest_addr.nl_groups = 0; // unicast    

    msg.msg_name = (void*) &dest_addr;
    msg.msg_namelen = sizeof(dest_addr);
    msg.msg_iov = (struct iovec*) iovp;
    msg.msg_iovlen = vsize;

    DEBUGF("nl_drv_ready_output: try send vsize=%d", vsize);    
    n = sendmsg(INT_EVENT(ctx->fd), &msg, 0);
    if (n < 0) {
	if ((errno == EAGAIN) || (errno == ENOBUFS))
	    return;
	ERRORF("write error=%s", strerror(errno));
	return;
    }
    DEBUGF("nl_drv_ready_output: sent %d bytes", n);
    if (driver_deq(ctx->port, n) == 0) {
	driver_select(ctx->port, ctx->fd, ERL_DRV_WRITE, 0);
	ctx->is_sending = 0;
    }
}

static ErlDrvSSizeT nl_drv_ctl(ErlDrvData d,unsigned int cmd,char* buf0,
			       ErlDrvSizeT len,char** rbuf,ErlDrvSizeT rsize)
{
    uint8_t* buf = (uint8_t*) buf0;
    nl_ctx_t* ctx = (nl_ctx_t*) d;

    DEBUGF("nl_drv_ctl cmd=%d", cmd);

    switch(cmd) {
    case CMD_ADD_MEMBERSHIP: {
	int opt;
	if (len != 4)
	    goto badarg;
	opt = get_int32(buf);
	if (setsockopt(INT_EVENT(ctx->fd), SOL_NETLINK,
		       NETLINK_ADD_MEMBERSHIP, 
		       (void*) &opt, sizeof(opt)) < 0)
	    goto error;
	goto ok;
    }
    case CMD_DROP_MEMBERSHIP: {
	int opt;
	if (len != 4)
	    goto badarg;
	opt = get_int32(buf);
	
	if (setsockopt(INT_EVENT(ctx->fd), SOL_NETLINK,
		       NETLINK_DROP_MEMBERSHIP, 
		       (void*) &opt, sizeof(opt)) < 0)
	    goto error;
	goto ok;
    }

    case CMD_SET_RCVBUF: {
	int opt;
	if (len != 4)
	    goto badarg;
	opt = get_int32(buf);
	if (setsockopt(INT_EVENT(ctx->fd), SOL_SOCKET,
		       SO_RCVBUF,
		       (void*) &opt, sizeof(opt)) < 0)
	    goto error;
	if (opt > 0) {
	    // make sure i/o buffer match
	    nl_realloc_buffer(ctx, (size_t)opt);
	}
	goto ok;
    }

    case CMD_SET_SNDBUF: {
	int opt;
	if (len != 4)
	    goto badarg;
	opt = get_int32(buf);
	if (setsockopt(INT_EVENT(ctx->fd), SOL_SOCKET,
		       SO_SNDBUF,
		       (void*) &opt, sizeof(opt)) < 0)
	    goto error;
	if (opt > 0) {
	    // make sure i/o buffer match
	    nl_realloc_buffer(ctx, (size_t)opt);
	}
	goto ok;
    }

    case CMD_GET_RCVBUF: {
	int opt;
	socklen_t optlen;
	if (len != 0)
	    goto badarg;
	optlen = sizeof(opt);
	opt = get_int32(buf);
	if (getsockopt(INT_EVENT(ctx->fd), SOL_SOCKET,
		       SO_RCVBUF,
		       (void*) &opt, &optlen) < 0)
	    goto error;
	return ctl_reply(CTL_INT, &opt, sizeof(opt), rbuf, rsize);
    }

    case CMD_GET_SNDBUF: {
	int opt;
	socklen_t optlen;
	if (len != 0)
	    goto badarg;
	optlen = sizeof(opt);
	opt = get_int32(buf);
	if (getsockopt(INT_EVENT(ctx->fd), SOL_SOCKET,
		       SO_SNDBUF,
		       (void*) &opt, &optlen) < 0)
	    goto error;
	return ctl_reply(CTL_INT, &opt, sizeof(opt), rbuf, rsize);
    }

    case CMD_GET_SIZEOF: {
	uint8_t sizes[6];
	
	sizes[0] = (uint8_t) sizeof(char);
	sizes[1] = (uint8_t) sizeof(short);
	sizes[2] = (uint8_t) sizeof(int);
	sizes[3] = (uint8_t) sizeof(long);
	sizes[4] = (uint8_t) sizeof(long long);
	sizes[5] = (uint8_t) sizeof(void*);
	return ctl_reply(CTL_LIST, sizes, 6, rbuf, rsize);
    }

    case CMD_ACTIVE: {
	int active;

	if (len != 4)
	    goto badarg;
	active = get_int32(buf);
	if (active) {
	    if (!ctx->is_selecting)
		driver_select(ctx->port, ctx->fd, ERL_DRV_READ, 1);
	    ctx->is_selecting = 1;
	    ctx->active = active;
	}
	else {
	    if (ctx->is_selecting)
		driver_select(ctx->port, ctx->fd, ERL_DRV_READ, 0);
	    ctx->is_selecting = 0;
	    ctx->active = 0;
	}
	goto ok;
	break;
    }
    case CMD_DEBUG: {
	if (len != 4)
	    goto badarg;
	debug_level = get_int32(buf);
	goto ok;
    }

    default:
	return -1;
    }

ok:
    return ctl_reply(CTL_OK, NULL, 0, rbuf, rsize);
badarg:
    errno = EINVAL;
error: {
    char* err_str = erl_errno_id(errno);
    return ctl_reply(CTL_ERR, err_str, strlen(err_str), rbuf, rsize);
}
}


static void       nl_drv_timeout(ErlDrvData d)
{
    (void) d;
    fprintf(stderr, "nl_drv_timeout called!!!\r\n");
}

static void nl_drv_stop_select(ErlDrvEvent event, void* arg)
{    
    (void) arg;
    DEBUGF("eth_drv: stop_select event=%d", INT_EVENT(event));
    close(INT_EVENT(event));
}


static ErlDrvData nl_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    nl_ctx_t* ctx;
    int flags;
    int fd;
    int protocol;
    char* ptr;
    char* arg;
    struct sockaddr_nl addr;

    ptr = command;
    while(*ptr && (*ptr != ' '))  ptr++;   // skip command
    while(*ptr && (*ptr == ' '))  ptr++;   // and blanks
    arg = ptr;
    while(*ptr && (*ptr >= '0') && (*ptr <= '9')) ptr++;
    if ((arg == ptr) || (*ptr != '\0')) {
	errno = EINVAL;
	return ERL_DRV_ERROR_ERRNO;
    }
    protocol = atoi(arg);
    
    if ((fd = socket(PF_NETLINK, SOCK_RAW, protocol)) < 0)
	return ERL_DRV_ERROR_ERRNO;

    memset(&addr, 0, sizeof(addr));
    addr.nl_family = AF_NETLINK;
    addr.nl_groups  = 0;         // start with no groups
    addr.nl_pid     = getpid();  // bind using this pid?

    if (bind(fd, (struct sockaddr* ) &addr, sizeof(addr))  < 0)
	return ERL_DRV_ERROR_ERRNO;

    flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, flags | O_NONBLOCK);

    if (!(ctx = driver_alloc(sizeof(nl_ctx_t))))
	return ERL_DRV_ERROR_ERRNO;
    memset(ctx, 0, sizeof(nl_ctx_t));
    ctx->port = port;
    ctx->dport = driver_mk_port(port);
    ctx->owner = driver_caller(port);
    ctx->protocol = protocol;
    ctx->fd       = (ErlDrvEvent)((long)fd);

    nl_realloc_buffer(ctx, MIN_NL_BUFSIZE);  // create i/o buffer

#ifdef PORT_CONTROL_BINARY
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
#endif
    
    return (ErlDrvData) ctx;
}

DRIVER_INIT(nl_drv)
{
    ErlDrvEntry* ptr = &nl_drv_entry;

    ptr->driver_name = "netlink_drv";
    ptr->init  = nl_drv_init;
    ptr->start = nl_drv_start;
    ptr->stop  = nl_drv_stop;
    ptr->output = nl_drv_output;
    ptr->ready_input  = nl_drv_ready_input;
    ptr->ready_output = nl_drv_ready_output;
    ptr->finish = nl_drv_finish;
    ptr->control = nl_drv_ctl;
    ptr->timeout = nl_drv_timeout;
#if 0
    ptr->outputv = nl_drv_outputv;
#endif
    ptr->ready_async = 0;
    ptr->flush = 0;
    ptr->call = 0;
    ptr->event = 0;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = nl_drv_stop_select;

    return (ErlDrvEntry*) ptr;
}
