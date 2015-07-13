/**** BEGIN COPYRIGHT ********************************************************
 *
 * Copyright (C) 2015 Rogvall Invest AB, <tony@rogvall.se>
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
 **** END COPYRIGHT **********************************************************/

#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <asm/types.h>
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>

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

#define PORT_CONTROL_BINARY

#define INT_EVENT(ptr)   ((int)((long)(ptr)))

typedef struct _hci_drv_ctx_t {
    ErlDrvPort       port;
    ErlDrvTermData   dport;
    ErlDrvTermData   owner;
    ErlDrvEvent      fd;       // socket
    int              protocol;  // bt protocol
    int              active;    
    int              is_selecting;
    int              is_sending;
    int              dev_id;        // bound device id 
    void*            hcibuf;
    size_t           hcibuf_len;
} hci_drv_ctx_t;


#define CMD_ACTIVE         1
#define CMD_DEBUG          2
#define CMD_BIND           3
#define CMD_GETFILTER      4
#define CMD_SETFILTER      5

#define CMD_HCIDEVUP	   201
#define CMD_HCIDEVDOWN	   202
#define CMD_HCIDEVRESET	   203
#define CMD_HCIDEVRESTAT   204
#define CMD_HCIGETDEVLIST  210
#define CMD_HCIGETDEVINFO  211
#define CMD_HCIGETCONNLIST 212
#define CMD_HCIGETCONNINFO 213
#define CMD_HCIGETAUTHINFO 215
#define CMD_HCISETRAW	   220
#define CMD_HCISETSCAN	   221
#define CMD_HCISETAUTH	   222
#define CMD_HCISETENCRYPT  223
#define CMD_HCISETPTYPE	   224
#define CMD_HCISETLINKPOL  225
#define CMD_HCISETLINKMODE 226
#define CMD_HCISETACLMTU   227
#define CMD_HCISETSCOMTU   228
#define CMD_HCIBLOCKADDR   230
#define CMD_HCIUNBLOCKADDR 231
#define CMD_HCIINQUIRY	   240

#define CTL_OK     0
#define CTL_INT    1
#define CTL_PAIR   2
#define CTL_BIN    3
#define CTL_LIST   4
#define CTL_ERR    255
#define CTL_STRERR 254

#define MIN_HCI_BUFSIZE (32*1024)
#define MAX_HCI_BUFSIZE (512*1024)

#define MAX_CONN 10

#define MAX_VSIZE 16

ErlDrvTermData am_ok;
ErlDrvTermData am_error;
ErlDrvTermData am_undefined;
ErlDrvTermData am_true;
ErlDrvTermData am_false;

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


ErlDrvEntry hci_drv_entry;

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

static inline void put_uint8(uint8_t* ptr, uint8_t v)
{
  ptr[0] = v;
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

static uint8_t* put_hci_dev_stats(uint8_t* ptr, struct hci_dev_stats* sp)
{
  put_uint32(ptr, sp->err_rx); ptr += sizeof(sp->err_rx);
  put_uint32(ptr, sp->err_tx); ptr += sizeof(sp->err_tx);
  put_uint32(ptr, sp->cmd_tx); ptr += sizeof(sp->cmd_tx);
  put_uint32(ptr, sp->evt_rx); ptr += sizeof(sp->evt_rx);
  put_uint32(ptr, sp->acl_tx); ptr += sizeof(sp->acl_tx);
  put_uint32(ptr, sp->acl_rx); ptr += sizeof(sp->acl_rx);
  put_uint32(ptr, sp->sco_tx); ptr += sizeof(sp->sco_tx);
  put_uint32(ptr, sp->sco_rx); ptr += sizeof(sp->sco_rx);
  put_uint32(ptr, sp->byte_tx); ptr += sizeof(sp->byte_tx);
  put_uint32(ptr, sp->byte_rx); ptr += sizeof(sp->byte_rx);
  return ptr;
};

static uint8_t* put_hci_dev_info(uint8_t* ptr, struct hci_dev_info* di)
{
  put_uint16(ptr, di->dev_id); ptr += sizeof(di->dev_id);
  memcpy(ptr, di->name, sizeof(di->name)); ptr += sizeof(di->name);
  memcpy(ptr, &di->bdaddr, sizeof(di->bdaddr)); ptr += sizeof(di->bdaddr);
  put_uint32(ptr, di->flags); ptr += sizeof(di->flags);
  put_uint8(ptr, di->type); ptr += sizeof(di->type);
  memcpy(ptr, di->features, sizeof(di->features)); ptr += sizeof(di->features);
  put_uint32(ptr, di->pkt_type); ptr += sizeof(di->pkt_type);
  put_uint32(ptr, di->link_policy); ptr += sizeof(di->link_policy);
  put_uint32(ptr, di->link_mode); ptr += sizeof(di->link_mode);

  put_uint16(ptr, di->acl_mtu); ptr += sizeof(di->acl_mtu);
  put_uint16(ptr, di->acl_pkts); ptr += sizeof(di->acl_pkts);
  put_uint16(ptr, di->sco_mtu); ptr += sizeof(di->sco_mtu);
  put_uint16(ptr, di->sco_pkts); ptr += sizeof(di->sco_pkts);

  return put_hci_dev_stats(ptr, &di->stat);
}

static uint8_t* put_hci_conn_info(uint8_t* ptr, struct hci_conn_info* ci)
{
  put_uint16(ptr, ci->handle);
  ptr += sizeof(ci->handle);
  memcpy(ptr, &ci->bdaddr, sizeof(ci->bdaddr));
  ptr += sizeof(ci->bdaddr);
  put_uint8(ptr, ci->type);
  ptr += sizeof(ci->type);
  put_uint8(ptr, ci->out);
  ptr += sizeof(ci->out);
  put_uint16(ptr, ci->state);
  ptr += sizeof(ci->state);
  put_uint32(ptr, ci->link_mode);
  ptr += sizeof(ci->link_mode);
  return ptr;
}

static uint8_t* put_hci_filter(uint8_t* ptr, struct hci_filter* fp)
{
  put_uint32(ptr, fp->type_mask); ptr += sizeof(fp->type_mask);
  put_uint32(ptr, fp->event_mask[0]); ptr += sizeof(fp->event_mask[0]);
  put_uint32(ptr, fp->event_mask[1]); ptr += sizeof(fp->event_mask[1]);
  put_uint16(ptr, fp->opcode); ptr += sizeof(fp->opcode);
  return ptr;
}

static uint8_t* get_hci_filter(uint8_t* ptr, struct hci_filter* fp)
{

  fp->type_mask = get_uint32(ptr); ptr += sizeof(fp->type_mask);
  fp->event_mask[0] = get_uint32(ptr); ptr += sizeof(fp->event_mask[0]);
  fp->event_mask[1] = get_uint32(ptr); ptr += sizeof(fp->event_mask[1]);
  fp->opcode = get_uint16(ptr); ptr += sizeof(fp->opcode);
  return ptr;
}

static int        hci_drv_init(void);
static void       hci_drv_finish(void);
static void       hci_drv_stop(ErlDrvData);
static void       hci_drv_output(ErlDrvData,char*,ErlDrvSizeT);
#if 0
static void       hci_drv_outputv(ErlDrvData, ErlIOVec*);
#endif
static void       hci_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void       hci_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData hci_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT hci_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**,ErlDrvSizeT);
static void       hci_drv_timeout(ErlDrvData);
static void       hci_drv_stop_select(ErlDrvEvent event, void* arg);

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

static void* hci_drv_realloc_buffer(hci_drv_ctx_t* ctx, size_t len)
{
    if (ctx->hcibuf_len < len) {
      ctx->hcibuf = driver_realloc(ctx->hcibuf, len);
      ctx->hcibuf_len = len;
    }
    return ctx->hcibuf;
}

static int hci_drv_init(void)
{
    INIT_ATOM(ok);
    INIT_ATOM(error);
    INIT_ATOM(undefined);
    INIT_ATOM(true);
    INIT_ATOM(false);
    return 0;
}

static void hci_drv_finish(void)
{
}

static void hci_drv_stop(ErlDrvData d)
{
  hci_drv_ctx_t* ctx = (hci_drv_ctx_t*) d;

  if (ctx) {
    if (ctx->is_selecting)
      driver_select(ctx->port, ctx->fd, ERL_DRV_READ, 0);
    if (ctx->is_sending)
      driver_select(ctx->port, ctx->fd, ERL_DRV_WRITE, 0);
    driver_select(ctx->port, ctx->fd, ERL_DRV_USE, 0);
    driver_free(ctx);
  }
}

// #define USE_WRITEV

static void hci_drv_output(ErlDrvData d, char* buf,ErlDrvSizeT len)
{
  hci_drv_ctx_t* ctx = (hci_drv_ctx_t*) d;
  int n;

  if ((n = driver_sizeq(ctx->port)) > 0) {
    driver_enq(ctx->port, buf, len);
    DEBUGF("hci_drv_tput: put on queue pending=%d", n+len);
  }
  else  {  // try send directly
    // data format must be 
    // <<HCI_COMMAND_PKT,Opcode:16,Plen:8,Param:Plen/binary>>
#ifdef USE_WRITEV
    struct iovec iv[3];
    int ivn;
    // try write with writev !!!
    iv[0].iov_base = buf;
    iv[0].iov_len  = 1;
    iv[1].iov_base = buf+1;
    iv[1].iov_len  = HCI_COMMAND_HDR_SIZE;
    iv[2].iov_base = buf+4;
    iv[2].iov_len  = buf[3];
    ivn = buf[3] ? 3 : 2;
    DEBUGF("writev = %d, len=%d", 1+HCI_COMMAND_HDR_SIZE+buf[3], len);

    n = writev(INT_EVENT(ctx->fd), iv, ivn);
#else
    n = write(INT_EVENT(ctx->fd), buf, len);
#endif
    if (n < 0) {
      ERRORF("write error=%s", strerror(errno));
      if ((errno == EAGAIN) || (errno = ENOBUFS)) {
	DEBUGF("hci_drv: put %d bytes on queue", n);
	driver_enq(ctx->port, buf, len);
	driver_select(ctx->port, ctx->fd, ERL_DRV_WRITE, 1);
	ctx->is_sending = 1;
      }
    }
    else if (n < (int)len) {
      driver_enq(ctx->port, buf+n, len+n);
      driver_select(ctx->port, ctx->fd, ERL_DRV_WRITE, 1);
      ctx->is_sending = 1;
    }
    else {
      DEBUGF("hci_drv: wrote %d bytes", n);
    }
  }
}

// hci socket triggered process data
static void hci_drv_ready_input(ErlDrvData d, ErlDrvEvent event)
{
  hci_drv_ctx_t* ctx = (hci_drv_ctx_t*) d;
  uint8_t buf[HCI_MAX_EVENT_SIZE];
  int n;
  (void) event;

  if ((n = read(INT_EVENT(ctx->fd), buf, sizeof(buf))) < 0) {
    if ((errno == EAGAIN) || (errno == EINTR))
      return;
    DEBUGF("hci_drv_ready_input: read error %d, %s",
	   errno, strerror(errno));
    return;
  }
  DEBUGF("hci_drv_ready_input: got %d bytes", n);
  if (n > 0)
    driver_output(ctx->port, (char*) buf, n);
}

static void hci_drv_ready_output(ErlDrvData d, ErlDrvEvent event)
{
    hci_drv_ctx_t* ctx = (hci_drv_ctx_t*) d;
    int vsize;
    SysIOVec* iovp;
    int n;

    DEBUGF("hci_drv_ready_output called");

    if (ctx->fd != event) {
	DEBUGF("hci_drv_ready_output bad event");
	return;
    }

    if ((iovp = driver_peekq(ctx->port, &vsize)) == NULL) {
	driver_select(ctx->port, ctx->fd, ERL_DRV_WRITE, 0);
	ctx->is_sending = 0;
	return;
    }
    vsize = vsize > MAX_VSIZE ? MAX_VSIZE : vsize;
    
    DEBUGF("hci_drv_ready_output: try send vsize=%d", vsize);    
    n = writev(INT_EVENT(ctx->fd), (const struct iovec*) iovp, vsize);
    if (n < 0) {
	if ((errno == EAGAIN) || (errno == ENOBUFS))
	    return;
	ERRORF("write error=%s", strerror(errno));
	return;
    }
    DEBUGF("hci_drv_ready_output: sent %d bytes", n);
    if (driver_deq(ctx->port, n) == 0) {
	driver_select(ctx->port, ctx->fd, ERL_DRV_WRITE, 0);
	ctx->is_sending = 0;
    }
}


static ErlDrvSSizeT hci_drv_ctl(ErlDrvData d,unsigned int cmd,char* buf0,
			       ErlDrvSizeT len,char** rbuf,ErlDrvSizeT rsize)
{
    uint8_t* buf = (uint8_t*) buf0;
    hci_drv_ctx_t* ctx = (hci_drv_ctx_t*) d;

    DEBUGF("hci_drv_ctl cmd=%d", cmd);

    switch(cmd) {
    case CMD_ACTIVE: {
	int active;

	if (len != 4)
	    goto badarg;
	active = get_int32(buf);
	if (active) {
	  if (!ctx->is_selecting) {
		driver_select(ctx->port, ctx->fd, ERL_DRV_READ, 1);
		DEBUGF("selecting %d for read\n", ctx->fd);
	  }
	  ctx->is_selecting = 1;
	  ctx->active = active;
	}
	else {
	  if (ctx->is_selecting) {
		driver_select(ctx->port, ctx->fd, ERL_DRV_READ, 0);
		DEBUGF("deselecting %d for read\n", ctx->fd);
	  }
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

    case CMD_BIND: {
      struct sockaddr_hci a;
      int dev_id;

      if (len != 4)
	goto badarg;
      if ((dev_id = get_int32(buf)) < 0) {
	errno = ENODEV;
	goto error;
      }
      memset(&a, 0, sizeof(a));
      a.hci_family = AF_BLUETOOTH;
      a.hci_dev = dev_id;

      if (bind(INT_EVENT(ctx->fd), (struct sockaddr *) &a, sizeof(a)) < 0)
	goto error;
      ctx->dev_id = dev_id;
      goto ok;
    }

    case CMD_GETFILTER: {
      struct hci_filter filt;
      socklen_t flen;
      uint8_t lbuf[sizeof(struct hci_filter)];
      uint8_t* lptr;      

      flen = sizeof(filt);
      if (getsockopt(INT_EVENT(ctx->fd), SOL_HCI, HCI_FILTER, &filt, &flen) < 0)
	goto error;
      lptr = put_hci_filter(lbuf, &filt);
      return ctl_reply(CTL_BIN, lbuf, lptr-lbuf, rbuf, rsize);
    }

    case CMD_SETFILTER: {
      struct hci_filter filt;

      if (len != 14)
	goto badarg;
      (void) get_hci_filter(buf, &filt);
      if (setsockopt(INT_EVENT(ctx->fd), SOL_HCI, HCI_FILTER, &filt,
		     sizeof(filt)) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCIDEVUP: {
      int dev_id;
      if (len != 4)
	goto badarg;
      dev_id = get_int32(buf);
      if (ioctl(INT_EVENT(ctx->fd), HCIDEVUP, dev_id) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCIDEVDOWN: {
      int dev_id;
      if (len != 4)
	goto badarg;
      dev_id = get_int32(buf);
      if (ioctl(INT_EVENT(ctx->fd), HCIDEVDOWN, dev_id) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCIDEVRESET: {
      int dev_id;
      if (len != 4)
	goto badarg;
      dev_id = get_int32(buf);
      if (ioctl(INT_EVENT(ctx->fd), HCIDEVRESET, dev_id) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCIDEVRESTAT: {
      int dev_id;
      if (len != 4)
	goto badarg;
      dev_id = get_int32(buf);
      if (ioctl(INT_EVENT(ctx->fd), HCIDEVRESTAT, dev_id) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCIGETDEVLIST: {
      struct hci_dev_list_req *dl;
      size_t sz = HCI_MAX_DEV*sizeof(struct hci_dev_req) +
	sizeof(struct hci_dev_list_req);
      uint8_t lbuf[HCI_MAX_DEV*sizeof(struct hci_dev_req)];
      uint8_t* lptr;
      int i;

      if ((dl = hci_drv_realloc_buffer(ctx, sz)) == NULL)
	goto error;
      memset(dl, 0, sz);
      dl->dev_num = HCI_MAX_DEV;
      
      if (ioctl(INT_EVENT(ctx->fd), HCIGETDEVLIST, (void *) dl) < 0)
	goto error;
      
      lptr = lbuf;
      for (i = 0; i < dl->dev_num; i++) {
	put_uint16(lptr, dl->dev_req[i].dev_id); 
	lptr += sizeof(dl->dev_req[i].dev_id);
	put_uint32(lptr, dl->dev_req[i].dev_opt);
	lptr += sizeof(dl->dev_req[i].dev_opt);
      }
      return ctl_reply(CTL_BIN, lbuf, lptr-lbuf, rbuf, rsize);
    }

    case CMD_HCIGETDEVINFO: {
      size_t sz = sizeof(struct hci_dev_info);
      uint8_t lbuf[sizeof(struct hci_dev_info)];
      uint8_t* lptr;
      struct hci_dev_info* di;
      int dev_id;

      if (len != 4)
	goto badarg;
      dev_id = get_int32(buf);

      if ((di = hci_drv_realloc_buffer(ctx, sz)) == NULL)
	goto error;
      memset(di, 0, sz);
      
      di->dev_id = dev_id;
      if (ioctl(INT_EVENT(ctx->fd), HCIGETDEVINFO, (void *) di) < 0)
	goto error;
      lptr = put_hci_dev_info(lbuf, di);
      return ctl_reply(CTL_BIN, lbuf, lptr-lbuf, rbuf, rsize);
    }

    case CMD_HCIGETCONNLIST: {
      struct hci_conn_list_req *cl;
      size_t sz = sizeof(struct hci_conn_list_req) +
	MAX_CONN*sizeof(struct hci_conn_info);
      uint8_t lbuf[MAX_CONN*sizeof(struct hci_conn_info)];
      uint8_t* lptr;
      int dev_id;
      int i;

      if (len != 4)
	goto badarg;
      dev_id  = get_int32(buf);

      if ((cl = hci_drv_realloc_buffer(ctx, sz)) == NULL)
	goto error;
      memset(cl, 0, sz);
      cl->dev_id = dev_id;
      cl->conn_num = MAX_CONN;

      if (ioctl(INT_EVENT(ctx->fd), HCIGETCONNLIST, (void *)cl) < 0)
	goto error;
	
      lptr = lbuf;
      for (i=0; i< cl->conn_num; i++)
	lptr = put_hci_conn_info(lptr, &cl->conn_info[i]);
      return ctl_reply(CTL_BIN, lbuf, lptr-lbuf, rbuf, rsize);
    }

    case CMD_HCIGETCONNINFO: {
      struct hci_conn_info_req *ci;
      size_t sz = sizeof(struct hci_conn_info_req) +
	sizeof(struct hci_conn_info);
      uint8_t lbuf[sizeof(struct hci_conn_info)];
      uint8_t* lptr;

      if (len != 7)
	goto badarg;
      
      if ((ci = hci_drv_realloc_buffer(ctx, sz)) == NULL)
	goto error;
      memset(ci, 0, sz);
      memcpy(&ci->bdaddr, buf, sizeof(bdaddr_t));
      ci->type = buf[6];

      if (ioctl(INT_EVENT(ctx->fd), HCIGETCONNINFO, (void *)ci) < 0)
	goto error;
      lptr = put_hci_conn_info(lbuf, &ci->conn_info[0]);
      return ctl_reply(CTL_BIN, lbuf, lptr-lbuf, rbuf, rsize);
    }

    case CMD_HCIGETAUTHINFO: {
      struct hci_auth_info_req ai;
      if (len != 6)
	goto badarg;
      memcpy(&ai.bdaddr, buf, sizeof(ai.bdaddr));
      if (ioctl(INT_EVENT(ctx->fd), HCIGETAUTHINFO, (void *)&ai) < 0)
	goto error;
      return ctl_reply(CTL_BIN, &ai.type, 1, rbuf, rsize);
    }

    case CMD_HCISETRAW: { // guessing...
      int dev_id;
      if (len != 4)
	goto badarg;
      dev_id = get_int32(buf);
      if (ioctl(INT_EVENT(ctx->fd), HCISETRAW, dev_id) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCISETSCAN: {
      struct hci_dev_req dr;
      if (len != 8)
	goto badarg;
      dr.dev_id  = get_int32(buf);
      dr.dev_opt = get_uint32(buf+4);
      if (ioctl(INT_EVENT(ctx->fd), HCISETSCAN, (void*) &dr) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCISETAUTH: {
      struct hci_dev_req dr;
      if (len != 5)
	goto badarg;
      dr.dev_id = get_int32(buf);
      dr.dev_opt = buf[4] ? AUTH_ENABLED : AUTH_DISABLED;
      if (ioctl(INT_EVENT(ctx->fd), HCISETAUTH, (void*) &dr) < 0)
	goto error;
      goto ok;
    }
      
    case CMD_HCISETENCRYPT: {
      struct hci_dev_req dr;
      if (len != 5)
	goto badarg;
      dr.dev_id = get_int32(buf);
      dr.dev_opt = buf[4] ? ENCRYPT_P2P : ENCRYPT_DISABLED;
      if (ioctl(INT_EVENT(ctx->fd), HCISETAUTH, (void*) &dr) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCISETPTYPE: {
      struct hci_dev_req dr;
      if (len != 8)
	goto badarg;
      dr.dev_id  = get_int32(buf);
      dr.dev_opt = get_uint32(buf+4);
      if (ioctl(INT_EVENT(ctx->fd), HCISETPTYPE, (void*) &dr) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCISETLINKPOL: {
      struct hci_dev_req dr;
      if (len != 8)
	goto badarg;
      dr.dev_id  = get_int32(buf);
      dr.dev_opt = get_uint32(buf+4);
      if (ioctl(INT_EVENT(ctx->fd), HCISETLINKPOL, (void*) &dr) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCISETLINKMODE: {
      struct hci_dev_req dr;
      if (len != 8)
	goto badarg;
      dr.dev_id  = get_int32(buf);
      dr.dev_opt = get_uint32(buf+4);
      if (ioctl(INT_EVENT(ctx->fd), HCISETLINKMODE, (void*) &dr) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCISETACLMTU: {
      struct hci_dev_req dr;
      uint16_t mtu, mpkt;
       
      if (len != 8)
	goto badarg;
      dr.dev_id  = get_int32(buf);
      mtu  = get_uint16(buf+4);
      mpkt = get_uint16(buf+6);
      dr.dev_opt = htobl(htobs(mpkt) | (htobs(mtu) << 16));
      if (ioctl(INT_EVENT(ctx->fd), HCISETACLMTU, (void*) &dr) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCISETSCOMTU: {
      struct hci_dev_req dr;
      uint16_t mtu, mpkt;

      if (len != 8)
	goto badarg;
      dr.dev_id  = get_int32(buf);
      mtu  = get_uint16(buf+4);
      mpkt = get_uint16(buf+6);
      dr.dev_opt = htobl(htobs(mpkt) | (htobs(mtu) << 16));
      if (ioctl(INT_EVENT(ctx->fd), HCISETSCOMTU, (void*) &dr) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCIBLOCKADDR: {  // bound
      bdaddr_t bdaddr;
      if (len != 6)
	goto badarg;
      memcpy(&bdaddr, buf, 6);
      if (ioctl(INT_EVENT(ctx->fd), HCIBLOCKADDR, (void*) &bdaddr) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCIUNBLOCKADDR: {  // bound, bdaddr={0,0,0,0,0,0} => all
      bdaddr_t bdaddr;
      if (len != 6)
	goto badarg;
      memcpy(&bdaddr, buf, 6);
      if (ioctl(INT_EVENT(ctx->fd), HCIUNBLOCKADDR, (void*) &bdaddr) < 0)
	goto error;
      goto ok;
    }

    case CMD_HCIINQUIRY: {
      // blocking! must be in a thread or use the async api
      uint8_t ibuf[sizeof(struct hci_inquiry_req)+sizeof(inquiry_info)*255];
      struct hci_inquiry_req* ir;

      if (len != 9)
	goto badarg;
      ir = (struct hci_inquiry_req *) ibuf;
      ir->length = get_uint8(buf);
      ir->num_rsp = get_uint8(buf+1);
      ir->lap[0]  = get_uint8(buf+2);
      ir->lap[1]  = get_uint8(buf+3);
      ir->lap[2]  = get_uint8(buf+4);
      ir->flags   = get_uint32(buf+5);
      ir->dev_id  = ctx->dev_id;

      if (ioctl(INT_EVENT(ctx->fd), HCIINQUIRY, (unsigned long) ibuf) < 0)
	goto error;

      // return size is sizoef(inquiry_info) * ir->num_rsp
      return ctl_reply(CTL_BIN, ibuf + sizeof(struct hci_inquiry_req),
		       sizeof(inquiry_info) * ir->num_rsp, rbuf, rsize);
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


static void hci_drv_timeout(ErlDrvData d)
{
    (void) d;
    fprintf(stderr, "hci_drv_timeout called!!!\r\n");
}

static void hci_drv_stop_select(ErlDrvEvent event, void* arg)
{    
    (void) arg;
    DEBUGF("eth_drv: stop_select event=%d", INT_EVENT(event));
    close(INT_EVENT(event));
}


static ErlDrvData hci_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    hci_drv_ctx_t* ctx;
    int fd;
    int protocol;
    char* ptr;
    // char* arg;

    ptr = command;
    while(*ptr && (*ptr != ' '))  ptr++;   // skip command
    while(*ptr && (*ptr == ' '))  ptr++;   // and blanks
    // arg = ptr;
    // while(*ptr && (*ptr >= '0') && (*ptr <= '9')) ptr++;
    // if ((arg == ptr) || (*ptr != '\0')) {
    //   errno = EINVAL;
    //   return ERL_DRV_ERROR_ERRNO;
    // }
    // protocol = atoi(arg);
    protocol = BTPROTO_HCI;
    if ((fd = socket(PF_BLUETOOTH,
		     SOCK_NONBLOCK| SOCK_RAW | SOCK_CLOEXEC,
		     protocol)) < 0)
      return ERL_DRV_ERROR_ERRNO;

    // flags = fcntl(fd, F_GETFL, 0);
    // fcntl(fd, F_SETFL, flags | O_NONBLOCK);

    if (!(ctx = driver_alloc(sizeof(hci_drv_ctx_t))))
      return ERL_DRV_ERROR_ERRNO;
    memset(ctx, 0, sizeof(hci_drv_ctx_t));
    ctx->port = port;
    ctx->dport = driver_mk_port(port);
    ctx->owner = driver_caller(port);
    ctx->protocol = protocol;
    ctx->fd       = (ErlDrvEvent)((long)fd);
    ctx->active = 0;
    ctx->is_selecting = 0;
    ctx->is_sending = 0;

    // create i/o buffer
    hci_drv_realloc_buffer(ctx, MIN_HCI_BUFSIZE);
#ifdef PORT_CONTROL_BINARY
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
#endif
    return (ErlDrvData) ctx;
}

DRIVER_INIT(hci_drv)
{
    ErlDrvEntry* ptr = &hci_drv_entry;

    ptr->driver_name = "hci_drv";
    ptr->init  = hci_drv_init;
    ptr->start = hci_drv_start;
    ptr->stop  = hci_drv_stop;
    ptr->output = hci_drv_output;
    ptr->ready_input  = hci_drv_ready_input;
    ptr->ready_output = hci_drv_ready_output;
    ptr->finish = hci_drv_finish;
    ptr->control = hci_drv_ctl;
    ptr->timeout = hci_drv_timeout;
#if 0
    ptr->outputv = hci_drv_outputv;
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
    ptr->stop_select = hci_drv_stop_select;

    return (ErlDrvEntry*) ptr;
}
