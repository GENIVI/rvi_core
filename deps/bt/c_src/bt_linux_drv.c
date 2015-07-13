//
// Work in progress (linux) bluetooth driver
//

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>
#include <string.h>
#include <memory.h>
#include <poll.h>
#include <errno.h>
#include <fcntl.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/l2cap.h>
#include <bluetooth/rfcomm.h>
#include <bluetooth/hci.h>
#include <bluetooth/hci_lib.h>

#include "dthread/include/dlog.h"
#include "dthread/include/ddata.h"
#include "bt_drv.h"
#include "bt_poll.h"

#define RFCOMM_CHANNEL_ID_IS_VALID(x) (((x) >= 1) && ((x) <= 30))
#define PSM_IS_VALID(x) (((x) >= 0) && ((x) <= 0xffff))

#define PTR2INT(x) ((int)((long)(x)))
#define INT2PTR(x) ((void*)((long)(x)))

#define alloc_type(type) calloc(1, sizeof(type))

typedef struct _drv_data_t {
    int dev_id;
    int mgmt_sock;
    int dev_sock;
    int inquiry_pid;
    int inquiry_fd;
} drv_data_t;

int set_nonblock(int fd)
{
   int flags;
#if defined(O_NONBLOCK)
    if (-1 == (flags = fcntl(fd, F_GETFL, 0)))
        flags = 0;
    return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
#else
    flags = 1;
    return ioctl(fd, FIOBIO, &flags);
#endif
}


#define ERR_SHORT 1
#define ERR_LONG  2

typedef struct {
    bt_ctx_t* ctx;  // access to subscription list
    int pending_accepts;
    subscription_list_t acceptors;
} linux_listen_queue_t;


static int mgmt_open(void)
{
	struct sockaddr_hci addr;
	int sk;

	sk = socket(AF_BLUETOOTH, SOCK_RAW, BTPROTO_HCI);
	if (sk < 0) {
		fprintf(stderr, "socket: %s\n", strerror(errno));
		return sk;
	}

	memset(&addr, 0, sizeof(addr));
	addr.hci_family = AF_BLUETOOTH;
	addr.hci_dev = HCI_DEV_NONE;
	addr.hci_channel = HCI_CHANNEL_CONTROL;

	if (bind(sk, (struct sockaddr *) &addr, sizeof(addr)) < 0) {
	    fprintf(stderr, "bind: %s\n", strerror(errno));
	    close(sk);
	    return -1;
	}

	return sk;
}


static inline int get_address(ddata_t* data, bdaddr_t* val)
{
    if (ddata_r_avail(data) >= sizeof(bdaddr_t)) {
	// For some reason, the address is reversed when it
	// hits bt_linux_drv.
	val->b[0] = *(data->rd+5);
	val->b[1] = *(data->rd+4);
	val->b[2] = *(data->rd+3);
	val->b[3] = *(data->rd+2);
	val->b[4] = *(data->rd+1);
	val->b[5] = *(data->rd+0);

	// Was: memcpy(val, data->rd, sizeof(bdaddr_t));
	data->rd += sizeof(bdaddr_t);
	return 1;
    }
    return 0;
}

static inline void ddata_put_addr(ddata_t* data, bdaddr_t* addr)
{
    uint8_t* ptr = ddata_alloc(data, sizeof(bdaddr_t)+1);
    *ptr++ = ADDR;
    *ptr++ = addr->b[5];
    *ptr++ = addr->b[4];
    *ptr++ = addr->b[3];
    *ptr++ = addr->b[2];
    *ptr++ = addr->b[1];
    *ptr++ = addr->b[0];
}


static void bt_local_info(drv_data_t* lctx, ddata_t* data_in, ddata_t* data_out)
{
    uint8_t op_code;

    ddata_put_tag(data_out, LIST);

    while(ddata_get_uint8(data_in, &op_code)) {
	switch(op_code) {
	case NFO_LOCAL_NAME: {
	    struct hci_dev_info di;

	    DEBUGF("local_info: local name");

	    hci_devinfo(lctx->dev_id, &di);

	    ddata_put_string(data_out, di.name);
	    break;
	}

	case NFO_LOCAL_CLASS: {
	    uint8_t cls_str[4];	    
	    uint32_t cls;

	    memset(cls_str, 0, sizeof(cls_str));
	    // Send in the lowest three bytes of the uint32 to 
	    // retrieve the three byte class
	    if (hci_read_class_of_dev(lctx->dev_sock, cls_str+1, 0) == -1) {
		DEBUGF("hci_read_class_of_dev() failed: %s", strerror(errno));
		break;
	    }
	    
	    cls = *((int*)cls_str);

	    DEBUGF("local_info: local class: %X", cls);
	    ddata_put_uint32(data_out, cls);
	    break;
	}

	case NFO_LOCAL_ADDRESS: {
	    struct hci_dev_info di;
	    DEBUGF("local_info: local address");
	    hci_devinfo(lctx->dev_id, &di);
	    ddata_put_addr(data_out, &(di.bdaddr));
	    break;
	}
	    
	case NFO_LOCAL_DISCOVERABLE: {
	    // Boolean value;
	    // if (IOBluetoothLocalDeviceGetDiscoverable(&value) == kIOReturnSuccess)
	    DEBUGF("local_info: discoverable");
	    ddata_put_boolean(data_out, 1);
	    break;
	}

	case NFO_LOCAL_POWER_STATE: {
	    /* BluetoothHCIPowerState powerState = */
	    /* 	[[IOBluetoothHostController defaultController] powerState]; */
	    /* if (powerState == kBluetoothHCIPowerStateON) */
	    /* 	ddata_put_atom(data_out, "on"); */
	    /* else if (powerState == kBluetoothHCIPowerStateOFF) */
	    /* 	ddata_put_atom(data_out, "off"); */
	    /* else if (powerState == kBluetoothHCIPowerStateUnintialized) */
	    /* 	ddata_put_atom(data_out, "unintialized"); */
	    /* else  */
	    DEBUGF("local_info: power_state");
	    ddata_put_atom(data_out, "on");

	    break;
	}
	default:
	    break;
	}
    }
    ddata_put_tag(data_out, LIST_END);
}


static void launch_inquiry_proc(drv_data_t *lctx, uint32_t max_seconds) {
    inquiry_info *ii = NULL;
    int32_t max_rsp;
    int32_t num_rsp;
    int dev_id, sock, len, flags;
    int i;
    int pfd[2];
    int child_pid;

    pipe(pfd);
    if ((child_pid = fork()) > 0) {
	close(pfd[1]);
	lctx->inquiry_pid = child_pid;
	lctx->inquiry_fd = pfd[0];
	return ;
    }

    // We are child
    close(pfd[0]);
	
    // Open a new socket to the adapter
    // just so that we don't mess up something in the stack
    // by accessing the same fd from two different processes.
    dev_id = hci_get_route(NULL);
    sock = hci_open_dev( dev_id );

    if (dev_id < 0 || sock < 0) {
	num_rsp = -1;
	write(pfd[1], &num_rsp, sizeof(num_rsp));
	close(pfd[1]);
	close(sock);
	exit(0);

    }

    len = (int32_t) ((float) max_seconds / 1.28); // 1.28 second per length unit
    max_rsp = 255;
    flags = IREQ_CACHE_FLUSH;

    ii = (inquiry_info*)malloc(max_rsp * sizeof(inquiry_info));
    
    num_rsp = hci_inquiry(dev_id, len, max_rsp, NULL, &ii, flags);

    if( num_rsp < 0 )  {
	num_rsp = 0;
	write(pfd[1], &num_rsp, sizeof(num_rsp));
	close(pfd[1]);
	free( ii );
	close( sock );
	exit(0);
    }

    bdaddr_t resp[num_rsp];

    // Retrieve address and remote name
    for (i = 0; i < num_rsp; i++) 
	resp[i] = (ii+i)->bdaddr;

    // Write number of responses that we have.
    printf("Num_resp: %d\n", num_rsp);
    write(pfd[1], &num_rsp, sizeof(num_rsp));
    write(pfd[1], resp, sizeof(resp));

    close(pfd[1]);
    free( ii );
    close( sock );
    exit(0);
}



/* type: 1=short-atom  2=string-long  3=both (encapsule in LIST ot TUPLE) */
static void ddata_put_io_error(ddata_t* data, int errnum, int type)
{
    if (errnum == 0) {
	if (type&1) ddata_put_atom(data, "Success");
	if (type&2) ddata_put_string(data, "OK");
    }
    else {
	if (type&1) ddata_put_atom(data, "Error"); // fixme
	if (type&2) ddata_put_string(data, strerror(errnum));	
    }
}


static void cleanup(subscription_t* s)
{
    DEBUGF("cleanup: %s", format_subscription(s));
 
    switch(s->type) {
    case INQUIRY: {
	drv_data_t* lctx = (drv_data_t*) s->handle;
	bt_poll_del(lctx->inquiry_fd);
	close(lctx->inquiry_fd);

	kill(lctx->inquiry_pid, SIGHUP);
	lctx->inquiry_pid = -1;
	lctx->inquiry_fd = -1;
	wait(0);
	break;
    }

    case REMOTE_NAME: break;
    case CONNECT: break;
    case SDP_QUERY: break;
    case SDP: break;
    case RFCOMM: {
	DEBUGF("cleanup: Disabling and closing descriptor %d", PTR2INT(s->handle));
	bt_poll_del(PTR2INT(s->handle));
	shutdown(PTR2INT(s->handle), 2); // Very likely not necessary at all
	close(PTR2INT(s->handle));
	break;
    }
    case RFCOMM_LISTEN: {
	bt_poll_del(PTR2INT(s->handle));
	close(PTR2INT(s->handle));
	break;
    }
    case L2CAP: break;
    case L2CAP_LISTEN: break;
    default:  // warn?
	break;
    }
}

// setup "standard" reply buf, with initial 32 but size 
static void mesg_setup(ddata_t* data, uint8_t* buf, size_t size)
{
    ddata_init(data, buf, size, 0);
    ddata_put_UINT32(data, 0);
}

/* Send OK reply */
static void reply_ok(uint32_t cmdid)
{
    uint8_t buf[16];
    ddata_t data;

    mesg_setup(&data, buf, sizeof(buf));
    ddata_put_tag(&data, REPLY_OK);
    ddata_put_UINT32(&data, cmdid);
    ddata_send(&data, 1);
    ddata_final(&data);
}

/* Send ERROR reply */
static void reply_error(uint32_t cmdid, int err)
{
    uint8_t buf[128];
    ddata_t data;

    mesg_setup(&data, buf, sizeof(buf));
    ddata_put_tag(&data, REPLY_ERROR);
    ddata_put_UINT32(&data, cmdid);
    ddata_put_io_error(&data, err, ERR_SHORT);
    ddata_send(&data, 1);
    ddata_final(&data);
}

// simple event
static void send_event(uint32_t sid, const char* evtname)
{
    uint8_t buf[64];
    ddata_t data;

    mesg_setup(&data, buf, sizeof(buf));
    ddata_put_tag(&data, REPLY_EVENT);
    ddata_put_UINT32(&data, sid);
    ddata_put_atom(&data, evtname);
    ddata_send(&data, 1);
    ddata_final(&data);
}


// CALLBACK 
static void rfcomm_running(struct pollfd* pfd, void* arg)
{
    subscription_t* s = arg;
    uint8_t buf[1024];
    uint8_t bt_data[800];
    int32_t bt_data_len;
    ddata_t data;


    if (pfd->revents & POLLIN) {  // input ready
	DEBUGF("rfcomm_running: %d has input", PTR2INT(s->handle));
	DEBUGF("rfcomm_running: sub %p", s);
	DEBUGF("rfcomm_running: id %d", s->id);

	bt_data_len = read(pfd->fd, bt_data, sizeof(bt_data));
	
	ddata_init(&data, buf, sizeof(buf), 0);
	ddata_put_UINT32(&data, 0);
	ddata_put_tag(&data, REPLY_EVENT);
	ddata_put_UINT32(&data, s->id);
	ddata_put_tag(&data, TUPLE);
	ddata_put_atom(&data, "data");
	ddata_put_binary(&data, bt_data, bt_data_len);
	ddata_put_tag(&data, TUPLE_END);
	ddata_send(&data, 1);
	ddata_final(&data);
    }

    if (pfd->revents & POLLHUP) {  // close
	DEBUGF("rfcomm_running: %d Hangup", PTR2INT(s->handle));
 	ddata_init(&data, buf, sizeof(buf), 0);
	ddata_put_UINT32(&data, 0);
	ddata_put_tag(&data, REPLY_EVENT);
	ddata_put_UINT32(&data, s->id);
	ddata_put_atom(&data, "closed");
	ddata_send(&data, 1);
	ddata_final(&data);

	bt_poll_del(pfd->fd);
	shutdown(pfd->fd, 2);
	close(pfd->fd);
    }

    if (pfd->revents & POLLOUT) {  // output ready
	DEBUGF("rfcomm_running: %d may output", PTR2INT(s->handle));
	// FIXME: Send additional pending data.
    }
}


// CALLBACK 
static void rfcomm_connected(struct pollfd* pfd, void* arg)
{
    subscription_t* s = arg;
    (void) pfd;
    DEBUGF("rfcomm_connected: %d", PTR2INT(s->handle));
    reply_ok(s->cmdid);
    // FIXME: check error
    s->cmdid = 0;
    bt_poll_set_cb(PTR2INT(s->handle),  rfcomm_running);
    bt_poll_set_events(PTR2INT(s->handle), POLLIN);
}



static void rfcomm_accept(struct pollfd* pfd, void* arg)
{
    // We have a pending client connection
    subscription_t *listen_s = (subscription_t*) arg;
    linux_listen_queue_t*  lq = (linux_listen_queue_t*) listen_s->opaque;
    struct sockaddr_rc rem_addr = { 0 };	
    struct sockaddr_rc loc_addr = { 0 };	
    int client_des = 0;
    socklen_t alen = sizeof(rem_addr);
    uint8_t buf[64];
    ddata_t data;
    subscription_link_t* link = 0;
    subscription_t* accept_s = 0;

    if (listen_s->type != RFCOMM_LISTEN) {
	DEBUGF("RFCOMM: not a listen subscription: %d", listen_s->type);
	exit(0);
    }

    //
    // Find a waiting process that is currently accepting traffic
    //
    if (!(link = lq->acceptors.first)) {
	DEBUGF("RFCOMM: no accepting processes. Should not happen");
	exit(0);
    }

    //
    // Is this the last accepting subscription?
    // If so. Remove the listen descriptor from the bt_poll
    // subsystem until we have another acceptor
    //
    if (lq->acceptors.length == 1) {
	DEBUGF("RFCOMM: This is the last accepting process. Will disable listen");
	bt_poll_del(pfd->fd);
    }
    // Retrieve the accepting subscriber process
    accept_s = link->s;
    
    // Remove from acceptors waiting for an accept on this socket.
    unlink_subscription(link);

    client_des = accept(pfd->fd, (struct sockaddr *)&rem_addr, &alen);

    bt_poll_add(client_des, POLLIN | POLLHUP, rfcomm_running, accept_s);

    accept_s->handle = INT2PTR(client_des);
    accept_s->accept = 0; // We are now a regular RFCOMM connection.

    getsockname(client_des, (struct sockaddr *)&loc_addr, &alen);

    DEBUGF("RFCOMM: accept on %X", &loc_addr.rc_channel);
    DEBUGF("RFCOMM: desc %d", client_des);
    DEBUGF("RFCOMM: ptr %p", accept_s);
    DEBUGF("RFCOMM: id %p", accept_s->id);

    ddata_init(&data, buf, sizeof(buf), 0);
    ddata_put_UINT32(&data, 0);
    ddata_put_tag(&data, REPLY_EVENT);
    ddata_put_UINT32(&data, accept_s->id);
    ddata_put_tag(&data, TUPLE);
    ddata_put_atom(&data, "accept");
    ddata_put_addr(&data, &rem_addr.rc_bdaddr);
    ddata_put_uint8(&data, loc_addr.rc_channel);
    ddata_put_tag(&data, TUPLE_END);
    ddata_send(&data, 1);
    ddata_final(&data);
}


// 
static void hci_inquiry_result(struct pollfd* pfd, void* arg)
{
    subscription_t* s = arg;
    uint8_t  out_buf[2048];
    ddata_t data_out;

    DEBUGF("hci_inquiry_result: %d", PTR2INT(s->handle));
    int32_t count;


    // Read count, which may be -1.
    read(pfd->fd, &count, sizeof(count));

    DEBUGF("hci_inquiry_res: %d addresses\n", count);

    ddata_init(&data_out, out_buf, sizeof(out_buf), 0);

    ddata_put_tag(&data_out, REPLY_OK);
    ddata_put_UINT32(&data_out, s->cmdid);
    ddata_put_tag(&data_out, LIST);

    while(count-- > 0) {
	bdaddr_t resp;
	read(pfd->fd, &resp, sizeof(resp));
	ddata_put_addr(&data_out, &resp);
    }

    ddata_put_tag(&data_out, LIST_END);

    bt_poll_del(pfd->fd);
    
    close(pfd->fd);

    // Wait for child process to die.
    wait(0);

    ddata_send(&data_out, 1);
    ddata_final(&data_out);

    s->cmdid = 0;

    // only for Inquiry objects
    release_subscription(s);
}


void bt_command(bt_ctx_t* ctx, const uint8_t* src, uint32_t src_len)
{
    uint8_t  op = 0;
    uint32_t cmdid = 0;
    int      bt_error = 0;
    uint8_t  out_buf[2048];
    ddata_t data_in;
    ddata_t data_out;
    drv_data_t* lctx = (drv_data_t*) ctx->drv_data;

    // dump subscription list
    if (dlog_debug_level == DLOG_DEBUG) {
	subscription_link_t* p = ctx->list.first;
	fprintf(stderr, "ctx.list = {");
	while(p) {
	    fprintf(stderr, " %s,", format_subscription(p->s));
	    p = p->next;
	}
	fprintf(stderr, "}\r\n");
    }

    ddata_r_init(&data_in, (uint8_t*)src, src_len, 0);
    mesg_setup(&data_out, out_buf, sizeof(out_buf));

    if (!ddata_get_uint8(&data_in, &op))
	goto badarg;
    if (!ddata_get_uint32(&data_in, &cmdid))
	goto badarg;

    switch (op) {
    case CMD_PING: {
	DEBUGF("CMD_PING cmdid=%d", cmdid);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_string(&data_out, "pong");
	goto reply;
    }
    case CMD_DEBUG: {  // <<level>>
	int level;
	DEBUGF("CMD_DEBUG cmdid=%d", cmdid);
	if (!ddata_get_int32(&data_in, &level))
	    goto badarg;
	dlog_set_debug(level);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	goto reply;
    }	


    case CMD_RFCOMM_OPEN: { /* id:32 bt-address(6) channel-id:8 */
	uint32_t sid;
	bdaddr_t bt_addr;
	struct sockaddr_rc addr;
	int sock;
	int r;
	uint8_t channel_id;
	subscription_t* s;
	char addrstr[40];

	DEBUGF("CMD_RFCOMM_OPEN cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if(!get_address(&data_in, &bt_addr))
	    goto badarg;
	if(!ddata_get_uint8(&data_in, &channel_id))
	    goto badarg;
	if (!RFCOMM_CHANNEL_ID_IS_VALID(channel_id))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if ((sock = socket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM)) < 0)
	    goto bt_error;

	addr.rc_family = AF_BLUETOOTH;
	addr.rc_channel = channel_id;
	bacpy(&addr.rc_bdaddr, &bt_addr);
	ba2str(&bt_addr, addrstr );
	DEBUGF("Will connect to %s channel %d", addrstr, channel_id);
	if (set_nonblock(sock) < 0) {
	    bt_error = errno;
	    close(sock);
	    goto bt_error;
	}
	if ((r = connect(sock, (struct sockaddr*)&addr, sizeof(addr))) < 0) {
	    if (errno != EINPROGRESS) {
		bt_error = errno;
		close(sock);
		goto bt_error;
	    }
	}

	if ((s = new_subscription(RFCOMM,sid,cmdid,0,cleanup)) == NULL) {
	    close(sock);
	    goto mem_error;
	}
	if (r < 0) // inprogress
	    bt_poll_add(sock, POLLOUT, rfcomm_connected, s);
	else
	    bt_poll_add(sock, POLLIN, rfcomm_running, s);
	s->handle = INT2PTR(sock);
	insert_last(&ctx->list, s);
	break;
    }


    case CMD_RFCOMM_LISTEN: { /* id:32,  channel:8 */
	uint32_t sid = 0;
	uint8_t channel = 0;
	subscription_t* listen_sub = 0;
	linux_listen_queue_t* lq = 0;
	int listen_desc = 0;
	struct sockaddr_rc loc_addr = { 0 };
	int dev_id;
	struct hci_dev_info dev_info;
	char buf[32];

	DEBUGF("CMD_RFCOMM_LISTEN cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;

	if(!ddata_get_uint8(&data_in, &channel))
	    goto badarg;	

	// Fixme auto-allocated channel ID.
	if ((channel == 0) || !RFCOMM_CHANNEL_ID_IS_VALID(channel))
	    goto badarg;

	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;


	if ((listen_sub = new_subscription(RFCOMM_LISTEN,
					   sid,
					   cmdid,
					   NULL,
					   cleanup)) == NULL)
	    goto mem_error;

	if ((lq = alloc_type(linux_listen_queue_t)) == NULL) {
	    release_subscription(listen_sub);
	    goto mem_error;
	}

	dev_id = hci_get_route(NULL);
	hci_devinfo(dev_id, &dev_info);
	ba2str( &dev_info.bdaddr, buf );

	DEBUGF("Listening on device %s\n", buf);

	// allocate socket
	listen_desc = socket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM);

	// bind socket to port 1 of the first available 
	// local bluetooth adapter
	loc_addr.rc_family = AF_BLUETOOTH;
	loc_addr.rc_bdaddr = *BDADDR_ANY;
	loc_addr.rc_channel = (uint8_t) channel;

	if (bind(listen_desc, 
		 (struct sockaddr *) &loc_addr, 
		 sizeof(loc_addr)) == -1) {
	    DEBUGF("bind(%d) failed: %s", channel, strerror(errno));
	    goto error;
	}
	
	// put socket into listening mode
	if (listen(listen_desc, 1) == -1) {
	    DEBUGF("listen(%d) failed: %s", channel, strerror(errno));
	    goto error;
	}

	listen_sub->handle = INT2PTR(listen_desc);
	listen_sub->opaque = (void* ) lq;
	insert_last(&ctx->list, listen_sub);

	// We will not add the listen descriptor to
	// the bt_poll subsystem until we have t least
	// one acceptor.

	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	goto reply;
    }


    case CMD_RFCOMM_ACCEPT: { /* id:32 listen_id:32 */
	uint32_t sid = 0;
	uint32_t listen_id = 0;
	linux_listen_queue_t* lq = 0;
	subscription_t* listen_s = 0;
	subscription_t* s = 0;

	DEBUGF("CMD_RFCOMM_ACCEPT cmdid=%d", cmdid);


	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;

	if (!ddata_get_uint32(&data_in, &listen_id))
	    goto badarg;

	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if (find_subscription(&ctx->list,RFCOMM, sid) != NULL) {
	    DEBUGF("subscription %d already exists", sid);
	    goto badarg;
	}

	if ((listen_s = find_subscription(&ctx->list,
					RFCOMM_LISTEN,listen_id))==NULL) {
	    DEBUGF("listen subscription %d does not exists", listen_id);
	    goto badarg;
	}

	if ((s = new_subscription(RFCOMM,sid,cmdid,NULL,cleanup)) == NULL)
	    goto mem_error;

	s->accept = listen_s;  // mark that we are accepting

	// Are we the first acceptor added to the listen
	// descriptor? If so, enable it.
	lq = (linux_listen_queue_t*) listen_s->opaque;
	if (lq->acceptors.length == 0) {
	    DEBUGF("First acceptor for listen descriptor %d. Add to poll", listen_id);
	    // Call rfcomm_accept with listen subscriber 
	    // when someone connects to us.
	    bt_poll_add(PTR2INT(listen_s->handle), POLLIN, rfcomm_accept, listen_s); 
	}


	insert_last(&lq->acceptors, s);
	insert_last(&ctx->list, s);

	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_send(&data_out, 1);
	
	goto done;
    }

    case CMD_RFCOMM_CLOSE: { /* arguments: id:32 */
	uint32_t sid;
	subscription_link_t* link;

	DEBUGF("CMD_RFCOMM_CLOSE cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if ((link = find_subscription_link(&ctx->list,RFCOMM,sid)) != NULL) {
	    subscription_t* s = link->s;
	    int sock;
	    DEBUGF("CMD_RFCOMM_CLOSE found RFCOMM link", cmdid);
	    s->cmdid = cmdid;
	    sock = PTR2INT(s->handle);
	    if (sock >= 0) {
		DEBUGF("RFCOMM_CLOSE: channel=%d", sock);
		unlink_subscription(link); // Will close and unlink s->handle
		goto ok;
	    }
	    else if (s->accept != NULL) { 
		DEBUGF("RFCOMM_CLOSE: This is an acceptor with no session");
		linux_listen_queue_t* lq = (linux_listen_queue_t*)((s->accept)->opaque); 
		remove_subscription(&lq->acceptors,RFCOMM,sid); 
		unlink_subscription(link); 
		goto ok; 
	    } 
	}
	else if ((link = find_subscription_link(&ctx->list,RFCOMM_LISTEN,sid)) != NULL) {
	    subscription_t* listen = link->s;
	    linux_listen_queue_t* lq = (linux_listen_queue_t*)listen->opaque;
	    subscription_link_t* link1; 
	    DEBUGF("RFCOMM_CLOSE: This is a listen subscriuber");
	    /* remove all waiters */
	    while((link1=lq->acceptors.first) != NULL) { 
	     	send_event(link1->s->id, "closed"); 
	     	unlink_subscription(link1);
	    }
	    unlink_subscription(link);
	    goto ok;
	}
	    DEBUGF("RFCOMM_CLOSE: Shit");
	goto error;
    }


    case CMD_RFCOMM_SEND: { /* id:32, data/rest */
	uint32_t sid;
	subscription_t* s;
	int r;
	int len;
	int sock;

	DEBUGF("CMD_RFCOMM_SEND cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if ((s = find_subscription(&ctx->list,RFCOMM,sid)) == NULL)
	    goto badarg;
	if ((sock = PTR2INT(s->handle)) < 0)
	    goto badarg;
	s->cmdid = cmdid;

	if ((len = ddata_r_avail(&data_in)) > 0) {
	    if (s->out == NULL)
		s->out = ddata_new(data_in.rd, len);
	    else
		ddata_add(s->out, data_in.rd, len);
	    r = write(sock, s->out->rd, ddata_r_avail(s->out));
	    if (r != -1)
		DEBUGF("CMD_RFCOMM_SEND wrote=%d", r);
	    else
		DEBUGF("CMD_RFCOMM_SEND failed: %s", strerror(errno));
		
	    if ((r >= 0) || ((r < 0) && (errno == EINPROGRESS))) {
		if (r < 0) r = 0;
		s->out->rd += r;
		if (ddata_r_avail(s->out) > 0)
		    bt_poll_set_events(PTR2INT(s->handle), POLLIN|POLLOUT);
		else {
		    ddata_reset(s->out);
		    s->cmdid = 0;
		    goto ok;
		}
	    }
	    else {
		s->cmdid = 0;
		goto error;
	    }
	}
	else {
	    s->cmdid = 0;
	    goto ok;
	}
	goto done;
    }


    case CMD_INQUIRY_START: { /* arguments: id:32 secs:32 */
	uint32_t sid;
	uint32_t secs;

	subscription_t* s;
	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;

	if (!ddata_get_uint32(&data_in, &secs))
	    goto badarg;
	
	DEBUGF("CMD_INQUIRY_START: %d", sid); 
	if (lctx->inquiry_pid != -1) { 
	    DEBUGF("CMD_INQUIRY_START: Already in progress"); 
	    goto error; 
	} 

	// hci_inquiry() is very, very blocking. We need to run it in a separate  
	// process in order to maintain asynchronicity. 
	// Will set ctx->drv_data->inquriry_pid and inquiry_fd
	launch_inquiry_proc(lctx, secs); 
	DEBUGF("CMD_INQUIRY_START: process launched"); 

	if ((s = new_subscription(INQUIRY,sid,0,lctx, cleanup)) == NULL)  
	    goto mem_error; 

	
	bt_poll_add(lctx->inquiry_fd, POLLIN, hci_inquiry_result, ctx); 
	insert_last(&ctx->list, s); 
	DEBUGF("CMD_INQUIRY_START: Done");

	goto ok;
    }

    case CMD_INQUIRY_STOP: { /* arguments: id:32 secs:32 */
	uint32_t sid;

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;

	// Will call cleanup, which will handle correct shutdown 
	// of inquiry process.
	remove_subscription(&ctx->list,INQUIRY,sid);

	goto ok;
    }

    case CMD_RECENT_DEVICES: {  
	DEBUGF("CMD_RECENT_DEVICES cmdid=%d: Not supported under linux", cmdid);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_tag(&data_out, LIST);
	ddata_put_tag(&data_out, LIST_END);
	goto reply;
    }	

    case CMD_PAIRED_DEVICES: {  
	DEBUGF("CMD_PAIRED_DEVICES cmdid=%d: Not supported under linux", cmdid);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_tag(&data_out, LIST);
	ddata_put_tag(&data_out, LIST_END);
	goto reply;
    }	

    case CMD_FAVORITE_DEVICES: {  
	DEBUGF("CMD_FAVORITE_DEVICES cmdid=%d: Not supported under linux", cmdid);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_tag(&data_out, LIST);
	ddata_put_tag(&data_out, LIST_END);
	goto reply;
    }	

    case CMD_DEVICE_INFO: {  
	bdaddr_t bt_addr;
	DEBUGF("CMD_DEVICE_INFO cmdid=%d", cmdid);

	if(!get_address(&data_in, &bt_addr))
	    goto badarg;

	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_tag(&data_out, LIST);
	ddata_put_tag(&data_out, LIST_END);
	
	goto reply;
    }	

    case CMD_LOCAL_INFO: {  
	DEBUGF("CMD_LOCAL_INFO cmdid=%d", cmdid);

	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);

	bt_local_info(lctx, &data_in, &data_out);
	
	goto reply;
    }	

    default:
	DEBUGF("CMD_UNKNOWN = %d cmdid=%d", op, cmdid);
	goto badarg;
    }

    goto done;

ok:
    if (cmdid == 0)
	goto done;
    ddata_put_tag(&data_out, REPLY_OK);
    ddata_put_UINT32(&data_out, cmdid);
    goto reply;

mem_error:
    if (cmdid == 0)
	goto done;
    bt_error = ENOMEM;
    goto bt_error;

badarg:
    if (cmdid == 0)
	goto done;
    bt_error = EINVAL;
    goto bt_error;

error:
    bt_error = errno;
bt_error:
/* reset, just in case something was inserted */
    ddata_reset(&data_out);
    ddata_put_UINT32(&data_out, 0);
    ddata_put_tag(&data_out, REPLY_ERROR);
    ddata_put_UINT32(&data_out, cmdid);
    ddata_put_io_error(&data_out, bt_error, ERR_SHORT);
reply:
    ddata_send(&data_out, 1);
done:
    ddata_final(&data_out);
}

// CALLBACK 
void read_callback(struct pollfd* pfd, void* data)
{
    bt_ctx_t* ctx = (bt_ctx_t*) data;
    int fd = pfd->fd;
    int n;

    DEBUGF("read_callback: %d:%X", pfd->fd, pfd->revents);

    if (pfd->revents & POLLHUP)  {
	DEBUGF("hangup");
	goto closed;
    }

    if (ctx->pbuf_len < sizeof(ctx->pbuf)) {
	int r = sizeof(ctx->pbuf) - ctx->pbuf_len;
	if ((n = read(fd, ctx->pbuf+ctx->pbuf_len, r)) < 0)
	    goto error;
	if (n == 0)
	    goto closed;
	ctx->pbuf_len += n;
	DEBUGF("READ: %d pbuf_len=%d", n, ctx->pbuf_len);
	if (ctx->pbuf_len == sizeof(ctx->pbuf)) {
	    ctx->len = (ctx->pbuf[0]<<24) + (ctx->pbuf[1]<<16) +
		(ctx->pbuf[2]<<8) + ctx->pbuf[3];
	    DEBUGF("READ: %d packet len=%d", n, ctx->len);
	    if (ctx->len > 0) {
		ctx->remain = ctx->len;
		ctx->packet = (uint8_t*) malloc(ctx->len);
		ctx->ptr = ctx->packet;
	    }
	    else {
		ctx->remain = 0;
		ctx->pbuf_len = 0;
	    }
	}
    }
    else {
	if ((n = read(fd, (void*)ctx->ptr, ctx->remain)) < 0)
	    goto error;
	if (n == 0)
	    goto closed;
	DEBUGF("READ: %d packet bytes", n);
	ctx->remain -= n;
	DEBUGF("PACKET: remain=%d", ctx->remain);
	ctx->ptr += n;
	if (ctx->remain == 0) {
	    bt_command(ctx, ctx->packet, ctx->len);
	    free(ctx->packet);
	    ctx->packet = NULL;
	    ctx->len = 0;
	    ctx->pbuf_len = 0;
	}
    }
    return;

error:
    DEBUGF("pipe read error",0);
    exit(1);

closed:
    DEBUGF("eof clean-up",0);
    exit(0);
}

static void  main_loop(bt_ctx_t* ctx)
{
    int dev_id = 0;
    int sock = 0;
    drv_data_t local_context;
    
    // FIXME: How the hell do we handle more than one BT adapter.
    dev_id = hci_get_route(NULL);
    sock = hci_open_dev(dev_id);
    if (sock == -1) {
	fprintf(stderr, "Failed to open BT device %d: %s\n", dev_id, strerror(errno));
	exit(0);
    }
    
    // Setup local context
//    local_context.mgmt_sock = mgmt_open();
    local_context.mgmt_sock = -1;
    local_context.dev_id = dev_id;
    local_context.dev_sock = sock;
    local_context.inquiry_fd = -1;
    local_context.inquiry_pid = -1;

    ctx->drv_data = &local_context;
    
    while(1) {
	int r = bt_poll(-1);
	if (r < 0) {
	    DEBUGF("poll error %s", strerror(errno));
	}
    }
}

int main(int argc, char** argv)
{
    bt_ctx_t bt_info;
    (void) argc;
    (void) argv;

    dlog_init();
    dlog_set_debug(DLOG_DEFAULT);
    memset(&bt_info, 0, sizeof(bt_ctx_t));
    
    bt_poll_add(0, POLLIN | POLLERR | POLLHUP, read_callback, &bt_info);
    main_loop(&bt_info);

    dlog_finish();
    DEBUGF("terminate",0);
    exit(0);
}
