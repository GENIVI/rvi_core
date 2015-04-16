//
//  UART "win32" implementation
//
// http://msdn.microsoft.com/en-us/library/ms810467.aspx
//
// http://support.microsoft.com/kb/115831
// devicename = COM1..COM9
// for COM10 and other (includes COM1..COM9)
// devicename = \\.\COM10  == "\\\\.\\COM10"
//
// EscapeCommFunction: (and others?)
// Apparently if a serial device is disconnected while you still have the 
// handle open, this can return at least two interesting error codes: 
// ERROR_ACCESS_DENIED and ERROR_BAD_COMMAND. 
//

#include <stdio.h>
#include "windows.h"

#define EAGAIN       ERROR_IO_PENDING
#define EWOULDBLOCK  ERROR_IO_PENDING
#define ENOMEM       ERROR_NOT_ENOUGH_MEMORY
#define EINVAL       ERROR_BAD_ARGUMENTS
#define EBUSY        ERROR_BUSY
#define EOVERFLOW    ERROR_TOO_MANY_CMDS
#define EMSGSIZE     ERROR_NO_DATA
#define ENOTCONN     ERROR_PIPE_NOT_CONNECTED
#define EINTR        ERROR_INVALID_AT_INTERRUPT_TIME //dummy
#define EBADF        ERROR_INVALID_HANDLE

#include "uart_drv.h"
#include "dthread.h"

// debug output - preserve last error
#ifdef DEBUG
#define DEBUG_ERROR(args...) do {			\
    DWORD error = GetLastError();			\
    dlib_emit_error(DLOG_DEBUG,__FILE__,__LINE__,args);	\
    SetLastError(error);				\
    } while(0)
#else
#define DEBUG_ERROR(args...)
#endif

#define DEFAULT_IN_QUEUE   2048
#define DEFAULT_OUT_QUEUE  2048

static struct _rate {
    int baud;
    DWORD speed;
} rate_tab[] = 
{
    {0,      0},
    {110,    CBR_110},
    {300,    CBR_300},
    {600,    CBR_600},
    {1200,   CBR_1200},
    {2400,   CBR_2400},
    {4800,   CBR_4800},
    {9600,   CBR_9600},
    {14400,  CBR_14400},
    {19200,  CBR_19200},
    {38400,  CBR_38400},
    {57600,  CBR_57600},
    {115200, CBR_115200},
    {128000, CBR_128000},
    {256000, CBR_256000},
    { -1,    0}
};

extern void _dosmaperr(DWORD);
extern int  errno;

static int uart_errno(uart_ctx_t* ctx)
{
    int error = GetLastError();
    _dosmaperr(error);
    ctx->error = errno;
    return errno;
}

static int from_speed(DWORD speed)
{
    int i = 0;
    int baud;

    while((rate_tab[i].baud != -1) && (rate_tab[i].speed != speed))
	i++;
    baud = rate_tab[i].baud;
    return baud;
}

static DWORD to_speed(int baud)
{
    int i = 0;
    int speed = 0;
    while((rate_tab[i].baud != -1) && (baud > rate_tab[i].baud))
	i++;
    if (rate_tab[i].baud == -1)
	speed = rate_tab[i-1].speed;
    else 
	speed = rate_tab[i].speed;
    return speed;
}



//
// t1 - t0 - t1 MUST be greater equal to t0 (later in time)
// or 0 is returned
//
unsigned long diff_time_ms(ErlDrvNowData* t1, ErlDrvNowData* t0)
{
    unsigned long dm,ds,du;
    unsigned long d = 0;
    unsigned long b;
    unsigned long t;

    if (t1->microsecs >= t0->microsecs) {
	du = t1->microsecs - t0->microsecs;
	b = 0;
    }
    else {
	du = (1000000 + t1->microsecs) - t0->microsecs;
	b = 1;
    }

    t = t0->secs+b;
    if (t1->secs >= t) {
	ds = t1->secs - t;
	b = 0;
    }
    else {
	ds = (t1->secs+1000000) - t;
	b = 1;
    }

    t = t0->megasecs+b;
    if (t1->megasecs >= t) {
	dm = t1->megasecs - t;
	b = 0;
    }
    else {
	dm = (t1->megasecs+1000000) - t;
	b = 1;
    }
    if (b == 1)
	return 0;
    d = du/1000;
    if (ds)
	d += ds*1000;
    if (dm) 
	d += dm*1000000000;
    return d;
}

void clear_timeout(uart_ctx_t* ctx)
{
    ctx->recv = 0;
    ctx->tp = NULL;
}

void set_timeout(uart_ctx_t* ctx, uint32_t tmo)
{
    if (tmo == 0xFFFFFFFF)
	return;
    driver_get_now(&ctx->t0);
    ctx->tp = &ctx->t0;
    ctx->tmo = tmo;
}

int next_timeout(uart_ctx_t* ctx)
{
    ErlDrvNowData t1;
    unsigned long td;
    if (ctx->tp == NULL)
	return -1;
    driver_get_now(&t1);
    td = diff_time_ms(&t1, ctx->tp);
    if (td >= ctx->tmo)
	return 0;
    return ctx->tmo - td;
}

static int open_device(uart_ctx_t* ctx, char* name)
{
    ctx->fh = CreateFile(name,
			 GENERIC_READ | GENERIC_WRITE, 
			 0, 
			 0, 
			 OPEN_EXISTING,
			 FILE_FLAG_OVERLAPPED,
			 0);
    if (ctx->fh == INVALID_HANDLE_VALUE) {
	DEBUG_ERROR("CreateFile: invalid handle: error %d", GetLastError());
	goto error;
    }

    if (!(ctx->in.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL))) {
	DEBUG_ERROR("CreateEvent: 1: error %d", GetLastError());
	goto error;
    }

    if (!(ctx->out.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL))) {
	DEBUG_ERROR("CreateEvent: 1: error %d", GetLastError());
	goto error;
    }

    if (!(ctx->stat.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL))) {
	DEBUG_ERROR("CreateEvent: 1: error %d", GetLastError());
	goto error;
    }

    // SetCommMask(wd->fh, EV_RXCHAR);
    WaitCommEvent(ctx->fh, &ctx->statm, &ctx->stat);
    return 0;

error:
    if (ctx->fh != INVALID_HANDLE_VALUE) CloseHandle(ctx->fh);
    if (ctx->in.hEvent) CloseHandle(ctx->in.hEvent);
    if (ctx->out.hEvent) CloseHandle(ctx->out.hEvent);
    if (ctx->stat.hEvent) CloseHandle(ctx->stat.hEvent);
    return -1;
}

static void close_device(uart_ctx_t* ctx)
{
    if (ctx->fh != INVALID_HANDLE_VALUE) { 
	DEBUGF("close_device: %d", ctx->fh);
	CloseHandle(ctx->fh);
    }
    if (ctx->in.hEvent) {
	if (ctx->reading) {
	    CancelIo(ctx->in.hEvent);
	    // waitfor it (ERROR_OPERATION_ABORTED)
	}
	CloseHandle(ctx->in.hEvent);
	ctx->in.hEvent = NULL;
    }
    if (ctx->out.hEvent) {
	if (ctx->writing) {
	    CancelIo(ctx->out.hEvent);
	    // waitfor it (ERROR_OPERATION_ABORTED)
	}
	CloseHandle(ctx->out.hEvent);
	ctx->out.hEvent = NULL;
    }
    if (ctx->stat.hEvent) {
	CloseHandle(ctx->stat.hEvent);
	ctx->stat.hEvent = NULL;
    }
}

// FIXME: use on linux subscribe to modem bits
// TIOCMIWAIT   wait for modem bits to change
// TCIOGICOUNT  count number of changes
//
#define SET_BIT(fs, x) fs |= (x)
#define CLR_BIT(fs, x) fs &= ~(x)

#define UPD_BIT(fs, x, on) do {		\
	if ((on)) SET_BIT(fs,x);	\
	else CLR_BIT(fs,(x));		\
    } while(0)


static int get_com_state(HANDLE fh, uart_com_state_t* com)
{
    DCB          dcb;
    COMMTIMEOUTS tmo;
    COMMPROP     prop;

    memset(&dcb, 0, sizeof(dcb));

    if (!GetCommState(fh, &dcb)) {
	DEBUG_ERROR("GetCommState: error %d", GetLastError());
	return -1;
    }
    // input baud reate
    com->ibaud = from_speed(dcb.BaudRate);
    com->obaud = com->ibaud;

    // parity
    if (!dcb.fParity)
	com->parity = UART_PARITY_NONE;
    else {
	switch (dcb.Parity) {
	case ODDPARITY:   com->parity = UART_PARITY_ODD; break;
	case EVENPARITY:  com->parity = UART_PARITY_EVEN; break;
	case MARKPARITY:  com->parity = UART_PARITY_MARK; break;
	case SPACEPARITY: com->parity = UART_PARITY_SPACE; break;
	case NOPARITY: com->parity = UART_PARITY_NONE; break;
	default: com->parity = UART_PARITY_NONE; break;
	}
    }
    // stop bits
    if (dcb.StopBits == ONESTOPBIT)
	com->stopb = 1;
    else if (dcb.StopBits == TWOSTOPBITS)
	com->stopb = 2;
    else if (dcb.StopBits == ONE5STOPBITS)
	com->stopb = 3; // error?
    else
	com->stopb = 0;

    com->csize = dcb.ByteSize;

    com->xonchar  = dcb.XonChar;
    com->xoffchar = dcb.XoffChar;

    com->iflow = 0;
    if (dcb.fInX) com->iflow |= UART_SW;
    if (dcb.fDtrControl == DTR_CONTROL_HANDSHAKE)
	com->iflow |= UART_DTR;
    if (dcb.fRtsControl == RTS_CONTROL_HANDSHAKE)
	com->iflow |= UART_RTS;

    com->oflow = 0;
    if (dcb.fOutX) com->oflow |= UART_SW;
    if (dcb.fOutxCtsFlow) com->oflow |= UART_CTS;
    if (dcb.fOutxDsrFlow) com->oflow |= UART_DSR;

    if (!GetCommTimeouts(fh, &tmo)) {
	DEBUG_ERROR("GetCommTimeouts: error %d", GetLastError());
	return -1;
    }

    if (!GetCommProperties(fh, &prop)) {
	DEBUG_ERROR("GetCommProperties: error %d", GetLastError());
	return -1;
    }	
    // Hardware buffer arguments (MODERNIZE this!!!)
    com->bufsz    = prop.dwCurrentRxQueue;
    // com->rxqueue  = prop.dwCurrentTxQueue; ????
    // com->txqueue  = prop.dwCurrentTxQueue;
    com->buftm    = tmo.ReadIntervalTimeout;

    return 0;
}

static int set_com_state(HANDLE fh, uart_com_state_t* com)
{
    DCB          dcb;
    COMMTIMEOUTS tmo;
    int baud;

    memset(&dcb, 0, sizeof(dcb));
    if (!GetCommState(fh, &dcb)) {
	DEBUG_ERROR("GetCommState: error %d", GetLastError());
	return -1;
    }

    baud = (com->ibaud > com->obaud) ? com->ibaud : com->obaud;
    dcb.BaudRate = to_speed(baud);

    switch(com->parity) {
    case UART_PARITY_NONE:
	dcb.fParity = FALSE;
	dcb.Parity = NOPARITY; 
	break;
    case UART_PARITY_ODD: 
	dcb.fParity = TRUE;
	dcb.Parity = ODDPARITY; 
	break;
    case UART_PARITY_EVEN: 
	dcb.fParity = TRUE;
	dcb.Parity = EVENPARITY; 
	break;
    case UART_PARITY_MARK:
	dcb.fParity = TRUE;
	dcb.Parity = MARKPARITY; 
	break;
    case UART_PARITY_SPACE:
	dcb.fParity = TRUE;
	dcb.Parity = SPACEPARITY; 
	break;
    default:
	dcb.fParity = FALSE;
	dcb.Parity = NOPARITY; 
	break;	
    }

    if (com->stopb == 1)
	dcb.StopBits = ONESTOPBIT;
    else if (com->stopb == 2)
	dcb.StopBits = TWOSTOPBITS;
    else if (com->stopb == 3)
	dcb.StopBits = ONE5STOPBITS;

    dcb.ByteSize = com->csize;

    dcb.XonChar          = com->xonchar;
    dcb.XoffChar         = com->xoffchar;

    // FIXME better flow handling!
    // {flow,[xoff|dtr|rts],[xoff|cts|dsr]} ?
    //
    dcb.fInX  = (com->iflow & UART_SW) != 0;
    dcb.fDtrControl = (com->iflow & UART_DTR) != 0;
    dcb.fRtsControl = (com->iflow & UART_RTS) != 0;

    dcb.fOutX = (com->oflow & UART_SW) != 0;
    dcb.fOutxCtsFlow = (com->oflow & UART_CTS) != 0;
    dcb.fOutxDsrFlow = (com->oflow & UART_DSR) != 0;

    dcb.fNull            = FALSE;
    
    dcb.DCBlength = sizeof(DCB);
    if (!SetCommState(fh, &dcb)) {
	DEBUG_ERROR("SetCommState: error %d", GetLastError());
	return -1;
    }

    // Setup harware buffer and timers
    if (com->bufsz > 255)
	SetupComm(fh, 255, DEFAULT_OUT_QUEUE);
    else
	SetupComm(fh, com->bufsz, DEFAULT_OUT_QUEUE);

    tmo.ReadIntervalTimeout = com->buftm;
    tmo.ReadTotalTimeoutMultiplier  =    0;
    tmo.ReadTotalTimeoutConstant    =    0;
    tmo.WriteTotalTimeoutMultiplier =    0;
    tmo.WriteTotalTimeoutConstant   =    0;
    if (!SetCommTimeouts(fh, &tmo)) {
	DEBUG_ERROR("GetCommTimeouts: error %d", GetLastError());
	return -1;
    }
    return 0;
}

static int get_modem_state(HANDLE fh, uart_modem_state_t* state)
{
    DCB dcb;
    DWORD status;
    int s = 0;

    if (!GetCommState(fh, &dcb)) {
	DEBUG_ERROR("GetCommState: error %d", GetLastError());
	return -1;
    }
    if (dcb.fDtrControl == DTR_CONTROL_ENABLE)	s |= UART_DTR;
    if (dcb.fRtsControl == RTS_CONTROL_ENABLE)	s |= UART_RTS;
    if (!GetCommModemStatus(fh, &status)) {
	DEBUG_ERROR("GetModemStatus: error %d", GetLastError());
	return -1;
    }
    if (status & MS_DSR_ON)  s |= UART_DSR;
    if (status & MS_CTS_ON)  s |= UART_CTS;
    if (status & MS_RING_ON) s |= UART_RI;
    if (status & MS_RLSD_ON) s |= UART_CD;
    *state = s;
    return 0;
}

static int set_modem_state(HANDLE fh, uart_modem_state_t state, int on)
{
    if (state & UART_DTR) {
	if (!EscapeCommFunction(fh, on ? SETDTR : CLRDTR)) {
	    DEBUG_ERROR("EscapeCommFunction: error %d", GetLastError());
	    return -1;
	}
    }
    if (state & UART_RTS) {
	if (!EscapeCommFunction(fh, on ? SETRTS : CLRRTS)) {
	    DEBUG_ERROR("EscapeCommFunction: error %d", GetLastError());
	    return -1;
	}
    }
    // Other bits ? RING?
    return 0;
}

static int uart_final(uart_ctx_t* ctx)
{
    uart_buf_finish(&ctx->ib);
    return 0;
}

void uart_init(uart_ctx_t* ctx, dthread_t* self, dthread_t* other) 
{
    memset(ctx, 0, sizeof(uart_ctx_t));
    ctx->fh = INVALID_HANDLE_VALUE;
    ctx->option.bsize = UART_DEF_BUFFER; 
    ctx->option.high = UART_HIGH_WATERMARK;
    ctx->option.low  = UART_LOW_WATERMARK;
    ctx->option.send_timeout = UART_INFINITY;
    ctx->option.send_timeout_close = 0;

    ctx->option.hsz = 0;                     // list header size
    ctx->option.htype = UART_PB_RAW;         // default packet type
    ctx->option.psize = 0;                   // no size check
    ctx->option.mode    = UART_MODE_LIST;    // list mode
    ctx->option.deliver = UART_DELIVER_TERM; // standard term format
    ctx->option.active  = UART_PASSIVE;      // start passive
    ctx->option.exitf   = 0;

    uart_buf_init(&ctx->ib);
    uart_queue_init(&ctx->oq);

    com_state_init(&ctx->state);

    ctx->port = self->port;
    ctx->dport = self->dport;

    ctx->self = self;
    ctx->other = other;
}

static int apply_opts(uart_ctx_t* ctx,
		      uart_com_state_t* state, uart_opt_t* option,
		      int32_t sflags)
{
    int old_active;
    unsigned int old_htype;

    if ((sflags & (1 << UART_OPT_DEVICE)) &&
	(strcmp(option->device_name, ctx->option.device_name) != 0)) {
	
	close_device(ctx);
	sflags  = 0;
	if (open_device(ctx, option->device_name) < 0)
	    return -1;
#ifdef DEBUG
	// com_state_dump(stderr, state);
#endif
	if (set_com_state(ctx->fh, state) < 0) {
	    DEBUGF("set_opts: uart_set_com_state failed");
	    return -1;
	}
	if (get_com_state(ctx->fh, state) >= 0) {
	    DEBUGF("set_opts: com_state: after");
#ifdef DEBUG
	    // com_state_dump(stderr, state);
#endif
	}
	else {
	    DEBUGF("apply_opts: get_com_state failed");
	}
    }
    else if (sflags & UART_OPT_COMM) {
	if (ctx->fh != INVALID_HANDLE_VALUE) {
	    // DEBUGF("set_opts: com_state before:");
	    // com_state_dump(stderr, state);
	    if (set_com_state(ctx->fh, state) < 0)
		return -1;
	    sflags = 0;
	    if (get_com_state(ctx->fh, state) >= 0) {
		// DEBUGF("set_opts: com_state: after");
#ifdef DEBUG
		// com_state_dump(stderr, state);
#endif
	    }
	}
    }

    old_active = ctx->option.active;
    old_htype = ctx->option.htype;
	    
    ctx->sflags = sflags;
    ctx->state  = *state;
    ctx->option = *option;

    if (ctx->fh != INVALID_HANDLE_VALUE) {
	if (ctx->option.active) {
	    if (!old_active || (ctx->option.htype != old_htype))
		return 1;
	    ctx->remain = 0; // CHECK ME
	    return 0;
	}
    }
    return 0;
}

/*
** Deliver packet ready 
** if len == 0 then check start with a check for ready packet
*/
int uart_deliver(uart_ctx_t* ctx, int len)
{
    int count = 0;
    int n;

    DEBUGF("uart_deliver(%ld): s=%ld about to deliver %d bytes...",
	   (long)ctx->port, (long)ctx->fh, len);

    /* Poll for ready packet */
    if (len == 0) {
	/* empty buffer or waiting for more input */
	if ((ctx->ib.base == NULL) || (ctx->remain > 0))
	    return count;
	n = uart_buf_remain(&ctx->ib, &len, ctx->option.htype,
			    ctx->option.psize);
	if (n != 0) {
	    if (n < 0) /* packet error */
		return n;
	    if (len > 0)  /* more data pending */
		ctx->remain = len;
	    return count;
	}
    }

    if (len > 0) {
	int code;

	code = uart_reply_data(ctx, (char*) ctx->ib.ptr_start, len);
	clear_timeout(ctx);

	/* XXX The buffer gets thrown away on error  (code < 0)    */
	/* Windows needs workaround for this in uart_uart_event...  */
	if (code < 0)
	    return code;
	ctx->ib.ptr_start += len;
	if (ctx->ib.ptr_start == ctx->ib.ptr)
	    uart_buf_reset(&ctx->ib);
	else
	    ctx->remain = 0;
    }

    count++;
    len = 0;

    if (!ctx->option.active) {
	if (ctx->ib.base != NULL)
	    uart_buf_restart(&ctx->ib);
    }
    else if (ctx->ib.base != NULL) {
	n = uart_buf_remain(&ctx->ib, &len,
			    ctx->option.htype, ctx->option.psize);
	if (n != 0) {
	    if (n < 0) /* packet error */
		return n;
	    uart_buf_restart(&ctx->ib);
	    if (len > 0)
		ctx->remain = len;
	    len = 0;
	}
    }
    return count;
}


// The modem has closed, cleanup and send event
int uart_recv_closed(uart_ctx_t* ctx)
{
    DEBUGF("uart_recv_closed(%ld)", (long) ctx->port);

    clear_timeout(ctx);
    uart_buf_reset(&ctx->ib);
    close_device(ctx);

    if (!ctx->option.active) {
	uart_async_error_am(ctx, ctx->dport, ctx->caller, am_closed);
    }
    else {
	uart_closed_message(ctx);
    }
    return -1;
}

//
// We have a read error determine the action
//
int uart_recv_error(uart_ctx_t* ctx, int err)
{
    if (err != EAGAIN) {
	clear_timeout(ctx);
	uart_buf_reset(&ctx->ib);
	close_device(ctx);
	
	if (!ctx->option.active) {
	    uart_async_error(ctx, ctx->dport, ctx->caller, err);
	}
	else {
	    uart_error_message(ctx, err); // first error
	    uart_closed_message(ctx);     /* then closed */
	}
	return -1;
    }
    return 0;
}

// process input data, buffer according to packet type
// return 0: no packet/data delivered
// return 1: packet delivered
// return -1: error
//
int process_input(uart_ctx_t* ctx, dthread_t* self, size_t request_len)
{
    (void) self;
    DWORD n;
    DWORD nread;

    if (ctx->ib.base == NULL) {  /* allocte a read buffer */
	size_t sz = ctx->option.bsize;
	if (request_len > 0)
	    sz = request_len;
	if (sz == 0)
	    sz = 1;
	if (uart_buf_alloc(&ctx->ib,sz) < 0)
	    return uart_recv_error(ctx, ENOMEM);
	ctx->remain = request_len;
	nread = sz;
    }
    else if (request_len > 0) { /* we have a data in buffer and a request */
	n = ctx->ib.ptr - ctx->ib.ptr_start;
	if (n >= request_len)
	    return uart_deliver(ctx, request_len);
	else if (uart_buf_expand(&ctx->ib, request_len) < 0)
	    return uart_recv_error(ctx, ENOMEM);
	else {
	    nread = request_len - n;
	    ctx->remain = nread;
	}
    }
    else if ((nread=ctx->remain) == 0) {  /* poll remain from buffer data */
	int len, i;
	i = uart_buf_remain(&ctx->ib, &len, 
			    ctx->option.htype,
			    ctx->option.psize);
	if (i < 0)
	    return uart_recv_error(ctx, EMSGSIZE);
	else if (i == 0)
	    return uart_deliver(ctx, len);
	else if (len > 0)
	    ctx->remain = len;
	nread = i;
    }
    
    DEBUGF("uart_recv(%ld): s=%ld about to read %d bytes...",
	   (long)ctx->port, (long)ctx->fh, nread);

    if (!ctx->reading) {
	if (!ReadFile(ctx->fh, ctx->rbuf, 1, &n, &ctx->in)) {
	    if (GetLastError() != ERROR_IO_PENDING)
		return uart_recv_error(ctx, uart_errno(ctx));
	    ctx->reading = 1;
	    DEBUGF(" => would block");
	    return 0;
	}
    }
    else {
	// check that arguments are the same as last call ?
	if (!GetOverlappedResult(ctx->fh, &ctx->in, &n, FALSE)) {
	    if (GetLastError() == ERROR_IO_INCOMPLETE) { // still waiting
		ctx->reading = 1;
		DEBUGF(" => would block");
		return 0;
	    }
	    ctx->reading = 0;
	    return uart_recv_error(ctx, uart_errno(ctx));
	}
	ctx->reading = 0;
    }

    if (n == 0) {
	DEBUGF("  => detected close");
	return uart_recv_closed(ctx);
    }
    if (n == 1)
	*ctx->ib.ptr = ctx->rbuf[0];

    DEBUGF(" => got %d bytes", n);
    ctx->ib.ptr += n;
    if (ctx->remain > 0) {
	ctx->remain -= n;
	if (ctx->remain == 0)
	    return uart_deliver(ctx, ctx->ib.ptr - ctx->ib.ptr_start);
    }
    else {
	int i, len;
	i = uart_buf_remain(&ctx->ib, &len, 
			    ctx->option.htype,
			    ctx->option.psize);
	if (i < 0)
	    return uart_recv_error(ctx, EMSGSIZE);
	else if (i == 0)
	    return uart_deliver(ctx, len);
	else if (len > 0)
	    ctx->remain = len;
    }
    return 0;
}


// process output queue
int process_output(uart_ctx_t* ctx, dthread_t* self)
{
    dmessage_t* mp;

    while ((mp = ctx->oq.mesg) != NULL) {
	DWORD nbytes = mp->used - ctx->oq.offs;
	DWORD nwritten;
	
	if (!ctx->writing) {
	    if (!WriteFile(ctx->fh, mp->buffer+ctx->oq.offs, 
			   nbytes, &nwritten, &ctx->out)) {
		if (GetLastError() != ERROR_IO_PENDING)
		    // return uart_send_error(ctx, uart_errno(ctx));
		    return -1;
	    }
	    ctx->writing = 1;
	    return 0;
	}
	else {
	    if (!GetOverlappedResult(ctx->fh, &ctx->out, &nwritten, FALSE)) {
		if (GetLastError() == ERROR_IO_INCOMPLETE) // still waiting
		    return 0;
                // return uart_send_error(ctx, uart_errno(ctx));
		ctx->writing = 0;
		return -1;
	    }
	    ctx->writing = 0;
	}

	if (nwritten < nbytes)
	    ctx->oq.offs += nwritten;
	else {
	    if (mp->from)
		dthread_port_send_ok(mp->source, self, mp->from, mp->ref);
	    dmessage_free(mp);
	    ctx->oq.offs = 0;
	    if ((mp = ctx->oq.front) == NULL)
		ctx->oq.mesg = NULL;
	    else {
		if (!(ctx->oq.front = mp->next))
		    ctx->oq.rear = NULL;
		ctx->oq.mesg = mp;
	    }
	}
    }
    return 0;
}


int enq_output(uart_ctx_t* ctx, dthread_t* self,
	       dmessage_t* mp, ErlDrvTermData from)
{
    dmessage_t* mr;

    mp->next = NULL;
    mp->from = from;  // from = 0 => async

    // set packet bytes header!
    set_packet_bytes(mp, ctx->option.htype);

    if ((ctx->oq.mesg == NULL) && (ctx->oq.front == NULL)) {
	ctx->oq.offs = 0;
	ctx->oq.mesg = mp;
	return process_output(ctx, self);
    }
    else {
	if ((mr = ctx->oq.rear) != NULL)
	    mr->next = mp;
	else
	    ctx->oq.front = mp;
	ctx->oq.rear = mp;
	return 0;
    }
}

    
// thread main!
int uart_win32_main(void* arg)
{
    dthread_t* self = (dthread_t*) arg;
    dthread_t* other = (dthread_t*) self->arg;
    dmessage_t* mp = NULL;
    dthread_poll_event_t ev[3];
    dthread_poll_event_t* evp;
    size_t nev;
    dterm_t term;
    uart_ctx_t ctx;
    ErlDrvTermData mp_from;
    ErlDrvTermData mp_ref;
    dthread_t*     mp_source;
    int tmo;
    int r;

    DEBUGF("uart_win32: thread started");

    uart_init(&ctx, self, other);

    dterm_init(&term);

again_tmo:
    tmo = next_timeout(&ctx);
again:
    nev = 0;

    if (ctx.writing) {
	ev[nev].event = (ErlDrvEvent) ctx.out.hEvent;
	ev[nev].events = ERL_DRV_READ; // yepp, even for write
	nev++;
    }

    while(!ctx.reading && (ctx.recv || (ctx.option.active != UART_PASSIVE)))
	process_input(&ctx, self, 0);

    if (ctx.reading) {
	ev[nev].event = (ErlDrvEvent) ctx.in.hEvent;
	ev[nev].events = ERL_DRV_READ;
	nev++;
    }

    evp = nev ? &ev[0] : NULL;

    DEBUGF("uart_win32_main: ctx.fh=%d, nev=%u, timeout = %d", 
	   ctx.fh, nev, tmo);
    r = dthread_poll(self, evp, &nev, tmo);

    if (r < 0) {
	DEBUGF("uart_win32_main: dthread_poll failed=%d", r);
	goto again_tmo;
    }
    else {
	DWORD i;
	DEBUGF("uart_win32_main: nev=%u, r=%d", nev, r);
	for (i = 0; i < nev; i++) {
	    if (ev[i].revents & ERL_DRV_READ) {
		if (ev[i].event == (ErlDrvEvent) ctx.in.hEvent) {
		    while((process_input(&ctx, self, 0) == 1) && 
			  (ctx.option.active != UART_PASSIVE))
			;
		}
		else if (ev[i].event == (ErlDrvEvent) ctx.out.hEvent) {
		    process_output(&ctx, self);
		}
	    }
	}
	tmo = next_timeout(&ctx);
	DEBUGF("uart_win32_main: timeout = %d", tmo);
	if (ctx.recv) {
	    if (tmo == 0) {
		uart_async_error_am(&ctx, ctx.dport, ctx.caller, am_timeout);
		clear_timeout(&ctx);
		ctx.remain = 0;
	    }
	}
	if (r == 0)
	    goto again;

	// r>0 (number of messages)
	DEBUGF("about to receive message r=%d", r);
	if ((mp = dthread_recv(self, NULL)) == NULL) {
	    DEBUGF("uart_win32_main: message was NULL");
	    goto again;
	}
	mp_from = mp->from;
	mp_ref  = mp->ref;
	mp_source = mp->source;

	switch (mp->cmd) {
	case DTHREAD_STOP:
	    DEBUGF("uart_win32_main: STOP");
	    close_device(&ctx);
	    uart_final(&ctx);
	    dmessage_free(mp);
	    DEBUGF("uart_win32_main: EXIT");
	    dthread_exit(0);
	    break;

	case DTHREAD_OUTPUT: // async send!
	    DEBUGF("uart_win32_main: OUTPUT");
	    if (ctx.fh == INVALID_HANDLE_VALUE) {
		dmessage_free(mp);
		goto again;
	    }
	    if (enq_output(&ctx, self, mp, 0) < 0) {
		mp = NULL;
		goto error;
	    }
	    goto again;

	case UART_CMD_CONNECT: {
	    ErlDrvTermData owner;
	    if (mp->used != 0) goto badarg;
	    owner = driver_connected(self->port);
	    self->owner   = owner;
	    other->owner  = owner;
	    goto ok;
	}

	case UART_CMD_CLOSE:
	    DEBUGF("uart_win32_main: CLOSE");
	    close_device(&ctx);
	    goto ok;

	case UART_CMD_SEND: // sync send
	    DEBUGF("uart_win32_main: SEND");
	    if (ctx.fh == INVALID_HANDLE_VALUE) goto ebadf;
	    if (enq_output(&ctx, self, mp, mp_from) < 0) {
		mp = NULL;
		goto error;
	    }
	    goto again;
	    
	case UART_CMD_SENDCHAR: // sync send
	    DEBUGF("uart_win32_main: SENDCHAR");
	    if (ctx.fh == INVALID_HANDLE_VALUE) goto ebadf;
	    if (enq_output(&ctx, self, mp, mp_from) < 0) {
		mp = NULL;
		goto error;
	    }
	    goto again;

	case UART_CMD_RECV: {  // <<Time:32, Length:32>> Time=0xffffffff=inf
	    uint32_t tm;
	    int len;
	    DEBUGF("uart_win32_main: RECV");
	    if (ctx.fh == INVALID_HANDLE_VALUE) goto ebadf;
	    if (ctx.recv) goto ealready;
	    if (mp->used != 8) goto badarg;
	    if (ctx.option.active != UART_PASSIVE) goto badarg;
	    tm = get_uint32((uint8_t*) mp->buffer);
	    len = (int) get_uint32((uint8_t*) (mp->buffer+4));
	    if ((len < 0) || (len > UART_MAX_PACKET_SIZE)) goto badarg;
	    ctx.ref = mp_ref;
	    ctx.caller = mp_from;
	    set_timeout(&ctx, tm);
	    ctx.recv = 1;
	    DEBUGF("recv timeout %lu", tm);
	    process_input(&ctx, self, len);
	    dmessage_free(mp);
	    goto again_tmo;
	}

	case UART_CMD_UNRECV: {  // argument is data to push back
	    uart_buf_push(&ctx.ib, mp->buffer, mp->used);
	    DEBUGF("unrecived %d bytes", ctx.ib.ptr - ctx.ib.ptr_start);
	    if (ctx.option.active != UART_PASSIVE) {
		while((process_input(&ctx, self, 0) == 1) && 
		      (ctx.option.active != UART_PASSIVE))
		    ;
	    }
	    goto ok;
	}

	case UART_CMD_SETOPTS: {
	    uart_com_state_t state  = ctx.state;
	    uart_opt_t       option = ctx.option;
	    uint32_t         sflags = ctx.sflags;

	    // parse & update options in state,option and sflag
	    if (uart_parse_opts(mp->buffer, mp->used, 
				&state, &option, &sflags) < 0)
		goto badarg;

	    //  apply the changed values
	    if ((r=apply_opts(&ctx, &state, &option, sflags)) < 0) {
		goto error;
	    }
	    goto ok;
	}

	case UART_CMD_GETOPTS: {
	    dterm_mark_t m1;
	    dterm_mark_t m2;
	    // {Ref, {ok,List}} || {Ref, {error,Reason}}
	    dterm_tuple_begin(&term, &m1); {
		dterm_uint(&term, mp_ref);
		dterm_tuple_begin(&term, &m2); {
		    dterm_atom(&term, am_ok);
		    if (uart_get_opts(&term, &ctx,(uint8_t*)mp->buffer,mp->used) < 0) {
			dterm_reset(&term);
			goto badarg;
		    }
		}
		dterm_tuple_end(&term, &m2);
	    }
	    dterm_tuple_end(&term, &m1);
	    dthread_port_send_dterm(mp_source, self, mp_from, &term);
	    dterm_reset(&term);
	    dmessage_free(mp);
	    goto again;
	}

	case UART_CMD_GET_MODEM: {
	    dterm_mark_t m1;
	    dterm_mark_t m2;
	    uart_modem_state_t mstate;
	    if (ctx.fh == INVALID_HANDLE_VALUE) goto ebadf;
	    if (get_modem_state(ctx.fh, &mstate) < 0) goto error;

	    dterm_tuple_begin(&term, &m1); {
		dterm_uint(&term, mp_ref);
		dterm_tuple_begin(&term, &m2); {
		    dterm_atom(&term, am_ok);
		    modem_state_dterm(&term, mstate);
		}
		dterm_tuple_end(&term, &m2);
	    }
	    dterm_tuple_end(&term, &m1);
	    dthread_port_send_dterm(mp_source, self, mp_from, &term);
	    dterm_reset(&term);
	    dmessage_free(mp);
	    goto again;
	}

	case UART_CMD_SET_MODEM: {
	    uart_modem_state_t mstate;	    
	    if (ctx.fh == INVALID_HANDLE_VALUE) goto ebadf;
	    if (mp->used != 4) goto badarg;
	    mstate = (uart_modem_state_t) get_uint32((uint8_t*) mp->buffer);
	    if (set_modem_state(ctx.fh, mstate, 1) < 0) goto error;
	    goto ok;
	}

	case UART_CMD_CLR_MODEM: {
	    uart_modem_state_t mstate;
	    if (ctx.fh == INVALID_HANDLE_VALUE) goto ebadf;
	    if (mp->used != 4) goto badarg;
	    mstate = (uart_modem_state_t) get_uint32((uint8_t*) mp->buffer);
	    if (set_modem_state(ctx.fh, mstate, 0) < 0) goto error;
	    goto ok;
	}
		
	case UART_CMD_HANGUP: {
	    if (ctx.fh == INVALID_HANDLE_VALUE) goto ebadf;
	    if (mp->used != 0) goto badarg;
	    // FIXME?
	    goto ok;
	}
		
	case UART_CMD_BREAK: {
	    int duration;
	    if (ctx.fh == INVALID_HANDLE_VALUE) goto ebadf;
	    if (mp->used != 4) goto badarg;
	    duration = (int) get_uint32((uint8_t*) mp->buffer);
	    if (!EscapeCommFunction(ctx.fh, SETBREAK)) {
		DEBUG_ERROR("EscapeCommFunction: error %d", GetLastError());
		goto error;
	    }
	    Sleep(duration);
	    if (!EscapeCommFunction(ctx.fh, CLRBREAK)) {
		DEBUG_ERROR("EscapeCommFunction: error %d", GetLastError());
		goto error;
	    }
	    goto ok;
	}

	case UART_CMD_FLOW:
	    if (ctx.fh == INVALID_HANDLE_VALUE) goto ebadf;
	    if (mp->used != 1) goto badarg;
	    switch(mp->buffer[0]) {
	    case 0:
		if (!EscapeCommFunction(ctx.fh, SETXOFF)) {
		    DEBUG_ERROR("EscapeCommFunction: error %d", GetLastError());
		    goto error;
		}
		break;
	    case 1:
		if (!EscapeCommFunction(ctx.fh, SETXON)) {
		    DEBUG_ERROR("EscapeCommFunction: error %d", GetLastError());
		    goto error;
		}
		break;
	    case 2:
		// TransmitCommChar(ctx.fh, XOFF);
		break;
	    case 3:
		// TransmitCommChar(ctx.fh, XON);
		break;
	    default: 
		goto badarg;
	    }
	    goto ok;

	default:
	    goto badarg;
	}
    }

ok:
    dthread_port_send_ok(mp_source, self,  mp_from, mp_ref);
    if (mp) dmessage_free(mp);
    goto again;

ebadf:
    errno = EBADF;
    goto error;
badarg:
    errno = EINVAL;
    goto error;
ealready:
    errno = EBUSY;
    goto error;

error:
    dthread_port_send_error(mp_source, self, mp_from, mp_ref,
			    uart_errno(&ctx));
    if (mp) dmessage_free(mp);
    goto again;
}
