//
//  UART "linux"/"unix"/"posix" implementation
//
// Nice ref about serial stuff is:
// http://www.easysw.com/~mike/serial/serial.html
// http://www.unixwiz.net/techtips/termios-vmin-vtime.html
//
//
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <sys/ioctl.h>

#include "uart_drv.h"

#if defined(__APPLE__)
#include <util.h>
#define HAVE_PTY
#else
#include <pty.h>
#define HAVE_PTY
#endif

static struct _rate {
    int baud;
    unsigned int speed;
} rate_tab[] = 
{
    {0,       B0     },
    {50,      B50    },
    {75,      B75    },
    {110,     B110   },
    {134,     B134   },
    {150,     B150   },
    {200,     B200   },
    {300,     B300   },
    {600,     B600   },
    {1200,    B1200  },
    {1800,    B1800  },
    {2400,    B2400  },
    {4800,    B4800  },
    {9600,    B9600  },
#ifdef B19200
    {19200,   B19200 },
#elif defined(EXTA)
    {19200,   EXTA },
#endif
#ifdef B38400
    {38400,   B38400 },
#elif defined(EXTB)
    {38400,   EXTB },
#endif
#ifdef B57600
    {57600,   B57600 },
#endif
#ifdef B76800
    {76800,   B76800 },
#endif
#ifdef B115200
    {115200,  B115200 },
#endif
#ifdef B153600
    {153600,  B153600 }, 	
#endif
#ifdef B230400
    {230400,  B230400 }, 	
#endif
#ifdef B307200
    {307200,  B307200 }, 	
#endif
#ifdef B460800
    {460800,  B460800 }, 	
#endif
#ifdef B500000
    {500000,  B500000 },
#endif
#ifdef B576000
    {576000,  B576000 },
#endif
#ifdef B921600 
    {921600,  B921600 },
#endif
#ifdef B1000000
    {1000000, B1000000 },
#endif
#ifdef B1152000
    {1152000, B1152000 },
#endif
#ifdef B1500000
    {1500000, B1500000 },
#endif
#ifdef B2000000
    {2000000, B2000000 },
#endif
#ifdef B2500000
    {2500000, B2500000 },
#endif
#ifdef B3000000
    {3000000, B3000000 },
#endif
#ifdef B3500000
    {3500000, B3500000 },
#endif
#ifdef B4000000
    {4000000, B4000000 },
#endif
    { -1, B0 }
};


#ifdef DARWIN
#define HAVE_C_ISPEED 1
#define HAVE_C_OSPEED 1
#endif

// save last error & return it
static int uart_errno(uart_ctx_t* ctx)
{
    int err = errno;
    ctx->error = err;
    return err;
}

static int from_speed(unsigned int speed)
{
#ifdef DIRECT_SPEED
    return (int) speed;
#else
    int i = 0;
    int baud;

    while((rate_tab[i].baud != -1) && (rate_tab[i].speed != speed))
	i++;
    baud = rate_tab[i].baud;
    return baud;
#endif
}

static unsigned int to_speed(int baud)
{
#ifdef DIRECT_SPEED
    return (unsigned int) baud;
#else
    int i = 0;
    int speed = 0;
    while((rate_tab[i].baud != -1) && (baud > rate_tab[i].baud))
	i++;
    if (rate_tab[i].baud == -1)
	speed = rate_tab[i-1].speed;
    else 
	speed = rate_tab[i].speed;
    return speed;
#endif
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

#if defined(__APPLE__)
static int local_ptsname_r(int fd, char* buf, size_t maxlen)
{
    char devname[128];
    struct stat sbuf;    
    size_t n;
    if (ioctl(fd, TIOCPTYGNAME, devname) < 0) {
	DEBUGF("TIOCPTYGNAME failed : %s", strerror(errno));
	return -1;
    }
    if (stat(devname, &sbuf) < 0) {
	DEBUGF("stat %s failed : %s", devname, strerror(errno));	
	return -1;
    }
    if ((n=strlen(devname)) >= maxlen) {
	errno = ERANGE;
	return -1;
    }
    memcpy(buf, devname, n);
    buf[n] = '\0';
    return 0;
}
#elif defined(__linux__)
#define local_ptsname_r ptsname_r
#elif defined(HAVE_PTY)
static int local_ptsname_r(int fd, char* buf, size_t maxlen)
{
    char* ptr;
    size_t n;

    // FIXME lock!
    if ((ptr = ptrname(fd)) == NULL) {
	DEBUGF("ptsname failed : %s", strerror(errno));
	return -1;
    }
    if ((n=strlen(ptr)) >= maxlen) {
	errno = ERANGE;
	return -1;
    }
    memcpy(buf, ptr, n);
    buf[n] = '\0';
    return 0;
}
#endif

// pseudo terminal devices to try
// On MacOS X: /dev/pty[p-w][0-9a-f]
// On *BSD: /dev/pty[p-sP-S][0-9a-v]
// On AIX: /dev/ptyp[0-9a-f]
// On HP-UX: /dev/pty[p-r][0-9a-f]
// On OSF/1: /dev/pty[p-q][0-9a-f]
// On Solaris: /dev/pty[p-r][0-9a-f]
#if defined(__APPLE__)
int local_openpt(int oflag)
{
//    (void) oflag;
    char devname[32];
    const char* a = "pqrstuvw";
    const char* b = "0123456789abcdef";
    char* prefix = "/dev/pty";
    int i,j,fd;

//    return getpt();
//    return open("/dev/ptmx", oflag);
//    return posix_openpt(oflag);

    for (i = 0; a[i]; i++) {
	for (j = 0; b[j]; j++) {
	    sprintf(devname, "%s%c%c", prefix,a[i],b[j]);
	    if ((fd = open(devname, oflag)) >= 0) {
		// fixme: check that the device is available
		return fd;
	    }
	}
    }
    errno = ENOENT;
    return -1;
}
#elif defined(__linux__)
int local_openpt(int oflag)
{
    return posix_openpt(oflag);
}
#elif defined(HAVE_PTY)
int local_openpt(int oflag)
{
    return posix_openpt(oflag);
}
#endif


static int open_device(uart_ctx_t* ctx, char* name, size_t max_namelen)
{
    int flags;
    int tty_fd = -1;
    int fd     = -1;

    if (strcmp(name, "//pty") == 0) {
#ifdef HAVE_PTY
	char slave_name[UART_MAX_DEVICE_NAME];
	
	if ((fd = local_openpt(O_RDWR|O_NOCTTY)) < 0) { 
	    DEBUGF("posix_openpt failed : %s", strerror(errno));
	    return -1;
	}
	if (grantpt(fd) < 0) {
	    DEBUGF("grantpt failed : %s", strerror(errno));
	    goto error;
	}
	if (unlockpt(fd) < 0) {
	    DEBUGF("unlockpt failed : %s", strerror(errno));
	    goto error;
	}
	if (local_ptsname_r(fd, slave_name, sizeof(slave_name)) < 0) {
	    DEBUGF("ptsname_r failed : %s", strerror(errno));
	    goto error;
	}
	if (strlen(slave_name) >= max_namelen) {
	    errno = ERANGE;
	    goto error;
	}
	strcpy(name, slave_name);
	if ((flags = fcntl(fd, F_GETFL, 0)) < 0) {
	    DEBUGF("fcntl: F_GETFL failed : %s", strerror(errno));
	    goto error;
	}
	if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0) {
	    DEBUGF("fcntl: F_SETFL failed : %s", strerror(errno));
	    goto error;
	}
#else
	errno = ENOTSUP;
	return -1;
#endif
    }
    if ((tty_fd = open(name, O_RDWR|O_NDELAY|O_NOCTTY)) < 0)
	goto error;
    // non-blocking!!!
    if ((flags = fcntl(tty_fd, F_GETFL, 0)) < 0) {
	DEBUGF("fcntl: F_GETFL tty_fd failed : %s", strerror(errno));
	goto error;
    }
    if (fcntl(tty_fd, F_SETFL, flags | O_NONBLOCK) < 0) {
	DEBUGF("fcntl: F_SETFL tty_fd failed : %s", strerror(errno));
	goto error;
    }
    tcflush(tty_fd, TCOFLUSH);
    tcflush(tty_fd, TCIFLUSH);
    ctx->tty_fd = tty_fd;
    ctx->fd = (fd>=0) ? fd : tty_fd;
    DEBUGF("open_device: tty_fd=%d fd=%d", ctx->tty_fd, ctx->fd);
    return tty_fd;
error:
    {
	int save_errno = errno;
	if (fd >= 0) close(fd);
	if ((fd != tty_fd) && (tty_fd >= 0)) close(tty_fd);
	errno = save_errno;	    
    }
    return -1;
}

static void close_device(uart_ctx_t* ctx)
{
    if (ctx->fd != ctx->tty_fd) {
	if (ctx->fd >= 0) {
	    DEBUGF("close_device: master=%d", ctx->fd);
	    close(ctx->fd);
	    ctx->fd = -1;
	}
    }
    if (ctx->tty_fd >= 0) {
	DEBUGF("close_device: fd=%d", ctx->tty_fd);
	close(ctx->tty_fd);
	ctx->tty_fd = -1;
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


static int get_com_state(int fd, uart_com_state_t* com)
{
    struct termios tio;
    
    if (tcgetattr(fd, &tio) < 0) 
	return -1;

    // input baud reate
    com->ibaud = from_speed(cfgetispeed(&tio));
    com->obaud = from_speed(cfgetospeed(&tio));

    // parity
    if (tio.c_cflag & PARENB) {
	if (tio.c_iflag & PARMRK)
	    com->parity = UART_PARITY_MARK;
	else if (tio.c_cflag & PARODD)
	    com->parity = UART_PARITY_ODD;
	else
	    com->parity = UART_PARITY_EVEN;
    }
    else
	com->parity = UART_PARITY_NONE;
    
    // stop bits
    if (tio.c_cflag & CSTOPB)
	com->stopb = 2;
    else
	com->stopb = 1;

    // csize
    switch(tio.c_cflag & CSIZE) {
    case CS5: com->csize = 5; break;
    case CS6: com->csize = 6; break;
    case CS7: com->csize = 7; break;
    case CS8: com->csize = 8; break;
    default: break;
    }
    
    // may be used for {packet, {size,N}} and also when
    // in {packet,N} (N!=0) when waiting for a certain amount of data
    com->bufsz    = tio.c_cc[VMIN];       // min number of bytes buffered
    com->buftm    = tio.c_cc[VTIME]*100;
    com->xonchar  = tio.c_cc[VSTART];
    com->xoffchar = tio.c_cc[VSTOP];

    com->iflow = 0;
    if (tio.c_iflag & IXOFF) com->iflow |= UART_SW;
#if defined(CRTS_IFLOW)
    if (tio.c_cflag & CRTS_IFLOW) com->iflow |= UART_RTS;
#endif
#if defined(CDTR_IFLOW)
    if (tio.c_cflag & CDTR_IFLOW) com->iflow |= UART_DTR;
#endif

    com->oflow = 0;
    if (tio.c_iflag & IXON) com->oflow |= UART_SW;
#if defined(CCTS_OFLOW)
    if (tio.c_cflag & CCTS_OFLOW) com->oflow |= UART_CTS;
#endif
#if defined(CDSR_OFLOW)
    if (tio.c_cflag & CDSR_OFLOW) com->oflow |= UART_DSR;
#endif
#if defined(CCAR_OFLOW)
    if (tio.c_cflag & CCAR_OFLOW) com->oflow |= UART_CD;
#endif
#if defined(CRTSCTS)
    if ((tio.c_cflag & CRTSCTS) == CRTSCTS) {
	com->oflow |= UART_CTS; //com->oflow |= UART_CD;
	com->iflow |= UART_RTS;
    }
#endif
    return 0;
}

static int set_com_state(int fd, uart_com_state_t* com)
{
    struct termios tio;

    // read current state
    if (tcgetattr(fd, &tio) < 0) {
	DEBUGF("unable to read com state: %s", strerror(errno));
	return -1;
    }

    // On Mac os X IOSSIOSPEED can be used to set "non-traditional baud rate"
    // From "IOKit/serial/ioss.h"

    cfsetispeed(&tio, to_speed(com->ibaud));
    cfsetospeed(&tio, to_speed(com->obaud));

    // update from state
    switch(com->parity) {
    case UART_PARITY_NONE:
	tio.c_iflag &= ~PARMRK;
	tio.c_cflag &= ~PARENB;
	break;
    case UART_PARITY_ODD: // odd 
	tio.c_iflag &= ~PARMRK;
	tio.c_cflag  |= PARODD;
	tio.c_cflag |= PARENB;
	break;
    case UART_PARITY_EVEN: // even
	tio.c_iflag &= ~PARMRK;
	tio.c_cflag &= ~PARODD;
	tio.c_cflag |= PARENB;
	break;
    case UART_PARITY_MARK:  // mark (FIXME)
	tio.c_iflag |= PARMRK;
	tio.c_cflag |= PARENB;
	break;
    case UART_PARITY_SPACE:  // space (FIXME) 
	tio.c_iflag &= ~PARMRK;
	tio.c_cflag &= ~PARENB;
	break;
    default:
	break;
    }

    if (com->stopb == 1)
	tio.c_cflag &= ~CSTOPB;
    else if (com->stopb == 2)
	tio.c_cflag |= CSTOPB;

    tio.c_cflag &= ~CSIZE;
    switch(com->csize) {
    case 5: tio.c_cflag |= CS5; break;
    case 6: tio.c_cflag |= CS6; break;
    case 7: tio.c_cflag |= CS7; break;
    case 8: tio.c_cflag |= CS8; break;
    default: break;
    }
    // Set the buffer number of bytes buffered before interrupt
    if (com->bufsz > 255)
	tio.c_cc[VMIN] = 255;
    else
	tio.c_cc[VMIN] = com->bufsz;
    // Set the max time to buffer bytes 
    if (com->buftm > 25500)  // 25500 ms = 25.5 sec
	tio.c_cc[VTIME] = 255;
    else
	tio.c_cc[VTIME] = com->buftm / 100;
    tio.c_cc[VSTART] = com->xonchar;
    tio.c_cc[VSTOP] = com->xoffchar;

    // input flow control
    UPD_BIT(tio.c_iflag, IXOFF, (com->iflow & UART_SW));
#if defined(CRTS_IFLOW)
    UPD_BIT(tio.c_cflag, CRTS_IFLOW, (com->iflow & UART_RTS));
#endif
#if defined(CDTR_IFLOW)
    UPD_BIT(tio.c_cflag, CDTR_IFLOW, (com->iflow & UART_DTR));
#endif

    // output flow control
    UPD_BIT(tio.c_iflag, IXON, (com->oflow & UART_SW));
#if defined(CCTS_OFLOW)
    UPD_BIT(tio.c_cflag, CCTS_OFLOW, (com->oflow & UART_CTS));
#endif
#if defined(CDSR_OFLOW)
    UPD_BIT(tio.c_cflag, CDSR_OFLOW, (com->oflow & UART_DSR));
#endif
#if defined(CCAR_OFLOW)
    UPD_BIT(tio.c_cflag, CCAR_OFLOW, (com->oflow & UART_CD));
#endif
#if defined(CRTSCTS)
    if ((com->iflow & UART_RTS) && (com->oflow & UART_CTS))
	tio.c_cflag |= CRTSCTS;
    else if (!(com->iflow & UART_RTS) && !(com->oflow & UART_CTS))
	tio.c_cflag &= ~CRTSCTS;
#endif
    // ignore break condition
    tio.c_iflag |= IGNBRK;

    // local line + enable receiver
    tio.c_cflag |= (CLOCAL | CREAD);

    // raw input processing
    tio.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG | IEXTEN);
    // no output processing
    tio.c_oflag &= ~(OPOST);
    // do NOT hangup-on-close, need? we keep one slave open
    tio.c_cflag &= ~HUPCL;   
    
    tcflush(fd, TCIFLUSH);
    return tcsetattr(fd, TCSANOW, &tio);
}

static int get_modem_state(int fd, uart_modem_state_t* state)
{
    int status, r, s;

    if ((r = ioctl(fd, TIOCMGET, &status)) < 0)
	return r;
    s = 0;
    if ((status & TIOCM_DTR) != 0) s |= UART_DTR;
    if ((status & TIOCM_RTS) != 0) s |= UART_RTS;
    if ((status & TIOCM_CTS) != 0) s |= UART_CTS;
    if ((status & TIOCM_CD)  != 0) s |= UART_CD;
    if ((status & TIOCM_RNG) != 0) s |= UART_RI;
    if ((status & TIOCM_DSR) != 0) s |= UART_DSR;
    *state = s;
    return 0;
}

static int set_modem_state(int fd, uart_modem_state_t state, int on)
{
    if (state) {
	int status = 0;
	if (state & UART_DTR) status |= TIOCM_DTR;  // out
	if (state & UART_RTS) status |= TIOCM_RTS;  // out
	if (state & UART_CTS) status |= TIOCM_CTS;  // in
	if (state & UART_CD)  status |= TIOCM_CD;   // in
	if (state & UART_RI)  status |= TIOCM_RI;   // in|out?
	if (state & UART_DSR) status |= TIOCM_DSR;  // in
	if (on) 
	    return ioctl(fd, TIOCMBIS, &status);
	else
	    return ioctl(fd, TIOCMBIC, &status);
    }
    return 0;
}

static int uart_final(uart_ctx_t* ctx)
{
    uart_buf_finish(&ctx->ib);
    // uart_queue_clear(&ctx->oq);
    return 0;
}

void uart_init(uart_ctx_t* ctx, dthread_t* self, dthread_t* other) 
{
    memset(ctx, 0, sizeof(uart_ctx_t));
    ctx->fd = -1;
    ctx->tty_fd = -1;
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
    ctx->option.eolchar = '\n';

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
	sflags &= ~(UART_OPT_COMM | (1 << UART_OPT_DEVICE));
	if (open_device(ctx,option->device_name,sizeof(option->device_name))<0)
	    return -1;
#ifdef DEBUG
	// com_state_dump(stderr, state);
#endif
	if (set_com_state(ctx->tty_fd, state) < 0) {
	    DEBUGF("set_opts: uart_set_com_state failed");
	    return -1;
	}
	if (get_com_state(ctx->tty_fd, state) >= 0) {
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
	if (ctx->tty_fd >= 0) {
	    // DEBUGF("set_opts: com_state before:");
	    // com_state_dump(stderr, state);
	    if (set_com_state(ctx->tty_fd, state) < 0)
		return -1;
	    sflags &= ~(UART_OPT_COMM);
	    if (get_com_state(ctx->tty_fd, state) >= 0) {
		// DEBUGF("set_opts: com_state: after");
#ifdef DEBUG
		// com_state_dump(stderr, state);
#endif
	    }
	}
    }

    if (sflags & (1 << UART_OPT_PTYPKT)) {
	if ((ctx->fd != ctx->tty_fd) && (ctx->fd >= 0)) {
	    DEBUGF("set TIOCPKT = %d\n", option->ptypkt);
	    if (ioctl(ctx->fd, TIOCPKT, &option->ptypkt) < 0) {
		DEBUGF("set_opts: ioctl TIOCPKT failed: %s", strerror(errno));
	    }
	}
	sflags &= ~(1 << UART_OPT_PTYPKT);
    }

    old_active = ctx->option.active;
    old_htype = ctx->option.htype;
	    
    ctx->sflags = sflags;
    ctx->state  = *state;
    ctx->option = *option;

    if (ctx->tty_fd >= 0) {
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
	   (long)ctx->port, (long)ctx->fd, len);

    /* Poll for ready packet */
    if (len == 0) {
	/* empty buffer or waiting for more input */
	if ((ctx->ib.base == NULL) || (ctx->remain > 0))
	    return count;
	n = uart_buf_remain(&ctx->ib, &len, 
			    ctx->option.htype,
			    ctx->option.psize,
			    ctx->option.eolchar);
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
			    ctx->option.htype, 
			    ctx->option.psize,
			    ctx->option.eolchar);
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
int process_input(uart_ctx_t* ctx, dthread_t* self, int request_len)
{
    (void) self;
    int n;
    int len;
    int nread;

    if (ctx->ib.base == NULL) {  /* allocte a read buffer */
	size_t sz = ctx->option.bsize;
	if (request_len > 0)
	    sz = request_len;
	if (sz == 0)
	    sz = 1;
	nread = uart_buf_alloc(&ctx->ib,sz);
	ctx->remain = request_len;
	if (nread < 0)
	    return uart_recv_error(ctx, ENOMEM);
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
	nread = uart_buf_remain(&ctx->ib, &len, 
				ctx->option.htype,
				ctx->option.psize,
				ctx->option.eolchar);
	if (nread < 0)
	    return uart_recv_error(ctx, EMSGSIZE);
	else if (nread == 0)
	    return uart_deliver(ctx, len);
	else if (len > 0)
	    ctx->remain = len;
    }
    
    DEBUGF("uart_recv(%ld): s=%ld about to read %d bytes...",
	   (long)ctx->port, (long)ctx->fd, nread);

    n = read(ctx->fd, ctx->ib.ptr, nread);

    if (n < 0) {
	int err = uart_errno(ctx);
	if (err == EAGAIN) {
	    DEBUGF(" => would block");
	    return 0;
	}
	else {
	    DEBUGF(" => error: %d", err);
	    return uart_recv_error(ctx, err);
	}
    }
    else if (n == 0) {
	DEBUGF("  => detected zero bytes %s", strerror(errno));
	return uart_recv_closed(ctx);
    }

    DEBUGF(" => got %d bytes", n);
    ctx->ib.ptr += n;
    if (ctx->remain > 0) {
	ctx->remain -= n;
	if (ctx->remain == 0)
	    return uart_deliver(ctx, ctx->ib.ptr - ctx->ib.ptr_start);
    }
    else {
	nread = uart_buf_remain(&ctx->ib, &len, 
				ctx->option.htype,
				ctx->option.psize,
				ctx->option.eolchar);
	if (nread < 0)
	    return uart_recv_error(ctx, EMSGSIZE);
	else if (nread == 0)
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
    int r = 0;

    while ((mp = ctx->oq.mesg) != NULL) {
	int n = mp->used - ctx->oq.offs;

	if ((r = write(ctx->fd, mp->buffer+ctx->oq.offs, n)) < 0) {
	    if ((r < 0) && (errno == EAGAIN)) {
		DEBUGF("uart_unix: process_output: EAGAIN");
		return 0;
	    }
	    return -1;
	}
	else if (r < n) {
	    ctx->oq.offs += r;
	    return 0;
	}
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
    return r;
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
int uart_unix_main(void* arg)
{
    dthread_t* self = (dthread_t*) arg;
    dthread_t* other = (dthread_t*) self->arg;
    dmessage_t* mp = NULL;
    dthread_poll_event_t ev, *evp;
    size_t nev;
    dterm_t term;
    uart_ctx_t ctx;
    ErlDrvTermData mp_from;
    ErlDrvTermData mp_ref;
    dthread_t*     mp_source;
    int tmo;
    int r;

    DEBUGF("uart_unix: thread started");

    uart_init(&ctx, self, other);

    dterm_init(&term);

again_tmo:
    tmo = next_timeout(&ctx);
again:
    nev = 0;
    evp = NULL;
    if (ctx.fd >= 0) {
	ev.event = (ErlDrvEvent) ((long)ctx.fd);
	ev.events = 0;
	if ((ctx.option.active != UART_PASSIVE) || ctx.recv) {
	    ev.events |= ERL_DRV_READ;
	    if (ctx.option.ptypkt && (ctx.fd != ctx.tty_fd))
		ev.events |= ERL_DRV_EXCEP;
	}
	if (ctx.oq.mesg)
	    ev.events |= ERL_DRV_WRITE;
	if (ev.events) {
	    evp = &ev;
	    nev = 1;
	}
	DEBUGF("ctx.fd=%d, ev.events=%d", ctx.fd, ev.events);
    }

    DEBUGF("uart_unix_main: nev=%d, events=%x, timeout = %d", 
	   nev, ev.events, tmo);
    r = dthread_poll(self, evp, &nev, tmo);

    if (r < 0) {
	DEBUGF("uart_unix_main: dthread_poll failed=%d", r);
	goto again_tmo;
    }
    else {
	DEBUGF("uart_unix_main: nev=%d, r=%d", nev, r);

	if (evp && (nev == 1)) {
	    if (evp->revents & ERL_DRV_WRITE)
		process_output(&ctx, self);
	    if (evp->revents & (ERL_DRV_READ|ERL_DRV_EXCEP)) {
		while((process_input(&ctx, self, 0) == 1) && 
		      (ctx.option.active != UART_PASSIVE))
		    ;
	    }
	}
	tmo = next_timeout(&ctx);
	DEBUGF("uart_unix_main: timeout = %d", tmo);
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
	    DEBUGF("uart_unix_main: message was NULL");
	    goto again;
	}
	mp_from = mp->from;
	mp_ref  = mp->ref;
	mp_source = mp->source;

	switch (mp->cmd) {
	case DTHREAD_STOP:
	    DEBUGF("uart_unix_main: STOP");
	    close_device(&ctx);
	    uart_final(&ctx);
	    dmessage_free(mp);
	    DEBUGF("uart_unix_main: EXIT");
	    dthread_exit(0);
	    break;

	case DTHREAD_OUTPUT: // async send!
	    DEBUGF("uart_unix_main: OUTPUT");
	    if (ctx.fd < 0) {
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
	    DEBUGF("uart_unix_main: CLOSE");
	    close_device(&ctx);
	    goto ok;

	case UART_CMD_SEND: // sync send
	    DEBUGF("uart_unix_main: SEND");
	    if (ctx.fd < 0) goto ebadf;
	    if (enq_output(&ctx, self, mp, mp_from) < 0) {
		mp = NULL;
		goto error;
	    }
	    goto again;
	    
	case UART_CMD_SENDCHAR: // sync send
	    DEBUGF("uart_unix_main: SENDCHAR");
	    if (ctx.fd < 0) goto ebadf;
	    if (enq_output(&ctx, self, mp, mp_from) < 0) {
		mp = NULL;
		goto error;
	    }
	    goto again;

	case UART_CMD_RECV: {  // <<Time:32, Length:32>> Time=0xffffffff=inf
	    uint32_t tm;
	    int len;
	    DEBUGF("uart_unix_main: RECV");
	    if (ctx.fd < 0) goto ebadf;
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
	    if ((r=apply_opts(&ctx, &state, &option, sflags)) < 0)
		goto error;

	    if (r == 1) {
		while((process_input(&ctx, self, 0) == 1) && 
		      (ctx.option.active != UART_PASSIVE))
		    ;
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
	    if (ctx.tty_fd < 0) goto ebadf;
	    if (get_modem_state(ctx.tty_fd, &mstate) < 0) goto error;

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
	    if (ctx.tty_fd < 0) goto ebadf;
	    if (mp->used != 4) goto badarg;
	    mstate = (uart_modem_state_t) get_uint32((uint8_t*) mp->buffer);
	    if (set_modem_state(ctx.tty_fd, mstate, 1) < 0) goto error;
	    goto ok;
	}

	case UART_CMD_CLR_MODEM: {
	    uart_modem_state_t mstate;
	    if (ctx.tty_fd < 0) goto ebadf;
	    if (mp->used != 4) goto badarg;
	    mstate = (uart_modem_state_t) get_uint32((uint8_t*) mp->buffer);
	    if (set_modem_state(ctx.tty_fd, mstate, 0) < 0) goto error;
	    goto ok;
	}
		
	case UART_CMD_HANGUP: {
	    struct termios tio;
	    int r;
	    if (ctx.tty_fd < 0) goto ebadf;
	    if (mp->used != 0) goto badarg;
	    if ((r = tcgetattr(ctx.tty_fd, &tio)) < 0) {
		INFOF("tcgetattr: error=%s\n", strerror(errno));
		goto badarg;
	    }
	    cfsetispeed(&tio, B0);
	    cfsetospeed(&tio, B0);
	    if ((r = tcsetattr(ctx.tty_fd, TCSANOW, &tio)) < 0) {
		INFOF("tcsetattr: error=%s\n", strerror(errno));
		goto badarg;		
	    }
	    goto ok;
	}
		
	case UART_CMD_BREAK: {
	    int duration;
	    if (ctx.tty_fd < 0) goto ebadf;
	    if (mp->used != 4) goto badarg;
	    duration = (int) get_uint32((uint8_t*) mp->buffer);
	    if (tcsendbreak(ctx.tty_fd, duration) < 0)
		goto error;
	    goto ok;
	}
	    
	case UART_CMD_FLOW:
	    if (ctx.tty_fd < 0) goto ebadf;
	    if (mp->used != 1) goto badarg;
	    switch(mp->buffer[0]) {
	    case 0: r = tcflow(ctx.tty_fd, TCIOFF); break;
	    case 1: r = tcflow(ctx.tty_fd, TCION); break;
	    case 2: r = tcflow(ctx.tty_fd, TCOOFF); break;
	    case 3: r = tcflow(ctx.tty_fd, TCOON); break;
	    default: goto badarg; break;
	    }
	    if (r < 0)
		goto error;
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
    errno = EALREADY;
    goto error;

error:
    dthread_port_send_error(mp_source, self, mp_from, mp_ref,
			    uart_errno(&ctx));
    if (mp) dmessage_free(mp);
    goto again;
}
