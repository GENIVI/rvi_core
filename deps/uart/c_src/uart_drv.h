//
// Uart interface definitions
//
#ifndef __UART_DRV__
#define __UART_DRV__

#include <stdio.h>
#include <stdint.h>

#include "erl_driver.h"
#include "dthread/include/dthread.h"
#include "dthread/include/dlog.h"

static inline uint32_t get_uint32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return value;
}

static inline uint16_t get_uint16(uint8_t* ptr)
{
    uint16_t value = (ptr[0]<<8) | (ptr[1]<<0);
    return value;
}

static inline uint8_t get_uint8(uint8_t* ptr)
{
    uint8_t value = (ptr[0]<<0);
    return value;
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

#define UART_CMD_OPEN       1
#define UART_CMD_HANGUP     2
#define UART_CMD_CLOSE      4
#define UART_CMD_FLOW       5
#define UART_CMD_BREAK      7
#define UART_CMD_SETOPTS    8
#define UART_CMD_GETOPTS    9
#define UART_CMD_SENDCHAR   10
#define UART_CMD_SEND       11
#define UART_CMD_GET_MODEM  12
#define UART_CMD_SET_MODEM  13
#define UART_CMD_CLR_MODEM  14
#define UART_CMD_UNRECV     15
#define UART_CMD_RECV       16
#define UART_CMD_CONNECT    17

#define UART_MAX_OPT_BUFFER (64*1024)
#define UART_DEF_BUFFER     1024
#define UART_MIN_BUFFER     1

#define UART_HIGH_WATERMARK (1024*2)    // 2k pending high => busy
#define UART_LOW_WATERMARK  (1024*1)    // 1k pending => allow more
#define UART_INFINITY       0xffffffff  // infinity value


#define UART_OPT_COMM \
    ((1 << UART_OPT_IBAUD) | (1 << UART_OPT_OBAUD) | \
     (1 << UART_OPT_CSIZE) | (1 << UART_OPT_BUFSZ) |		\
     (1 << UART_OPT_BUFTM) | (1 << UART_OPT_STOPB) |		\
     (1 << UART_OPT_PARITY) | (1 << UART_OPT_IFLOW) |		\
     (1 << UART_OPT_OFLOW) | (1 << UART_OPT_XOFFCHAR) |	\
     (1 << UART_OPT_XONCHAR) | (1 << UART_OPT_EOLCHAR))


#define UART_PB_LITTLE_ENDIAN 0x00008000  // UART_PB_<n> 
#define UART_PB_BYTES_MASK    0x00000F00  // UART_PB_<n> 0..8 allowed
#define UART_PB_FIXED_MASK    0xFFFF0000  // UART_PB_RAW
#define UART_PB_TYPE_MASK     0x000000FF  // UART_PB_x

#define UART_PB_RAW           0
#define UART_PB_N             1
#define UART_PB_LINE_LF       2
#define UART_PB_BASIC_0710    3
#define UART_PB_ADVANCED_0710 4
#define UART_PB_GSM_0710      5

#define UART_PASSIVE  0
#define UART_ACTIVE   1
#define UART_ONCE     2

#define UART_PARITY_NONE  0
#define UART_PARITY_ODD   1
#define UART_PARITY_EVEN  2
#define UART_PARITY_MARK  3
#define UART_PARITY_SPACE 4

#define UART_DELIVER_PORT  0
#define UART_DELIVER_TERM  1

#define UART_MODE_LIST    0
#define UART_MODE_BINARY  1

#define UART_OK       0
#define UART_ERROR    1
#define UART_OPTIONS  2

#define UART_F_OPEN               0x0001
#define UART_F_BUSY               0x0080
#define UART_F_CLOSE_SENT         0x0002
#define UART_F_DELAYED_CLOSE_RECV 0x0004
#define UART_F_DELAYED_CLOSE_SEND 0x0008


#define UART_OPT_DEVICE     1
#define UART_OPT_IBAUD      2
#define UART_OPT_OBAUD      3
#define UART_OPT_CSIZE      4
#define UART_OPT_BUFSZ      5
#define UART_OPT_BUFTM      6
#define UART_OPT_STOPB      7
#define UART_OPT_PARITY     8
#define UART_OPT_IFLOW      9
#define UART_OPT_OFLOW      10
#define UART_OPT_XOFFCHAR   11
#define UART_OPT_XONCHAR    12
#define UART_OPT_EOLCHAR    13
// #define UART_OPT_14   14
#define UART_OPT_ACTIVE     15
#define UART_OPT_DELAY_SEND 16
#define UART_OPT_DELIVER    17
#define UART_OPT_MODE       18
// #define UART_OPT_19   19
#define UART_OPT_HEADER     20
#define UART_OPT_PACKET     21
#define UART_OPT_PSIZE      22
#define UART_OPT_HIGH       23
#define UART_OPT_LOW        24
#define UART_OPT_SENDTMO    25  // send timeout
#define UART_OPT_CLOSETMO   26  // send close timeout
#define UART_OPT_BUFFER     27
#define UART_OPT_DEBUG      28
#define UART_OPT_EXITF      29
#define UART_OPT_PTYPKT     30
#define UART_OPT_MAX        31


typedef struct {
    int ibaud;       // input baud rate (9600)
    int obaud;       // output baud rate (9600)
    int parity;      // parity (0)
    int stopb;       // stop bits (1)
    int csize;       // characters size  (8)
    int bufsz;       // 1  size of UART controller buffer (max 255)
    int buftm;       // 1  milliseconds inter character timeout
    int xonchar;     // 0
    int xoffchar;    // 0
    int iflow;       // 0 input flow control
    int oflow;       // 0 output flow control
} uart_com_state_t;


#define UART_DTR  0x0002  // Data Terminal Ready
#define UART_RTS  0x0004  // Ready To Send
#define UART_CTS  0x0008  // Clear To Send
#define UART_CD   0x0010  // Carrier Detect
#define UART_RI   0x0020  // Ring Indicator
#define UART_DSR  0x0040  // Data Set Ready
#define UART_SW   0x8000  // Software, only for iflow/oflow 

typedef uint16_t uart_modem_state_t;

extern ErlDrvTermData am_dtr;
extern ErlDrvTermData am_rts;
extern ErlDrvTermData am_cts;
extern ErlDrvTermData am_cd;
extern ErlDrvTermData am_ri;
extern ErlDrvTermData am_dsr;
extern ErlDrvTermData am_sw;

extern ErlDrvTermData am_device;
extern ErlDrvTermData am_baud;
extern ErlDrvTermData am_ibaud;
extern ErlDrvTermData am_obaud;
extern ErlDrvTermData am_csize;
extern ErlDrvTermData am_bufsz;
extern ErlDrvTermData am_buftm;
extern ErlDrvTermData am_stopb;
extern ErlDrvTermData am_parity;
extern ErlDrvTermData am_iflow;
extern ErlDrvTermData am_oflow;
extern ErlDrvTermData am_xoffchar;
extern ErlDrvTermData am_xonchar;
extern ErlDrvTermData am_eolchar;
extern ErlDrvTermData am_active;
extern ErlDrvTermData am_delay_send;
extern ErlDrvTermData am_deliver;
extern ErlDrvTermData am_mode;
extern ErlDrvTermData am_header;
extern ErlDrvTermData am_packet;
extern ErlDrvTermData am_packet_size;
extern ErlDrvTermData am_high_watermark;
extern ErlDrvTermData am_low_watermark;
extern ErlDrvTermData am_send_timeout;
extern ErlDrvTermData am_send_timeout_close;
extern ErlDrvTermData am_buffer;
extern ErlDrvTermData am_exit_on_close;
extern ErlDrvTermData am_debug;
extern ErlDrvTermData am_ptypkt;

extern ErlDrvTermData am_none;
extern ErlDrvTermData am_odd;
extern ErlDrvTermData am_even;
extern ErlDrvTermData am_mark;
extern ErlDrvTermData am_space;

extern ErlDrvTermData am_true;
extern ErlDrvTermData am_false;
extern ErlDrvTermData am_once;

extern ErlDrvTermData am_port;
extern ErlDrvTermData am_term;

extern ErlDrvTermData am_list;
extern ErlDrvTermData am_binary;

extern ErlDrvTermData am_size;
extern ErlDrvTermData am_line;
extern ErlDrvTermData am_basic_0710;
extern ErlDrvTermData am_advanced_0710;
extern ErlDrvTermData am_gsm_0710;

extern ErlDrvTermData am_ok;
extern ErlDrvTermData am_uart;
extern ErlDrvTermData am_error;
extern ErlDrvTermData am_uart_async;
extern ErlDrvTermData am_uart_reply;
extern ErlDrvTermData am_timeout;
extern ErlDrvTermData am_closed;
extern ErlDrvTermData am_uart_closed;
extern ErlDrvTermData am_uart_error;
extern ErlDrvTermData am_empty_out_q;


#define ATOM(NAME) am_ ## NAME
#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)

#define UART_MAX_DEVICE_NAME 256

#define UART_MAX_PACKET_SIZE 0x01000000

typedef struct {
    char         device_name[UART_MAX_DEVICE_NAME];
    int          high;               // high watermark
    int          low;                // low watermark
    int          send_timeout;       // timeout to use in send 
    int          send_timeout_close; // auto-close fd on send_timeout
    int          active;             // PASSIVE|ACTIVE|ONCE
    int          delay_send;         // just buffer data 
    int          deliver;            // TERM | PORT
    int          mode;               // LIST | BINARY
    unsigned int htype;              // header type 
    unsigned int psize;              // max packet size
    int          eolchar;            // '\n' line delimiter
    unsigned int hsz;                // the list header size, -1 is large !!!
    unsigned int bsize;              // input buffer size (buffer)
    int          exitf;              // exit on error
    int          ptypkt;             // pty packet mode
} uart_opt_t;


typedef struct _uart_buf_t
{
    size_t   sz;        // allocate buffer size
    uint8_t* base;      // base buffer point
    uint8_t* ptr;       // current pos in buf 
    uint8_t* ptr_start; // packet start pos in buf 
} uart_buf_t;

extern void uart_buf_init(uart_buf_t* bf);
extern void uart_buf_finish(uart_buf_t* bf);
extern void uart_buf_reset(uart_buf_t* bf);
extern int  uart_buf_expand(uart_buf_t* bf, size_t len);
extern int  uart_buf_alloc(uart_buf_t* bf, size_t sz);
extern void uart_buf_restart(uart_buf_t* bf);
extern int uart_buf_push(uart_buf_t* bf, char* buf, size_t len);
extern int uart_buf_packet(uart_buf_t* bf, unsigned int htype, 
			   unsigned max_plen, int eol, unsigned trunc_len);
extern int uart_buf_remain(uart_buf_t* bf, int* len,
			   unsigned int htype, unsigned int psize, int eol);

extern char* format_hex(uint8_t* ptr, int len, char* dst, int dst_len);

typedef struct _uart_queue_t
{
    int            offs;     // offset in current message
    dmessage_t*    mesg;     // current message
    dmessage_t*    front;    // send from front
    dmessage_t*    rear;     // add to rear    
} uart_queue_t;

extern void uart_queue_init(uart_queue_t* q);
extern void uart_queue_clear(uart_queue_t* q);
extern void uart_queue_final(uart_queue_t* q);
extern int  set_packet_bytes(dmessage_t* mp, unsigned int htype);

typedef struct _uart_ctx_t
{
#ifdef __WIN32__
    HANDLE           fh;        // File handle
    OVERLAPPED       in;        // Overlapped input
    OVERLAPPED       out;       // Overlapped output
    OVERLAPPED       stat;      // Overlapped status
    DWORD            statm;     // Status result
    BOOLEAN          reading;   // Overlapped read in progress
    BOOLEAN          writing;   // Overlapped write in progress
    char             rbuf[1];   // Overlapped read into this buffer
#else
    int              tty_fd;    // fd connected to tty device
    int              fd;        // master side when pty else same as tty_fd
#endif
    ErlDrvPort       port;
    ErlDrvTermData   dport;     // the port identifier as DriverTermData
    uint32_t         ref;       // command reference
    uint32_t         flags;     // uart UART_F_xxx
    int              error;     // last known error code
    uint32_t         sflags;    // flags for update state & opts
    uart_com_state_t state;     // communication params 
    uart_opt_t       option;
    int                recv;    // ==1 if in recv 
    int              remain;    // remaining chars to read (recv)

    ErlDrvTermData caller;      // recipient of sync reply

    dthread_t*     self;        // io thread
    dthread_t*     other;       // current calling thread

    uart_queue_t   oq;          // Output queue 
    uart_buf_t     ib;          // Input buffer 
    ErlDrvNowData  t0;          // point at start of operations
    ErlDrvNowData* tp;         //  if tmo then point to t0!
    uint32_t       tmo;        //  timeout value in (ms)
} uart_ctx_t;

extern void com_state_dump(FILE* f, uart_com_state_t* state);
extern void com_state_init(uart_com_state_t* ptr);
extern void com_state_copy(uart_com_state_t* dst,uart_com_state_t* src,
			   uint32_t sflags);

extern void modem_state_dump(FILE* f, uart_modem_state_t state);
extern int modem_state_dterm(dterm_t* p, uart_modem_state_t state);

// uart_options (dterm?)
extern void put_kv_uint(dterm_t* p,ErlDrvTermData key, uint32_t value);
extern void put_kv_int(dterm_t* p,ErlDrvTermData key, int32_t value);
extern void put_kv_atom(dterm_t* p,ErlDrvTermData key, ErlDrvTermData value);
extern void put_kv_bool(dterm_t* p,ErlDrvTermData key, int value);
extern void put_kv_string(dterm_t* p,ErlDrvTermData key, char* value);
extern int uart_get_opts(dterm_t* p, uart_ctx_t* ctx, uint8_t* ptr, size_t len);
extern int uart_parse_opts(char* buf, ErlDrvSizeT len,
			   uart_com_state_t* state, uart_opt_t* option,
			   uint32_t* sflags);

extern int uart_reply_error(uart_ctx_t* ctx, int err);
extern int uart_port_data(uart_ctx_t* ctx, const char* buf, int len);
extern int uart_port_binary_data(uart_ctx_t* ctx, ErlDrvBinary* bin, int offs, int len);
extern int uart_message(uart_ctx_t* ctx, const char* buf, int len);
extern int uart_binary_message(uart_ctx_t* ctx, ErlDrvBinary* bin, 
			       int offs, int len);
extern int uart_closed_message(uart_ctx_t* ctx);
extern int uart_error_message(uart_ctx_t* ctx, int err);

extern int uart_reply_data(uart_ctx_t* ctx, char* buf, int len);
extern int uart_reply_binary_data(uart_ctx_t* ctx, ErlDrvBinary* bin, int offs, int len);

extern int uart_async_error_am(uart_ctx_t* ctx, ErlDrvTermData Port,
			       ErlDrvTermData recipient, ErlDrvTermData Reason);
extern int uart_async_error(uart_ctx_t* ctx, ErlDrvTermData Port,
			    ErlDrvTermData recipient, int err);

#endif
