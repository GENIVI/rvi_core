#ifndef __BT_DRV_H__
#define __BT_DRV_H__

#include <stdint.h>

#include "bt_sub.h"

#define CMD_PING              1
#define CMD_RECENT_DEVICES    2
#define CMD_PAIRED_DEVICES    3
#define CMD_FAVORITE_DEVICES  4
#define CMD_INQUIRY_START     5
#define CMD_INQUIRY_STOP      6
#define CMD_REMOTE_NAME       7
#define CMD_CONNECT           8
#define CMD_DISCONNECT        9
#define CMD_DEVICE_INFO      10
#define CMD_SERVICE_INFO     11
#define CMD_SERVICE_QUERY    12
#define CMD_SERVICE_ADD      13
#define CMD_SERVICE_DEL      14
#define CMD_SERVICE_RFCOMM   15
#define CMD_LOCAL_INFO       16
#define CMD_DEBUG            17

/* RCCOMM Channels */
#define CMD_RFCOMM_OPEN      20
#define CMD_RFCOMM_CLOSE     21
#define CMD_RFCOMM_LISTEN    22
#define CMD_RFCOMM_SEND      23
#define CMD_RFCOMM_ACCEPT    24
#define CMD_RFCOMM_MTU       25
#define CMD_RFCOMM_ADDRESS   26
#define CMD_RFCOMM_CHANNEL   27

/* L2CAP */
#define CMD_L2CAP_OPEN        30
#define CMD_L2CAP_CLOSE       31
#define CMD_L2CAP_LISTEN      32
#define CMD_L2CAP_SEND        33
#define CMD_L2CAP_ACCEPT      34
#define CMD_L2CAP_MTU         35
#define CMD_L2CAP_ADDRESS     36
#define CMD_L2CAP_PSM         37

/* device info codes */
#define NFO_DEVICE_NAME             1  /* string */
#define NFO_DEVICE_CLASS            2  /* uint32 */
#define NFO_DEVICE_CLOCK            3  /* uint16 */
#define NFO_DEVICE_INQUIRY          4  /* date */
#define NFO_DEVICE_ACCESS           5  /* date */
#define NFO_DEVICE_UPDATE           6  /* date */
#define NFO_DEVICE_IS_FAVORITE      7  /* Boolean */
#define NFO_DEVICE_IS_PAIRED        8  /* Boolean */
#define NFO_DEVICE_IS_CONNECTED     9  /* Boolean */

/* local info codes */
#define NFO_LOCAL_NAME              1 /* string */
#define NFO_LOCAL_CLASS             2  /* uint32 */
#define NFO_LOCAL_ADDRESS           3 /* addr */
#define NFO_LOCAL_DISCOVERABLE      4 /* Boolean */
#define NFO_LOCAL_POWER_STATE       5 /* on | off */
/* add more */

#define REPLY_OK            1
#define REPLY_ERROR         2
#define REPLY_EVENT         3

/* extension data types */
#define ADDR           100  /* bluetooth address 6 bytes */
#define DATE           101  /* uint32 seconds since 1970 unix-time */

typedef struct _bt_ctx_t
{
    size_t  pbuf_len;        // number of bytes in pbuf
    uint8_t pbuf[4];         // packet length bytes
    const uint8_t* ptr;       // data ptr
    size_t len;             // length of data
    size_t remain;          // remaining bytes to read
    uint8_t* packet;          // the data packet being built
    subscription_list_t list;
    void* drv_data;            // Extra, per-driver specific data.
} bt_ctx_t;

#define LISTEN_QUEUE_LENGTH 8  /* max connections can only be 7 ? */

typedef struct {
    bt_ctx_t* ctx;  // access to subscription list
    int qh;
    int qt;
    void* qelem[LISTEN_QUEUE_LENGTH];
    subscription_list_t wait;
} listen_queue_t;

#endif

