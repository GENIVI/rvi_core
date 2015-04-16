//
// Output queue processing
//
#include "uart_drv.h"

void uart_queue_init(uart_queue_t* q)
{
    memset(q, 0, sizeof(uart_queue_t));
}

void uart_queue_clear(uart_queue_t* q)
{
    dmessage_t* mp;

    if ((mp = q->mesg) != NULL) {
	dmessage_free(mp);
	q->mesg = NULL;
    }
    q->offs = 0;
    mp = q->front;
    while(mp) {
	dmessage_t* tmp = mp->next;
	dmessage_free(mp);
	mp = tmp;
    }
    q->front = NULL;
    q->rear = NULL;
}

void uart_queue_final(uart_queue_t* q)
{
    uart_queue_clear(q);
}

//
// Patch message with header bytes
// 8 bytes are reserved before message on entry
// mp->data + 8 == mp->buffer
//
int set_packet_bytes(dmessage_t* mp, unsigned int htype)
{
#define UINT8(x) ((uint8_t) (x))
    if ((htype & UART_PB_TYPE_MASK) == UART_PB_N) {
	int h_len;
	uint64_t pl = (uint64_t) mp->used;
	h_len = (htype & UART_PB_BYTES_MASK) >> 8;
	// FIXME check that we have at least 8 byte reserved
	if (htype & UART_PB_LITTLE_ENDIAN) {
	    uint8_t* ptr = (uint8_t*) mp->buffer-h_len;
	    switch(h_len) {
	    case 8: *ptr++ = UINT8(pl); pl >>= 8;
	    case 7: *ptr++ = UINT8(pl); pl >>= 8;
	    case 6: *ptr++ = UINT8(pl); pl >>= 8;
	    case 5: *ptr++ = UINT8(pl); pl >>= 8;
	    case 4: *ptr++ = UINT8(pl); pl >>= 8;
	    case 3: *ptr++ = UINT8(pl); pl >>= 8;
	    case 2: *ptr++ = UINT8(pl); pl >>= 8;
	    case 1: *ptr = UINT8(pl); break;
	    default: return -1;
	    }
	    mp->buffer -= h_len;
	    mp->used   += h_len;
	}
	else {
	    uint8_t* ptr = (uint8_t*)mp->buffer;
	    switch(h_len) {
	    case 8: *--ptr = UINT8(pl); pl >>= 8;
	    case 7: *--ptr = UINT8(pl); pl >>= 8;
	    case 6: *--ptr = UINT8(pl); pl >>= 8;
	    case 5: *--ptr = UINT8(pl); pl >>= 8;
	    case 4: *--ptr = UINT8(pl); pl >>= 8;
	    case 3: *--ptr = UINT8(pl); pl >>= 8;
	    case 2: *--ptr = UINT8(pl); pl >>= 8;
	    case 1: *--ptr = UINT8(pl); break;
	    default: return -1;
	    }
	    mp->buffer = (char*)ptr;
	    mp->used   += h_len;
	}
    }
    return 0;
#undef UINT8
}

