//
// Input buffer processing
//
#include "uart_drv.h"

void uart_buf_init(uart_buf_t* bf)
{
    memset(bf, 0, sizeof(uart_buf_t));
}

void uart_buf_finish(uart_buf_t* bf)
{
    if (bf->base != NULL)
	DFREE(bf->base);
    uart_buf_init(bf);
}

void uart_buf_reset(uart_buf_t* bf)
{
    bf->ptr_start = bf->base;
    bf->ptr       = bf->base;
}

int uart_buf_alloc(uart_buf_t* bf, size_t sz)
{
    uint8_t* base;

    if ((base = DALLOC(sz)) == NULL)
	return -1;
    bf->sz        = sz;
    bf->base      = base;
    bf->ptr_start = base;
    bf->ptr       = base;
    return (int) sz;
}

//
// Set new size on buffer, used when packet size is determined
// and the buffer is to small.
// buffer must have a size of at least len bytes (counting from ptr_start!)
//
int uart_buf_expand(uart_buf_t* bf, size_t len)
{
    uint8_t* base;
    intptr_t offs1;
    intptr_t offs2;
    size_t used = bf->ptr_start - bf->base;
    size_t ulen = used + len;

    if (bf->sz >= ulen) /* packet will fit */
	return 0;

    offs1 = bf->ptr_start - bf->base;
    offs2 = bf->ptr - bf->ptr_start;

    if ((base = DREALLOC(bf->base, ulen)) == NULL)
	return -1;

    bf->base      = base;
    bf->ptr_start = bf->base + offs1;
    bf->ptr       = bf->ptr_start + offs2;
    bf->sz        = ulen;
    return 0;
}


// Move data so that ptr_start point at buf->base
void uart_buf_restart(uart_buf_t* bf)
{
    if (bf->ptr_start != bf->base) {
	intptr_t n = bf->ptr - bf->ptr_start;
	memmove(bf->base, bf->ptr_start, n);
	bf->ptr_start = bf->base;
	bf->ptr = bf->ptr_start + n;
    }
}

// push data into base
int uart_buf_push(uart_buf_t* bf, char* buf, size_t len)
{
    if (bf->base == NULL) {
	if (uart_buf_alloc(bf, len) < 0)
	    return -1;
	memcpy(bf->base, buf, len);
	bf->ptr = bf->ptr_start + len;
    }
    else {
	size_t sz_before = bf->ptr_start - bf->base;
	size_t sz_filled = bf->ptr - bf->ptr_start;
	
	if (len <= sz_before) {
	    memcpy(bf->ptr_start - len, buf, len);
	    bf->ptr_start -= len;
	}
	else {
	    uint8_t* base = DALLOC(bf->sz+len);
	    if (base == NULL)
		return -1;
	    memcpy(base, buf, len);
	    memcpy(base+len, bf->ptr_start, sz_filled);
	    DFREE(bf->base);
	    bf->sz += len;
	    bf->base = base;
	    bf->ptr_start = base;
	    bf->ptr = base + len + sz_filled;
	}
    }
    return 0;
}

// Return > 0 Total packet length.in bytes
//        = 0 Length unknown, need more data.
//        < 0 Error, invalid format.
// psize  - Max packet length, 0=no limit
// trunc_len - Truncate (lines) if longer, 0=no limit
int uart_buf_packet(uart_buf_t* bf, unsigned int htype, unsigned psize,
		    int eol, unsigned trunc_len)
{
    uint8_t* ptr = bf->ptr_start;
    size_t   n   = bf->ptr - bf->ptr_start;
    size_t   hlen, plen;

    switch (htype & UART_PB_TYPE_MASK) {
    case UART_PB_RAW: {
	unsigned m;
        if (n == 0) 
	    goto more;
	hlen = 0;
	m = (htype & UART_PB_FIXED_MASK) >> 16;
	if ((plen = m) == 0) {
            DEBUGF(" => nothing remain packet=%d", n);
            return n;
        }
	goto remain;
    }

    case UART_PB_N: {
	uint64_t pl = 0;
	hlen = (htype & UART_PB_BYTES_MASK) >> 8;
	if (n < hlen) goto more;
	if (htype & UART_PB_LITTLE_ENDIAN) {
	    ptr += hlen;
	    switch(hlen) {
	    case 8: pl = (pl << 8) | *--ptr;
	    case 7: pl = (pl << 8) | *--ptr;
	    case 6: pl = (pl << 8) | *--ptr;
	    case 5: pl = (pl << 8) | *--ptr;
	    case 4: pl = (pl << 8) | *--ptr;
	    case 3: pl = (pl << 8) | *--ptr;
	    case 2: pl = (pl << 8) | *--ptr;
	    case 1: pl = (pl << 8) | *--ptr;
		break;
	    default: return -1;		
	    }
	}
	else {
	    switch(hlen) {
	    case 8: pl = (pl << 8) | *ptr++;
	    case 7: pl = (pl << 8) | *ptr++;
	    case 6: pl = (pl << 8) | *ptr++;
	    case 5: pl = (pl << 8) | *ptr++;
	    case 4: pl = (pl << 8) | *ptr++;
	    case 3: pl = (pl << 8) | *ptr++;
	    case 2: pl = (pl << 8) | *ptr++;
	    case 1: pl = (pl << 8) | *ptr++;
		break;
	    default: return -1;
	    }
	}
	plen = (unsigned) pl;
	goto remain;
    }

    case UART_PB_LINE_LF: {
        /* UART_PB_LINE_LF:  [Data ... \n]  */
        const uint8_t* ptr2;
        if ((ptr2 = memchr(ptr, eol, n)) == NULL) {
            if ((n >= trunc_len) && (trunc_len!=0)) { /* buffer full */
                DEBUGF(" => line buffer full (no NL)=%d", n);
                return trunc_len;
            }
            goto more;
        }
        else {
            int len = (ptr2 - ptr) + 1; /* including newline */
            if ((len > (int)trunc_len) && (trunc_len != 0)) {
                DEBUGF(" => truncated line=%d", trunc_len);
                return trunc_len;
            }
            DEBUGF(" => nothing remain packet=%d", len);
            return len;
        }
    }
    case UART_PB_GSM_0710: {
	// check for either BASIC or ADVANCED mode
	if (n > 1) {
	    if (ptr[0] == 0x7E) goto advanced_0710;
	    if (ptr[0] == 0xF9) goto basic_0710;
	    plen = 1;
	    while(plen < n) {
		if (ptr[plen] == 0x7D) // escape
		    plen++;
		else if ((ptr[plen] == 0x7E) ||
			 (ptr[plen] == 0xF9))
		    return plen;
		    plen++;
	    }
	    return n;
	}
	goto more;
    }

    case UART_PB_ADVANCED_0710: {
	advanced_0710:
	// 0x7E <escaped data> 0x7E
	if (n > 1) {
	    if (ptr[0] == 0x7E) {
		plen = 1;
		while((plen < n) && (ptr[plen] != 0x7E)) {
		    if (ptr[plen] == 0x7D) {  // escape
			plen++;
			if (plen == n)
			    goto more;
		    }
		    plen++;
		}
		return plen+1;
	    }
	    else {
		plen = 1;
		while(plen < n) {
		    if (ptr[plen] == 0x7D) // escape
			plen++;
		    else if (ptr[plen] == 0x7E)
			return plen;
		    plen++;
		}
		return n;
	    }
	}
	goto more;
    }

    case UART_PB_BASIC_0710: {
	/* UART_BP_BASIC_0710:
	   <<0xF9,Address,Control,Len:7,1:1,Data/Len,FCS,0xF9>>
	   <<0xF9,Address,Control,L0:7,0:1,L1:8,Data/(L1<<7+L0),FCS,0xF9>>
	*/
	basic_0710:
	if (n >= 6) {
	    if (ptr[0] == 0xF9) {
		if (ptr[3] & 0x1) { // short length
		    plen = ptr[3]>>1;  // length of Data
		    hlen = 6;          // rest of the bytes
		    if (n >= plen+hlen)
			return (plen+hlen);
		    goto more;
		}
		else {
		    plen = (ptr[4]<<7)+(ptr[3]>>1);  // length of Data
		    hlen = 6;          // rest of the bytes
		    if (n >= plen+hlen)
			return (plen+hlen);
		    goto more;
		}
	    }
	    else {
		// scan for 0xF9 or send error ? 
		plen = 1;
		while(plen < n) {
		    if (ptr[plen] == 0xF9)
			return plen;  // deliver plen bytes!
		    plen++;
		}
		return n;  // deliver all bytes
	    }
	}
	goto more;
    }
    default:
        DEBUGF(" => case error");
        return -1;
    }

more:
    return 0;

remain:
    {
        int tlen = hlen + plen;
	if (((psize != 0) && (plen > psize))
	    || tlen < (int)hlen) { /* wrap-around protection */
	    return -1;
	}
	return tlen;
    }		


}

/*
** Calculate number of bytes that remain to read before deliver
** Assume buf, ptr_start, ptr has been setup
**
** return  > 0 if more to read
**         = 0 if holding complete packet
**         < 0 on error
**
** if return value == 0 then *len will hold the length of the first packet
**    return value > 0 then if *len == 0 then value means upperbound
**                             *len > 0  then value means exact
**
*/
int uart_buf_remain(uart_buf_t* bf, int* len,
		    unsigned int htype, unsigned int psize, int eol)
{
    uint8_t* ptr = bf->ptr_start;
    int nfill = (bf->ptr - bf->base);  // filled
    int nsz   = bf->sz   - nfill;        // remain
    int n     = bf->ptr  - ptr;  // number of bytes read
    int tlen;

    tlen = uart_buf_packet(bf, htype, psize, eol, bf->sz);
    if (tlen > 0) {
        if (tlen <= n) { // got a packet 
            *len = tlen;
            DEBUGF(" => nothing remain packet=%d", tlen);
            return 0;
        }
        else { // need (known) more
            if (uart_buf_expand(bf, tlen) < 0)
                return -1;
            *len = tlen - n;
            DEBUGF(" => remain=%d", *len);
            return *len;
        }
    }
    else if (tlen == 0) { // need (unknown) more
        *len = 0;
        if (nsz == 0) {
            if (nfill == n) {
                if ((psize != 0) && 
		    ((int)psize > nfill)) {
                    if (uart_buf_expand(bf, psize) < 0)
                        return -1;
                    return psize;
                }
                else
                    goto error;
            }
            DEBUGF(" => restart more=%d", nfill - n);
            return nfill - n;
        }
        else {
            DEBUGF(" => more=%d", nsz);
            return nsz;
        }	    
    }

error:
    DEBUGF(" => packet error");
    return -1;
}
