/****** BEGIN COPYRIGHT *******************************************************
 *
 * Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
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
 ****** END COPYRIGHT ********************************************************/

#ifndef __DDATA_H__
#define __DDATA_H__

#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

#include "dlib.h"

typedef struct _ddata_t
{
    int dyn_alloc;
    uint8_t* base;       /* base pointer */
    uint8_t* rd;         /* read pointer */
    uint8_t* wr;         /* write pointer */
    uint8_t* eob;        /* end of buffer (+1) */
    uint8_t  buf[1];     /* used in some dynamic cases */
} ddata_t;

static void ddata_init(ddata_t* data, uint8_t* buf, uint32_t len, 
		       int dynamic) __attribute__((unused));
static void ddata_r_init(ddata_t* data, uint8_t* buf, uint32_t len, 
			 int dynamic) __attribute__((unused));
static void ddata_reset(ddata_t* data) __attribute__((unused));

static ddata_t* ddata_new(uint8_t* buf, uint32_t len) __attribute__((unused));
static void ddata_final(ddata_t* data) __attribute__((unused));
static void ddata_free(ddata_t* data) __attribute__((unused));
static int ddata_compact(ddata_t* data) __attribute__((unused));
static int ddata_realloc(ddata_t* data, size_t need) __attribute__((unused));
static inline size_t ddata_r_avail(ddata_t* data) __attribute__((unused));
static inline size_t ddata_w_avail(ddata_t* data) __attribute__((unused));

static inline intptr_t ddata_used(ddata_t* data)__attribute__((unused));
static inline uint8_t* ddata_alloc(ddata_t* data, size_t len) __attribute__((unused));
static inline void ddata_add(ddata_t* data, uint8_t* buf, uint32_t len) __attribute__((unused));
static inline void ddata_forward(ddata_t* data, uint32_t len) __attribute__((unused));
static inline void ddata_backward(ddata_t* data, uint32_t len) __attribute__((unused));
static void ddata_send(ddata_t* data, int fd) __attribute__((unused));

#define BOOLEAN        0  /* uint8_t */
#define UINT8          1  /* uint8_t */
#define UINT16         2  /* uint16_t */
#define UINT32         3  /* uint32_t */
#define UINT64         4  /* uint64_t */
#define STRING1        5  /* len byte followed by UTF-8 chars */
#define LIST           6  /* list begin */
#define LIST_END       7  /* list end */
#define TUPLE          8  /* tuple begin */
#define TUPLE_END      9 /* tuple end */
#define ATOM           10 /* len bytes followed by ASCII chars */
#define BINARY         11 /* binary 4-byte len followed by Octets */
#define INT8           12
#define INT16          13
#define INT32          14
#define INT64          15
#define FLOAT32        16
#define FLOAT64        17
#define STRING4        18 /* 4-byte len followed by UTF-8 string  */

#define DDATA_PUT_UINT8(ptr, n) do { \
	((uint8_t*)(ptr))[0] = ((n) & 0xff); \
    } while(0)

#define DDATA_GET_UINT8(ptr) \
    (((uint8_t*)(ptr))[0])

#define DDATA_PUT_UINT16(ptr, n) do { \
	((uint8_t*)(ptr))[0] = (((n) >> 8) & 0xff);	\
	((uint8_t*)(ptr))[1] = ((n) & 0xff);		\
    } while(0)

#define DDATA_GET_UINT16(ptr) \
    ((((uint8_t*)(ptr))[0] << 8) + ((uint8_t*)(ptr))[1])

#define DDATA_PUT_UINT32(ptr, n) do { \
	((uint8_t*)(ptr))[0] = (((n) >> 24) & 0xff);		\
	((uint8_t*)(ptr))[1] = (((n) >> 16) & 0xff);		\
	((uint8_t*)(ptr))[2] = (((n) >> 8) & 0xff);		\
	((uint8_t*)(ptr))[3] = ((n) & 0xff);			\
    } while(0)

#define DDATA_GET_UINT32(ptr) \
    ((((uint8_t*)(ptr))[0] << 24) + (((uint8_t*)(ptr))[1] << 16) +	\
     (((uint8_t*)(ptr))[2] << 8) + ((uint8_t*)(ptr))[3])

#define DDATA_GET_UINT64(ptr) \
    ((((uint64_t)DDATA_GET_UINT32(ptr)) << 32) | \
     DDATA_GET_UINT32(ptr+4))

#ifndef _QUAD_HIGHWORD
#define _QUAD_HIGHWORD 1
#endif

#ifndef _QUAD_LOWWORD
#define _QUAD_LOWWORD 0
#endif

#define DDATA_PUT_UINT64(ptr, n) do { \
	union { uint64_t u64; uint32_t u32[2]; } iu; \
	uint32_t n32;                                 \
	iu.u64 = (n);                                  \
	n32=iu.u32[_QUAD_HIGHWORD]; DDATA_PUT_UINT32((ptr), n32);  \
	n32=iu.u32[_QUAD_LOWWORD]; DDATA_PUT_UINT32((ptr)+4, n32); \
    } while(0)

#define DDATA_PUT_FLOAT32(ptr, n) do { \
	union { float f32; uint32_t u32; } fu;   \
	uint32_t n32;                            \
	fu.f32 = (n);                             \
	n32=fu.f32; DDATA_PUT_UINT32((ptr), n32);       \
    } while(0)

#define DDATA_PUT_FLOAT64(ptr, n) do { \
	union { double f64; uint32_t u32[2]; } fu;       \
	uint32_t n32;                                     \
	fu.f64 = (n);                                   \
	n32=fu.u32[_QUAD_HIGHWORD]; DDATA_PUT_UINT32((ptr), n32);  \
	n32=fu.u32[_QUAD_LOWWORD]; DDATA_PUT_UINT32((ptr)+4, n32); \
    } while(0)


static void ddata_reset(ddata_t* data) 
{
    data->wr = data->base;
    data->rd = data->wr;
}

static void ddata_init(ddata_t* data, uint8_t* buf, uint32_t len, int dynamic)
{
    data->dyn_alloc = dynamic;
    data->base = buf;
    data->rd   = data->base;
    data->wr   = data->base;
    data->eob  = data->base + len;
}

static void ddata_r_init(ddata_t* data, uint8_t* buf, uint32_t len, int dynamic)
{
    data->dyn_alloc = dynamic;
    data->base = buf;
    data->rd   = data->base;
    data->wr   = data->base + len;
    data->eob  = data->base + len;
}

static ddata_t* ddata_new(uint8_t* buf, uint32_t len)
{
    ddata_t* data = DALLOC(sizeof(ddata_t)+len-1);
    if (data == NULL)
	return NULL;
    data->dyn_alloc = 0;    /* dyn_alloc=1 only when buffer is separate! */
    data->base = data->buf;
    data->rd   = data->base;
    data->wr   = data->base;
    if (buf != NULL) {
	memcpy(data->rd, buf, len);
        data->wr += len;
    }
    data->eob  = data->base + len;
    return data;
}

static void ddata_final(ddata_t* data)
{
    if (data->dyn_alloc && (data->base != NULL)) {
	DFREE(data->base);
	data->base = NULL;
    }
}

static void ddata_free(ddata_t* data)
{
    ddata_final(data);
    DFREE(data);
}

static int ddata_compact(ddata_t* data)
{
    size_t used, roffs, woffs;

    roffs = data->rd - data->base;
    woffs = data->wr - data->base;
    used  = woffs-roffs;
    memmove(data->base, data->rd, used);
    data->rd = data->base;
    data->wr = data->base+used;
    return 0;
}

static int ddata_realloc(ddata_t* data, size_t need)
{
    uint8_t* base0;
    size_t roffs, woffs;
    size_t wavail = data->eob - data->wr;
    size_t old_size;
    size_t new_size;

    if (wavail >= need)
	return 0;
    if (need < 256)
	need += 256;
    old_size = data->eob - data->base;
    new_size = old_size + need;
    base0 = data->base;
    roffs = data->rd - data->base;
    woffs = data->wr - data->base;
    if (data->dyn_alloc) {
	void* ptr = DREALLOC(base0, new_size);
	if (ptr == NULL)
	    return -1;
	data->base = ptr;
    }
    else {
	if ((data->base = DALLOC(new_size)) == NULL)
	    return -1;
	memcpy(data->base, base0, old_size);
    }
    data->rd   = data->base + roffs;
    data->wr   = data->base + woffs;
    data->eob  = data->base + new_size;
    data->dyn_alloc = 1;
    return 0;
}

static inline size_t ddata_r_avail(ddata_t* data)
{
    return (data->wr - data->rd);
}

static inline size_t ddata_w_avail(ddata_t* data)
{
    return (data->eob - data->wr);
}

static inline intptr_t ddata_used(ddata_t* data)
{
    return (data->wr - data->rd);
}

static inline uint8_t* ddata_alloc(ddata_t* data, size_t len)
{
    uint8_t* ptr;

    if (ddata_w_avail(data) < len)
	ddata_realloc(data, len);
    ptr = data->wr;
    data->wr += len;
    return ptr;
}

/* add "raw" data to Data buffer */
static inline void ddata_add(ddata_t* data, uint8_t* buf, uint32_t len)
{
    uint8_t* ptr = ddata_alloc(data, len);
    memcpy(ptr, buf, len);
}

/* skip "data" moving ptr forward */
static inline void ddata_forward(ddata_t* data, uint32_t len)
{
    /* uint8_t* ptr = */ ddata_alloc(data, len);
    /* just use the side effect of data_alloc */
}

static inline void ddata_backward(ddata_t* data, uint32_t len)
{
    uint8_t* ptr = data->wr - len;
    if (ptr < data->base)
	data->wr = data->base;
    else
	data->wr = ptr;
}

static void ddata_send(ddata_t* data, int fd)
{
    uint32_t len = (data->wr - data->rd) - 4;
    DDATA_PUT_UINT32(data->rd, len);
    write(fd, data->rd, len+4);
}

/*******************************************************************************
 *
 * PUT Untagged data
 *
 *******************************************************************************/

static inline void ddata_put_UINT8(ddata_t* data, uint8_t n)
{
    uint8_t* ptr = ddata_alloc(data, 1);
    DDATA_PUT_UINT8(ptr, n);
}

static inline void ddata_put_UINT16(ddata_t* data, uint16_t n)
{
    uint8_t* ptr = ddata_alloc(data, 2);
    DDATA_PUT_UINT16(ptr, n);
}

static inline void ddata_put_UINT32(ddata_t* data, uint32_t n)
{
    uint8_t* ptr = ddata_alloc(data, 4);
    DDATA_PUT_UINT32(ptr, n);
}

static inline void ddata_put_UINT64(ddata_t* data, uint64_t n)
{
    uint8_t* ptr = ddata_alloc(data, 8);
    DDATA_PUT_UINT64(ptr, n);
}

/*******************************************************************************
 *
 * PUT tagged data
 *
 *******************************************************************************/

static inline void ddata_put_boolean(ddata_t* data, uint8_t value)
{
    uint8_t* ptr = ddata_alloc(data, 2);
    ptr[0] = BOOLEAN;
    ptr[1] = value;
}

static inline void ddata_put_int8(ddata_t* data, int8_t n)
{
    uint8_t* ptr = ddata_alloc(data, 2);
    *ptr++ = INT8;
    DDATA_PUT_UINT8(ptr, n);
}

static inline void ddata_put_int16(ddata_t* data, int16_t n)
{
    uint8_t* ptr = ddata_alloc(data, 3);
    *ptr++ = INT16;
    DDATA_PUT_UINT16(ptr, n);
}

static inline void ddata_put_int32(ddata_t* data, int32_t n)
{
    uint8_t* ptr = ddata_alloc(data, 5);
    *ptr++ = INT32;
    DDATA_PUT_UINT32(ptr, n);
}

static inline void ddata_put_int64(ddata_t* data, int64_t n)
{
    uint8_t* ptr = ddata_alloc(data, 9);
    *ptr++ = INT64;
    DDATA_PUT_UINT64(ptr, n);
}

static inline void ddata_put_float32(ddata_t* data, float n)
{
    uint8_t* ptr = ddata_alloc(data, 5);
    *ptr++ = FLOAT32;
    DDATA_PUT_FLOAT32(ptr, n);
}

static inline void ddata_put_float64(ddata_t* data, double n)
{
    uint8_t* ptr = ddata_alloc(data, 9);
    *ptr++ = FLOAT64;
    DDATA_PUT_FLOAT64(ptr, n);
}

static inline void ddata_put_uint8(ddata_t* data, uint8_t n)
{
    uint8_t* ptr = ddata_alloc(data, 2);
    *ptr++ = UINT8;
    DDATA_PUT_UINT8(ptr, n);
}

static inline void ddata_put_uint16(ddata_t* data, uint16_t n)
{
    uint8_t* ptr = ddata_alloc(data, 3);
    *ptr++ = UINT16;
    DDATA_PUT_UINT16(ptr, n);
}

static inline void ddata_put_uint32(ddata_t* data, uint32_t n)
{
    uint8_t* ptr = ddata_alloc(data, 5);
    *ptr++ = UINT32;
    DDATA_PUT_UINT32(ptr, n);
}

static inline void ddata_put_uint64(ddata_t* data, uint64_t n)
{
    uint8_t* ptr = ddata_alloc(data, 9);
    *ptr++ = UINT64;
    DDATA_PUT_UINT64(ptr, n);
}

/* put special tag like TUPLE/LIST/TUPLE_END/TUPLE_END */
static inline void ddata_put_tag(ddata_t* data, uint8_t tag)
{
    uint8_t* ptr = ddata_alloc(data, 1);
    *ptr = tag;
}

static inline void ddata_put_atom(ddata_t* data, const char* atom)
{
    uint8_t* ptr;
    uint32_t n = strlen(atom);

    if (n > 0xff)
	n = 0xff; /* truncate */
    ptr = ddata_alloc(data, n+2);
    *ptr++ = ATOM;
    *ptr++ = n;
    memcpy(ptr, atom, n);
}

static inline void ddata_put_string(ddata_t* data, const char* string)
{
    if (string == NULL) {
	uint8_t* ptr = ddata_alloc(data, 2);
	*ptr++ = STRING1;
	*ptr++ = 0;
    }
    else {
	uint32_t n = strlen(string);
	if (n <= 0xff) {
	    uint8_t* ptr = ddata_alloc(data, n+2);
	    *ptr++ = STRING1;
	    *ptr++ = n;
	    memcpy(ptr, string, n);
	}
	else {
	    uint8_t* ptr = ddata_alloc(data, n+5);
	    *ptr++ = STRING4;
	    DDATA_PUT_UINT32(ptr, n);
	    ptr += 4;
	    memcpy(ptr, string, n);
	}
    }
}

static inline void ddata_put_binary(ddata_t* data, const uint8_t* buf, uint32_t len)
{
    uint8_t* ptr = ddata_alloc(data, len+5);
    *ptr++ = BINARY;
    DDATA_PUT_UINT32(ptr, len);
    ptr += 4;
    memcpy(ptr, buf, len);
}

/*******************************************************************************
 *
 * GET untagged data
 *
 *******************************************************************************/

static inline int ddata_get_boolean(ddata_t* data, uint8_t* val)
{
    if (ddata_r_avail(data) < sizeof(uint8_t)) return 0;
    *val = (DDATA_GET_UINT8(data->rd) != 0);
    data->rd += sizeof(uint8_t);
    return 1;
}

static inline int ddata_get_uint8(ddata_t* data, uint8_t* val)
{
    if (ddata_r_avail(data) < sizeof(uint8_t)) return 0;
    *val = DDATA_GET_UINT8(data->rd);
    data->rd += sizeof(uint8_t);
    return 1;
}

static inline int ddata_get_uint16(ddata_t* data, uint16_t* val)
{
    if (ddata_r_avail(data) < sizeof(uint16_t)) return 0;
    *val = DDATA_GET_UINT16(data->rd);
    data->rd += sizeof(uint16_t);
    return 1;
}

static inline int ddata_get_uint32(ddata_t* data, uint32_t* val)
{
    if (ddata_r_avail(data) < sizeof(uint32_t)) return 0;
    *val = DDATA_GET_UINT32(data->rd);
    data->rd += sizeof(uint32_t);
    return 1;
}

static inline int ddata_get_int32(ddata_t* data, int32_t* val)
{
    if (ddata_r_avail(data) < sizeof(int32_t)) return 0;
    *val = (int32_t) DDATA_GET_UINT32(data->rd);
    data->rd += sizeof(int32_t);
    return 1;
}

static inline int ddata_get_uint64(ddata_t* data, uint64_t* val)
{
    if (ddata_r_avail(data) < sizeof(uint64_t)) return 0;
    *val = DDATA_GET_UINT64(data->rd);
    data->rd += sizeof(uint64_t);
    return 1;
}
#endif
