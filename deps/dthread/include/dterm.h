/****** BEGIN COPYRIGHT *******************************************************
 *
 * Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
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
#ifndef __DTERM_H__
#define __DTERM_H__

#include <stdio.h>
#include "dlib.h"

struct _dterm_t;

// ErlDrvTerm construction 
#define DTERM_EXTRA  64
#define DTERM_FIXED  256

typedef struct _dterm_link_t {
    struct _dterm_link_t* next;
    unsigned char data[];
} dterm_link_t;

typedef struct _dterm_mark_t {
    struct _dterm_mark_t* next;
    ErlDrvTermData   type;    // TUPLE | LIST ?RECORD?
    size_t           count;   // number of elements
} dterm_mark_t;

typedef struct _dterm_t {
    int dyn_alloc;
    int dyn_size;    // real size of data (if dynamic)
    ErlDrvTermData* base;
    ErlDrvTermData* ptr;
    ErlDrvTermData* ptr_end;
    dterm_link_t* head;
    dterm_mark_t* mark;
    ErlDrvTermData  data[DTERM_FIXED];
} dterm_t;

extern void dterm_lib_init(void);
extern void dterm_lib_finish(void);

extern void dterm_init(dterm_t* p);
extern void dterm_reset(dterm_t* p);
extern void dterm_finish(dterm_t* p);
extern int dterm_expand(dterm_t* p, size_t n);
extern void* dterm_link_alloc_data(dterm_t* p, size_t size);
extern void* dterm_link_copy_data(dterm_t* p, void* src, size_t size);

extern int dterm_dyn_size(ErlDrvTermData* spec, int len);
extern char* dterm_dyn_copy(ErlDrvTermData* spec, int len, char* ptr);
extern int dterm_dump(FILE*, ErlDrvTermData* spec, int len);

extern void dterm_kv_int(dterm_t* t,ErlDrvTermData key, ErlDrvSInt value);
extern void dterm_kv_uint(dterm_t* t,ErlDrvTermData key, ErlDrvUInt value);
extern void dterm_kv_atom(dterm_t* t,ErlDrvTermData key, ErlDrvTermData value);
extern void dterm_kv_bool(dterm_t* t,ErlDrvTermData key, int value);
extern void dterm_kv_string(dterm_t* t,ErlDrvTermData key, char* value);


static inline ErlDrvTermData* dterm_data(dterm_t* p)
{
    return p->base;
}

static inline size_t dterm_allocated_size(dterm_t* p)
{
    return (p->ptr_end - p->base);
}

static inline size_t dterm_used_size(dterm_t* p)
{
    return (p->ptr - p->base);
}

static inline size_t dterm_remain(dterm_t* p)
{
    return (p->ptr_end - p->ptr);
}

static inline int dterm_need(dterm_t* p, size_t need)
{
    size_t remain = dterm_remain(p);
    if (remain < need)
	return dterm_expand(p, (need-remain)+DTERM_EXTRA);
    return 1;
}

static inline int dterm_put(dterm_t* p, ErlDrvTermData d1)
{
    if (dterm_need(p, 1)) {
	p->ptr[0] = d1;
	p->ptr += 1;
	if (p->mark) p->mark->count++;
	return 1;
    }
    return 0;
}

static inline int dterm_put2(dterm_t* p, ErlDrvTermData d1, ErlDrvTermData d2)
{
    if (dterm_need(p, 2)) {
	p->ptr[0] = d1;
	p->ptr[1] = d2;
	p->ptr += 2;
	if (p->mark) p->mark->count++;
	return 1;
    }
    return 0;
}

static inline int dterm_put3(dterm_t* p, ErlDrvTermData d1, ErlDrvTermData d2,
			     ErlDrvTermData d3)
{
    if (dterm_need(p, 3)) {
	p->ptr[0] = d1;
	p->ptr[1] = d2;
	p->ptr[2] = d3;
	p->ptr += 3;
	if (p->mark) p->mark->count++;
	return 1;
    }
    return 0;
}

static inline int dterm_put4(dterm_t* p, ErlDrvTermData d1, ErlDrvTermData d2,
			     ErlDrvTermData d3,ErlDrvTermData d4)
{
    if (dterm_need(p, 4)) {
	p->ptr[0] = d1;
	p->ptr[1] = d2;
	p->ptr[2] = d3;
	p->ptr[3] = d4;
	p->ptr += 4;
	if (p->mark) p->mark->count++;
	return 1;
    }
    return 0;
}

static inline int dterm_int(dterm_t* p, ErlDrvSInt val)
{
    return dterm_put2(p, ERL_DRV_INT, val);
}

static inline int dterm_uint(dterm_t* p, ErlDrvUInt val)
{
    return dterm_put2(p, ERL_DRV_UINT, val);
}

static inline int dterm_int64(dterm_t* p, ErlDrvSInt64 val)
{
    void* ptr = dterm_link_copy_data(p, (void*) &val, sizeof(val));
    return dterm_put2(p, ERL_DRV_INT64, (ErlDrvTermData)ptr);
}

static inline int dterm_uint64(dterm_t* p, ErlDrvUInt64 val)
{
    void* ptr = dterm_link_copy_data(p, (void*) &val, sizeof(val));
    return dterm_put2(p, ERL_DRV_UINT64, (ErlDrvTermData)ptr);
}

static inline int dterm_float(dterm_t* p, double val)
{
    void* ptr = dterm_link_copy_data(p, (void*) &val, sizeof(val));
    return dterm_put2(p, ERL_DRV_FLOAT, (ErlDrvTermData)ptr);
}

static inline int dterm_atom(dterm_t* p, ErlDrvTermData atom)
{
    return dterm_put2(p, ERL_DRV_ATOM, atom);
}

static inline int dterm_port(dterm_t* p, ErlDrvTermData port)
{
    return dterm_put2(p, ERL_DRV_PORT, port);
}

static inline int dterm_pid(dterm_t* p, ErlDrvTermData pid)
{
    return dterm_put2(p, ERL_DRV_PID, pid);
}

static inline int dterm_binary(dterm_t* p, ErlDrvBinary* bin, ErlDrvUInt size,
			       ErlDrvUInt offs)
{
    return dterm_put4(p, ERL_DRV_BINARY, (ErlDrvTermData)bin, size, offs);
}

static inline int dterm_string(dterm_t* p, const char* ptr, size_t len)
{
    return dterm_put3(p, ERL_DRV_STRING, (ErlDrvTermData)ptr, len);
}

static inline int dterm_string_cons(dterm_t* p, const char* ptr, size_t len)
{
    if (!dterm_put3(p, ERL_DRV_STRING_CONS, (ErlDrvTermData)ptr, len))
	return 0;
    if (p->mark) {
	if (p->mark->count == 0)
	    return 0;
	p->mark->count--;
    }
    return 1;
}

static inline int dterm_buf_binary(dterm_t* p, const char* ptr, size_t len)
{
    return dterm_put3(p, ERL_DRV_BUF2BINARY, (ErlDrvTermData)ptr, len);
}

static inline int dterm_tuple(dterm_t* p, size_t size)
{
    return dterm_put2(p, ERL_DRV_TUPLE, size);
}

static inline int dterm_nil(dterm_t* p)
{
    return dterm_put(p, ERL_DRV_NIL);
}

static inline int dterm_list(dterm_t* p, size_t size)
{
    return dterm_put2(p, ERL_DRV_LIST, size);
}

static inline int dterm_list_begin(dterm_t* p, dterm_mark_t* mark)
{
    mark->next = p->mark;
    p->mark    = mark;
    mark->type = ERL_DRV_LIST;
    mark->count = 0;
    return 1;
}

static inline int dterm_list_end(dterm_t* p, dterm_mark_t* mark)
{
    dterm_nil(p);
    p->mark = mark->next;  // count the list creation below
    return dterm_list(p, mark->count);
}

static inline int dterm_tuple_begin(dterm_t* p, dterm_mark_t* mark)
{
    mark->next = p->mark;
    p->mark    = mark;
    mark->type = ERL_DRV_TUPLE;
    mark->count = 0;
    return 1;
}

static inline int dterm_tuple_end(dterm_t* p, dterm_mark_t* mark)
{
    p->mark = mark->next;
    return dterm_tuple(p, mark->count);
}

#endif
