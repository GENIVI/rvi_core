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
#include <stddef.h>
#include <stdio.h>
#include <stdarg.h>
#include <memory.h>
#include <inttypes.h>
#include "../include/dterm.h"

static ErlDrvTermData am_true;
static ErlDrvTermData am_false;


/******************************************************************************
 *
 *  Term (dterm_t)
 *
 *****************************************************************************/

void dterm_lib_init()
{
    dlib_init();
    am_true = driver_mk_atom("true");
    am_false = driver_mk_atom("false");
}

void dterm_lib_finish()
{
    dlib_finish();
}

void dterm_init(dterm_t* p)
{
    p->dyn_alloc = 0;
    p->dyn_size  = DTERM_FIXED;
    p->base      = p->data;
    p->ptr       = p->data;
    p->ptr_end   = p->data + DTERM_FIXED;
    p->head      = 0;
    p->mark      = 0;
}

int dterm_dump(FILE* f, ErlDrvTermData* spec, int len)
{
    int i = 0;
    while(i < len) {
	switch(spec[i]) {
	case ERL_DRV_NIL:
	    fprintf(f, "%d: NIL\r\n", i);
	    i += 1;
	    break;	    
	case ERL_DRV_INT:
	    fprintf(f, "%d: INT %d\r\n", i, (int)spec[i+1]);
	    i += 2;
	    break;
	case ERL_DRV_UINT:
	    fprintf(f, "%d: UINT %u\r\n", i, (unsigned)spec[i+1]);
	    i += 2;
	    break;
	case ERL_DRV_ATOM:
	    fprintf(f, "%d: ATOM %u\r\n", i, (unsigned)spec[i+1]);
	    i += 2;
	    break;
	case ERL_DRV_PORT:
	    fprintf(f, "%d: PORT %u\r\n", i, (unsigned)spec[i+1]);
	    i += 2;
	    break;
	case ERL_DRV_PID:
	    fprintf(f, "%d: PID %u\r\n", i, (unsigned)spec[i+1]);
	    i += 2;
	    break;
	case ERL_DRV_TUPLE:
	    fprintf(f, "%d: TUPLE %u\r\n", i,(unsigned)spec[i+1]);
	    i += 2;
	    break;
	case ERL_DRV_LIST:
	    fprintf(f, "%d: LIST %u\r\n", i, (unsigned)spec[i+1]);
	    i += 2;
	    break;
	case ERL_DRV_INT64:
	    fprintf(f, "%d: INT64 %" PRId64 "\r\n", i,
		    *(int64_t*)spec[i+1]);
	    i += 2;
	    break;	    
	case ERL_DRV_UINT64:
	    fprintf(f, "%d: UINT64 %" PRIu64 "\r\n", i,
		    *(uint64_t*)spec[i+1]);
	    i += 2;
	    break;
	case ERL_DRV_FLOAT:
	    fprintf(f, "%d: FLOAT %f\r\n", i,
		    *(double*)spec[i+1]);
	    i += 2;
	    break;
	case ERL_DRV_STRING:
	    fprintf(f, "%d: STRING %d \"%s\"\r\n", i,
		    (int)spec[i+2], (char*)spec[i+1]);
	    i += 3;
	    break;
	case ERL_DRV_STRING_CONS:
	    fprintf(f, "%d: STRING_CONS %d \"%s\"\r\n", i,
		    (int)spec[i+2], (char*)spec[i+1]);
	    i += 3;
	    break;
	case ERL_DRV_BUF2BINARY:
	    fprintf(f, "%d: BUF2BINARY_CONS %d <<%s>>\r\n", i,
		    (int)spec[i+2], (char*)spec[i+1]);
	    i += 3;
	    break;
	case ERL_DRV_BINARY:
	    fprintf(f, "%d: BINARY\r\n", i);
	    // not yet!!!
	    i += 4;
	    return -1;
	default:
	    return -1;
	}
    }
    return (int) 0;    
}


// Calculate the size of pointer portion
// When sending a message in the non SMP buffer and data are copied
int dterm_dyn_size(ErlDrvTermData* spec, int len)
{
    size_t n = 0;
    int i = 0;

    while(i < len) {
	switch(spec[i]) {
	case ERL_DRV_NIL:
	    i++;
	    break;	    
	case ERL_DRV_INT:
	case ERL_DRV_UINT:
	case ERL_DRV_ATOM:
	case ERL_DRV_PORT:
	case ERL_DRV_PID:
	case ERL_DRV_TUPLE:
	case ERL_DRV_LIST:
	    i += 2;
	    break;
	case ERL_DRV_INT64:
	    n += sizeof(ErlDrvSInt64);
	    i += 2;
	    break;	    
	case ERL_DRV_UINT64:
	    n += sizeof(ErlDrvUInt64);
	    i += 2;
	    break;
	case ERL_DRV_FLOAT:
	    n += sizeof(double);
	    i += 2;
	    break;
	case ERL_DRV_STRING:
	    n += spec[i+2];
	    i += 3;
	    break;
	case ERL_DRV_STRING_CONS:
	    n += spec[i+2];
	    i += 3;
	    break;
	case ERL_DRV_BUF2BINARY:
	    n += spec[i+2];
	    i += 3;
	    break;
	case ERL_DRV_BINARY:
	    // not yet!!!
	    n += spec[i+2];
	    i += 4;
	    return -1;
	    break;
	default:
	    return -1;
	}
    }
    return (int) n;
}

// When sending a message in the non SMP buffer and data are copied
char* dterm_dyn_copy(ErlDrvTermData* spec, int len, char* ptr)
{
    int i = 0;
    while(i < len) {
	switch(spec[i]) {
	case ERL_DRV_NIL:
	    i++;
	    break;	    
	case ERL_DRV_INT:
	case ERL_DRV_UINT:
	case ERL_DRV_ATOM:
	case ERL_DRV_PORT:
	case ERL_DRV_PID:
	case ERL_DRV_TUPLE:
	case ERL_DRV_LIST:
	    i += 2;
	    break;
	case ERL_DRV_INT64:
	    memcpy(ptr, (void*)spec[i+1], sizeof(ErlDrvSInt64));
	    spec[i+1] = (ErlDrvTermData) ptr;
	    ptr += sizeof(ErlDrvSInt64);
	    i += 2;
	    break;	    
	case ERL_DRV_UINT64:
	    memcpy(ptr, (void*)spec[i+1], sizeof(ErlDrvUInt64));
	    spec[i+1] = (ErlDrvTermData) ptr;
	    ptr += sizeof(ErlDrvUInt64);
	    i += 2;
	    break;
	case ERL_DRV_FLOAT:
	    memcpy(ptr, (void*)spec[i+1], sizeof(double));
	    spec[i+1] = (ErlDrvTermData) ptr;
	    ptr += sizeof(double);
	    i += 2;
	    break;
	case ERL_DRV_STRING:
	    memcpy(ptr, (void*)spec[i+1], spec[i+2]);
	    spec[i+1] = (ErlDrvTermData) ptr;
	    ptr += spec[i+2];
	    i += 3;
	    break;
	case ERL_DRV_STRING_CONS:
	    memcpy(ptr, (void*)spec[i+1], spec[i+2]);
	    spec[i+1] = (ErlDrvTermData) ptr;
	    ptr += spec[i+2];
	    i += 3;
	    break;
	case ERL_DRV_BUF2BINARY:
	    memcpy(ptr, (void*)spec[i+1], spec[i+2]);
	    spec[i+1] = (ErlDrvTermData) ptr;
	    ptr += spec[i+2];
	    i += 3;
	    break;
	case ERL_DRV_BINARY:
	    // wont work yet! (maybe change to BUF2BINARY)
	    ptr += spec[i+2];
	    i += 4;
	    return NULL;
	    break;
	default:
	    return NULL;
	}
    }
    return ptr;
}



// dynamic allocation of dterm_t structure, the data part is
// can be less the DTERM_FIXED in this case
dterm_t* dterm_alloc(size_t size)
{
    size_t  sz = (sizeof(dterm_t) - DTERM_FIXED*sizeof(ErlDrvTermData)) +
	size*sizeof(ErlDrvTermData);
    dterm_t* p;

    if ((p = DALLOC(sz)) != NULL) {
	p->dyn_alloc  = 1;
	p->dyn_size   = size;
	p->base       = p->data;
	p->ptr        = p->data;
	p->ptr_end    = p->data + p->dyn_size;
	p->head       = 0;
	p->mark       = 0;
    }
    return p;
}

void dterm_reset_links(dterm_t* p)
{
    if (p->head) {
	dterm_link_t* lp = p->head;
	while(lp) {
	    dterm_link_t* nlp = lp->next;
	    DFREE(lp);
	    lp = nlp;
	}
	p->head = NULL;
    }
}

void dterm_finish(dterm_t* p)
{
    if (p->base != p->data)
	DFREE(p->base);
    dterm_reset_links(p);
}

void dterm_free(dterm_t* p)
{
    dterm_finish(p);
    if (p->dyn_alloc)
	DFREE(p);
}
    
// reset base pointer & clear link space
void dterm_reset(dterm_t* p)
{
    p->ptr = p->base;  // restart allocation
    dterm_reset_links(p);
}

int dterm_expand(dterm_t* p, size_t n)
{
    ErlDrvTermData* new_base;
    size_t old_size = dterm_allocated_size(p);
    size_t new_size = old_size + n;
    size_t old_sz   = old_size * sizeof(ErlDrvTermData);
    size_t new_sz   = new_size * sizeof(ErlDrvTermData);
    ptrdiff_t offset = p->ptr - p->base;  // offset of ptr

    if (p->base == p->data) {
	if ((new_base = DALLOC(new_sz)) == NULL)
	    return 0;
	memcpy(new_base, p->base, old_sz);
    }
    else if ((new_base = DREALLOC(p->base, new_sz)) == NULL)
	return 0;
    p->base    = new_base;
    p->ptr     = p->base + offset;
    p->ptr_end = new_base + new_size;
    p->base    = new_base;
    return 1;
}

// auxillary space
void* dterm_link_alloc_data(dterm_t* p, size_t size)
{
    dterm_link_t* lp = DALLOC(sizeof(dterm_link_t)+size);
    lp->next = p->head;
    p->head = lp;
    return (void*) &lp->data[0];
}

// auxillary space
void* dterm_link_copy_data(dterm_t* p, void* src, size_t size)
{
    void* dst = dterm_link_alloc_data(p, size);
    memcpy(dst, src, size);
    return dst;
}


void dterm_kv_int(dterm_t* t,ErlDrvTermData key, ErlDrvSInt value)
{
    dterm_mark_t m;
    dterm_tuple_begin(t, &m); {
	dterm_atom(t, key);
	dterm_int(t, value);
    }
    dterm_tuple_end(t, &m);
}

void dterm_kv_uint(dterm_t* t,ErlDrvTermData key, ErlDrvUInt value)
{
    dterm_mark_t m;
    dterm_tuple_begin(t, &m); {
	dterm_atom(t, key);
	dterm_uint(t, value);
    }
    dterm_tuple_end(t, &m);
}

void dterm_kv_atom(dterm_t* t,ErlDrvTermData key, ErlDrvTermData value)
{
    dterm_mark_t m;
    dterm_tuple_begin(t, &m); {
	dterm_atom(t, key);
	dterm_atom(t, value);
    }
    dterm_tuple_end(t, &m);
}

void dterm_kv_bool(dterm_t* t,ErlDrvTermData key, int value)
{
    dterm_mark_t m;
    dterm_tuple_begin(t, &m); {
	dterm_atom(t, key);
	dterm_atom(t, value ? am_true : am_false);
    }
    dterm_tuple_end(t, &m);
}

void dterm_kv_string(dterm_t* t,ErlDrvTermData key, char* value)
{
    dterm_mark_t m;
    size_t len = strlen(value);
    char* dst = (char*) dterm_link_copy_data(t, value, len);

    dterm_tuple_begin(t, &m); {
	dterm_atom(t, key);
	dterm_string(t, dst, len);
    }
    dterm_tuple_end(t, &m);
}
