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
#include <stdint.h>
#include <memory.h>
#include <errno.h>

#include "../include/dlog.h"
#include "../include/dlib.h"

static size_t dlib_num_allocated = 0;
static size_t dlib_tot_allocated = 0;

#ifndef NO_ERL_DRIVER
static ErlDrvMutex* dlib_mtx;  // USE ME
#endif

#if defined(NO_ERL_DRIVER)
#define MEM_ALLOC(sz)        malloc((sz))
#define MEM_ZALLOC(sz)       zalloc((sz))
#define MEM_FREE(ptr)        free((ptr))
#define MEM_REALLOC(ptr,sz)  realloc((ptr),(sz))
#define MEM_ZERO(ptr,sz)     memset((ptr),'\0',(sz))
#else
#define MEM_ALLOC(sz)        driver_alloc((sz))
#define MEM_ZALLOC(sz)       zalloc((sz))
#define MEM_FREE(ptr)        driver_free((ptr))
#define MEM_REALLOC(ptr,sz)  driver_realloc((ptr),(sz))
#define MEM_ZERO(ptr,sz)     memset((ptr),'\0',(sz))
#endif

#define MARK   0x5A5A5A5A
#define FREE   0x0
#define MAGIC  0xCAFEFEED

typedef struct _dheader_t {
    uint32_t mark;
    uint32_t magic;
    size_t   sz;
    char*    file;
    int      line;
    double   pad;
    char     data[0];
}  dheader_t;

void dlib_init()
{
    dlib_num_allocated = 0;
    dlib_tot_allocated = 0;
#ifndef NO_ERL_DRIVER
    dlib_mtx = erl_drv_mutex_create("dlib_mtx");
#endif
}

void dlib_finish()
{
#ifndef NO_ERL_DRIVER
    erl_drv_mutex_destroy(dlib_mtx);
#endif
}

// return (known) number of allocated memory
size_t dlib_allocated(void)
{
    return dlib_num_allocated;
}

// return the the sum of all memory allocations so far
size_t dlib_total_allocated(void)
{
    return dlib_tot_allocated;
}

void dlib_break_here()
{
    exit(1);
}

// allocate a memory block
// store | mark | magic | sz | file | line |  .... |
void* dlib_alloc(size_t sz, char* file, int line)
{
    dheader_t* dptr;
    if ((dptr = MEM_ALLOC(sizeof(dheader_t)+sz)) == NULL) {
	dlog_emit_error(DLOG_ALERT, file, line, "allocation failed");
	return NULL;
    }
    else {
	dptr->mark  = MARK;
	dptr->magic = MAGIC;
	dptr->sz = sz;
	dptr->file = file;
	dptr->line = line;
	dlib_num_allocated += sz;
	dlib_tot_allocated += sz;
	return (void*) &dptr->data[0];
    }
}

void* dlib_zalloc(size_t sz, char* file, int line)
{
    dheader_t* dptr;
    if ((dptr = MEM_ALLOC(sizeof(dheader_t)+sz)) == NULL) {
	dlog_emit_error(DLOG_ALERT, file, line, "allocation failed");
	return NULL;
    }
    else {
	dptr->mark = MARK;
	dptr->magic = MAGIC;
	dptr->sz = sz;
	dptr->file = file;
	dptr->line = line;
	memset(&dptr->data[0], '\0', sz);
	dlib_num_allocated += sz;
	dlib_tot_allocated += sz;
	return (void*) &dptr->data[0];
    }
}

void dlib_free(void* ptr, char* file, int line)
{
    if (ptr) {
	dheader_t* dptr = (dheader_t*) ((char*)ptr - sizeof(dheader_t));
	if ((dptr->mark == MARK) && (dptr->magic == MAGIC)) {
	    if (dptr->sz > dlib_num_allocated) {
		dlog_emit_error(DLOG_EMERGENCY, file, line,
				"free more data than allocated");
		dlib_break_here();
	    }
	    dptr->mark = FREE;
	    dlib_num_allocated -= dptr->sz;
	    MEM_FREE(dptr);
	}
	else if (dptr->magic == MAGIC) {
	    dlog_emit_error(DLOG_EMERGENCY, file, line,
			    "block %p already free, allocated by %s:%d",
			    dptr, dptr->file, dptr->line);
	    dlib_break_here();
	}
	else {
	    dlog_emit_error(DLOG_EMERGENCY, file, line,
			    "block %p mark=%x, magic=%x, not dallocated",
			    dptr, dptr->mark, dptr->magic);
	    dlib_break_here();
	}
    }
}

void* dlib_realloc(void* ptr, size_t sz, char* file, int line)
{
    if (ptr) {
	dheader_t* dptr = (dheader_t*) ((char*)ptr - sizeof(dheader_t));
	ptr = (void*) dptr;
	if ((dptr->mark == MARK) && (dptr->magic == MAGIC)) {
	    dptr->mark = FREE;  // mark potential old segment as FREE
	    if (dptr->sz > dlib_num_allocated) {
		dlog_emit_error(DLOG_EMERGENCY, file, line,
				"realloc release more data than allocated");
		dlib_break_here();
	    }
	    dlib_num_allocated -= dptr->sz;
	}
	else if (dptr->magic == MAGIC) {
	    dlog_emit_error(DLOG_EMERGENCY, file, line,
			    "block is free, allocated by %s:%d",
			    dptr->file, dptr->line);
	    dlib_break_here();
	}
	else {
	    dlog_emit_error(DLOG_EMERGENCY, file, line,
			    "block %p mark=%x, magic=%x, not dallocated",
			    dptr, dptr->mark, dptr->magic);
	    dlib_break_here();
	}
    }
    if ((ptr = MEM_REALLOC(ptr, sizeof(dheader_t)+sz)) == NULL) {
	dlog_emit_error(DLOG_ALERT, file, line, "reallocation failed");
	return ptr;
    }
    else {
	dheader_t* dptr = (dheader_t*) ptr;
	dptr->mark  = MARK;
	dptr->magic = MAGIC;
	dptr->sz = sz;
	// set last alloaction/reallocation position
	dptr->file = file;
	dptr->line = line;
	dlib_num_allocated += sz;
	dlib_tot_allocated += sz;
	return (void*) &dptr->data[0];
    }
}

void dlib_zero(void* ptr, size_t sz, char* file, int line)
{
    if (ptr) {
	dheader_t* dptr = (dheader_t*) ((char*)ptr - sizeof(dheader_t));
	if ((dptr->mark == MARK) && (dptr->magic == MAGIC)) {
	    if (sz > dptr->sz) {
		dlog_emit_error(DLOG_EMERGENCY, file, line, "overwrite heap");
		dlib_break_here();
	    }
	    memset(&dptr->data[0], '\0', sz);
	}
	else if (dptr->magic == MAGIC) {
	    dlog_emit_error(DLOG_EMERGENCY, file, line,
			    "zero free block, allocated by %s:%d",
			    dptr->file, dptr->line);
	    dlib_break_here();
	}
	else {
	    dlog_emit_error(DLOG_EMERGENCY, file, line,
			    "block %p mark=%x, magic=%x, not dallocated",
			    dptr, dptr->mark, dptr->magic);
	    dlib_break_here();
	}
    }
    else {
	dlog_emit_error(DLOG_EMERGENCY, file, line,
			"null pointer");
	dlib_break_here();
    }
}
