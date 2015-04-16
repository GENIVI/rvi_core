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
#ifndef __DLIB_H__
#define __DLIB_H__

#include <string.h>

#ifndef NO_ERL_DRIVER
#include "erl_driver.h"
#endif

extern void dlib_init(void);
extern void dlib_finish(void);

extern void* dlib_alloc(size_t sz, char* file, int line);
extern void* dlib_zalloc(size_t sz, char* file, int line);
extern void  dlib_free(void* ptr, char* file, int line);
extern void* dlib_realloc(void* ptr, size_t sz, char* file, int line);
extern void  dlib_zero(void* ptr, size_t sz, char* file, int line);
extern size_t dlib_allocated(void);
extern size_t dlib_total_allocated(void);

#ifdef DEBUG_MEM
#define DALLOC(sz)        dlib_alloc((sz),__FILE__,__LINE__)
#define DZALLOC(sz)       dlib_zalloc((sz),__FILE__,__LINE__)
#define DFREE(ptr)        dlib_free((ptr),__FILE__,__LINE__)
#define DREALLOC(ptr,sz)  dlib_realloc((ptr),(sz),__FILE__,__LINE__)
#define DZERO(ptr,sz)     dlib_zero((ptr),(sz),__FILE__,__LINE__)
#else 

#if defined(NO_ERL_DRIVER)
#define DALLOC(sz)        malloc((sz))
#define DZALLOC(sz)       zalloc((sz))
#define DFREE(ptr)        free((ptr))
#define DREALLOC(ptr,sz)  realloc((ptr),(sz))
#define DZERO(ptr,sz)     memset((ptr),'\0',(sz))
#else
#define DALLOC(sz)        driver_alloc((sz))
#define DZALLOC(sz)       zalloc((sz))
#define DFREE(ptr)        driver_free((ptr))
#define DREALLOC(ptr,sz)  driver_realloc((ptr),(sz))
#define DZERO(ptr,sz)     memset((ptr),'\0',(sz))
#endif

static inline void* zalloc(size_t sz)
{
    void* ptr = DALLOC(sz);
    if (ptr != NULL)
	DZERO(ptr,sz);
    return ptr;
}

#endif

#endif
