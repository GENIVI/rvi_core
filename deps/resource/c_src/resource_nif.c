/****** BEGIN COPYRIGHT *******************************************************
 *
 * Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 ****** END COPYRIGHT ********************************************************/
//
// Resource nif
//

#include <stdint.h>
#include "erl_nif.h"

// #define DEBUG

#ifdef DEBUG
#include <stdio.h>
#define DBG(...) printf(__VA_ARGS__)
#else
#define DBG(...)
#endif

// Atom macros
#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)			\
    atm_##name = enif_make_atom(env,string)

// Type names
DECL_ATOM(resource);

static int res_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int res_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int res_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, 
			 ERL_NIF_TERM load_info);
static void res_unload(ErlNifEnv* env, void* priv_data);

ErlNifResourceType* res_resource;

static ERL_NIF_TERM notify_when_destroyed(ErlNifEnv* env, int argc, 
					  const ERL_NIF_TERM argv[]);

ErlNifFunc res_funcs[] =
{
    { "notify_when_destroyed", 2, notify_when_destroyed },
};

typedef struct {
    ErlNifEnv*   env;
    ErlNifPid    pid;
    ERL_NIF_TERM message;
} res_object_t;
    

static void res_dtor(ErlNifEnv* env, res_object_t* obj)
{
    // mabe add a timestamp os:timestamp() when event was sent!?
    enif_send(0, &obj->pid, obj->env, obj->message);
    enif_free_env(obj->env);
}

//
// resource:notify_when_destroyed(Pid, Message)
// send a message to Pid when reource is destroyed
//
ERL_NIF_TERM notify_when_destroyed(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    res_object_t* obj;
    ERL_NIF_TERM r;

    if (!enif_is_pid(env, argv[0]))
	return enif_make_badarg(env);

    if (!(obj = enif_alloc_resource(res_resource, sizeof(res_object_t))))
	return enif_make_badarg(env);

    if (!(obj->env = enif_alloc_env())) {
	enif_release_resource(obj);
	return enif_make_badarg(env);
    }

    if (!enif_get_local_pid(env, argv[0], &obj->pid)) {
	enif_release_resource(obj);
	return enif_make_badarg(env);	
    }

    obj->message = enif_make_copy(obj->env, argv[1]);

    r = enif_make_tuple3(env, ATOM(resource), 
			 enif_make_ulong(env, (unsigned long) obj),
			 enif_make_resource(env, obj));
    enif_release_resource(obj);
    return r;
}

static int res_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) load_info;
    ErlNifResourceFlags tried;

    DBG("res_load\r\n");

    LOAD_ATOM(resource);

    res_resource = enif_open_resource_type(env, 0, "resource",
					   (ErlNifResourceDtor*) res_dtor,
					   ERL_NIF_RT_CREATE,
					   &tried);


    *priv_data = 0;
    return 0;
}

static int res_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    DBG("res_reload\r\n");
    return 0;
}

static int res_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, 
			 ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    DBG("res_upgrade\r\n");
    *priv_data = *old_priv_data;
    return 0;
}

static void res_unload(ErlNifEnv* env, void* priv_data)
{
    (void) env;
    DBG("res_unload\r\n");
}

ERL_NIF_INIT(resource, res_funcs,
	     res_load, res_reload, 
	     res_upgrade, res_unload)




