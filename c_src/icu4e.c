
#include "erl_nif.h"

static ErlNifResourceType* icu4e_RESOURCE;

typedef struct
{
} icu4e_handle;

// Prototypes
ERL_NIF_TERM icu4e_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM icu4e_myfunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, icu4e_new},
    {"myfunction", 1, icu4e_myfunction}
};

ERL_NIF_TERM icu4e_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    icu4e_handle* handle = enif_alloc_resource(env,
                                                    icu4e_RESOURCE,
                                                    sizeof(icu4e_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(env, handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


ERL_NIF_TERM icu4e_myfunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static void icu4e_resource_cleanup(ErlNifEnv* env, void* arg)
{
    // Delete any dynamically allocated memory stored in icu4e_handle
    // icu4e_handle* handle = (icu4e_handle*)arg;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    icu4e_RESOURCE = enif_open_resource_type(env, "icu4e_resource",
                                                  &icu4e_resource_cleanup,
                                                  ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                                  0);
    return 0;
}

ERL_NIF_INIT(icu4e, nif_funcs, &on_load, NULL, NULL, NULL);
