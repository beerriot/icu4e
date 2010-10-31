
#include "erl_nif.h"
#include "unicode/utypes.h"
#include "unicode/ustring.h"
#include "unicode/unorm.h"
#include "unicode/ubrk.h"

/* Prototypes */
ERL_NIF_TERM ustring_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_cmp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_casecmp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_toupper(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_tolower(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM error_tuple(ErlNifEnv* env, const char* msg);

static ErlNifFunc nif_funcs[] =
{
    {"new", 1, ustring_new},
    {"cmp", 2, ustring_cmp},
    {"casecmp", 2, ustring_casecmp},
    {"toupper", 1, ustring_toupper},
    {"tolower", 1, ustring_tolower},
    {"length", 2, ustring_length}
};

ERL_NIF_TERM ustring_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary norm, in;
    int32_t size, used;
    UErrorCode ec = U_ZERO_ERROR;

    if(!enif_inspect_binary(env, argv[0], &in))
        return enif_make_badarg(env);
    size = in.size;

    do {
        if(!enif_alloc_binary(env, size, &norm))
            return error_tuple(env, "failed to alloc normalized binary");

        used = unorm_normalize((UChar*)in.data, in.size/2, UNORM_NFC, 0,
                               (UChar*)norm.data, norm.size/2, &ec);
        if (ec == U_BUFFER_OVERFLOW_ERROR) {
            /* enlarge buffer if it was too small */
            enif_release_binary(env, &norm);
            size = used*2;
        }
    } while (ec == U_BUFFER_OVERFLOW_ERROR);

    if (U_FAILURE(ec)) {
        enif_release_binary(env, &norm);
        return error_tuple(env, u_errorName(ec));
    }

    if (used*2 != size) {
        /* shrink binary if it was too large */
        enif_realloc_binary(env, &norm, used*2);
    }

    return enif_make_binary(env, &norm);
}

ERL_NIF_TERM ustring_cmp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary stra, strb;
    int32_t result;

    if (!enif_inspect_binary(env, argv[0], &stra))
        return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[1], &strb))
        return enif_make_badarg(env);

    result = u_strCompare((UChar*)stra.data, stra.size/2,
                          (UChar*)strb.data, strb.size/2,
                          FALSE);

    return enif_make_int(env, result);
}
ERL_NIF_TERM ustring_casecmp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary stra, strb;
    int32_t result;
    UErrorCode ec = U_ZERO_ERROR;

    if (!enif_inspect_binary(env, argv[0], &stra))
        return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[1], &strb))
        return enif_make_badarg(env);

    result = u_strCaseCompare((UChar*)stra.data, stra.size/2,
                              (UChar*)strb.data, strb.size/2,
                              U_FOLD_CASE_DEFAULT, &ec);

    if(U_FAILURE(ec))
        return error_tuple(env, u_errorName(ec));

    return enif_make_int(env, result);
}
ERL_NIF_TERM ustring_toupper(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary upper, in;
    int32_t size, used;
    UErrorCode ec = U_ZERO_ERROR;

    if (!enif_inspect_binary(env, argv[0], &in))
        return enif_make_badarg(env);
    size = in.size;

    do {
        if (!enif_alloc_binary(env, size, &upper))
            return error_tuple(env, "failed to alloc upper binary");

        used = u_strToUpper((UChar*)upper.data, upper.size/2,
                            (UChar*)in.data, in.size/2, NULL, &ec);
        if (ec == U_BUFFER_OVERFLOW_ERROR) {
            /* enlarge buffer if it was too small */
            enif_release_binary(env, &upper);
            size = used*2;
        }
    } while (ec == U_BUFFER_OVERFLOW_ERROR);

    if (U_FAILURE(ec)) {
        enif_release_binary(env, &upper);
        return error_tuple(env, u_errorName(ec));
    }

    if (used*2 != size) {
        /* shrink binary if it was too large */
        enif_realloc_binary(env, &upper, used*2);
    }

    return enif_make_binary(env, &upper);
}
ERL_NIF_TERM ustring_tolower(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary lower, in;
    int32_t size, used;
    UErrorCode ec = U_ZERO_ERROR;

    if (!enif_inspect_binary(env, argv[0], &in))
        return enif_make_badarg(env);
    size = in.size;

    do {
        if (!enif_alloc_binary(env, size, &lower))
            return error_tuple(env, "failed to alloc lower binary");

        used = u_strToLower((UChar*)lower.data, lower.size/2,
                            (UChar*)in.data, in.size/2, NULL, &ec);
        if (ec == U_BUFFER_OVERFLOW_ERROR) {
            /* enlarge buffer if it was too small */
            enif_release_binary(env, &lower);
            size = used*2;
        }
    } while (ec == U_BUFFER_OVERFLOW_ERROR);

    if (U_FAILURE(ec)) {
        enif_release_binary(env, &lower);
        return error_tuple(env, u_errorName(ec));
    }

    if (used*2 != size) {
        /* shrink binary if it was too large */
        enif_realloc_binary(env, &lower, used*2);
    }

    return enif_make_binary(env, &lower);
}
ERL_NIF_TERM ustring_length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;
    char type[10];
    int len;
    UBreakIterator* iter;
    UErrorCode ec = U_ZERO_ERROR;

    if(!enif_inspect_binary(env, argv[0], &bin))
        return enif_make_badarg(env);
    if(!enif_get_atom(env, argv[1], type, 10))
        return enif_make_badarg(env);

    switch(type[0]) {
    case 'c': /* codeunits */
        return enif_make_int(env, bin.size/2);
    case 'g': /* graphemes */
        iter = ubrk_open(UBRK_CHARACTER, NULL,
                         (UChar*)bin.data, bin.size/2, &ec);

        if(U_FAILURE(ec))
            return error_tuple(env, u_errorName(ec));

        len = 0;
        while(ubrk_next(iter) != UBRK_DONE)
            len++;

        return enif_make_int(env, len);
    default: /* unknown length type */
        return enif_make_badarg(env);
    }
}
static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

ERL_NIF_TERM error_tuple(ErlNifEnv* env, const char* msg) {
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_string(env, msg, ERL_NIF_LATIN1));
}

ERL_NIF_INIT(ustring, nif_funcs, &on_load, NULL, NULL, NULL)
