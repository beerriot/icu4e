/* -------------------------------------------------------------------
 *
 * icu4e: Erlang NIF wrappers for ICU
 *
 * Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
 *
 * This file is provided to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License.  You may obtain
 * a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 * -------------------------------------------------------------------
 */

/*
 * NIF implementations for ustring.erl
 *
 * All binaries passed to these functions should be UTF16-encoded,
 * in the native endian.
 */
#include "erl_nif_compat.h"
#include "icu4e.h"
#include "unicode/utypes.h"
#include "unicode/uchar.h"
#include "unicode/ustring.h"
#include "unicode/unorm.h"
#include "unicode/ubrk.h"

/* Atoms (initialized in on_load) */
static ERL_NIF_TERM ATOM_ENDIAN;

/* Prototypes */
ERL_NIF_TERM ustring_endian(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_new(ErlNifEnv* env, int argc,
                         const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_cmp(ErlNifEnv* env, int argc,
                         const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_casecmp(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_toupper(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_tolower(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ustring_length(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"endian", 0, ustring_endian},
    {"new", 1, ustring_new},
    {"cmp", 2, ustring_cmp},
    {"casecmp", 2, ustring_casecmp},
    {"toupper", 1, ustring_toupper},
    {"tolower", 1, ustring_tolower},
    {"length", 2, ustring_length}
};

/*
 * Return the endian atom, as derived in on_load
 * Inputs: none
 * Outputs:
 *  atom(), either 'little' or 'big' to represent the native endian
 */
ERL_NIF_TERM ustring_endian(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[]) {
    return ATOM_ENDIAN;
}

/*
 * Perform NFC normalization on the input string.
 * Inputs:
 *   0: binary(), the string to be normalized
 * Outputs (one of):
 *   binary(), normalized string
 *   {'error', Reason::string()}, something went wrong
 */
ERL_NIF_TERM ustring_new(ErlNifEnv* env, int argc,
                         const ERL_NIF_TERM argv[]) {
    ErlNifBinary norm, in;
    int32_t size, used;
    UErrorCode ec = U_ZERO_ERROR;

    if(!enif_inspect_binary(env, argv[0], &in))
        return enif_make_badarg(env);
    size = in.size;

    do {
        if(!enif_alloc_binary_compat(env, size, &norm))
            return error_tuple(env, "failed to alloc normalized binary");

        used = unorm_normalize((UChar*)in.data, in.size/2, UNORM_NFC, 0,
                               (UChar*)norm.data, norm.size/2, &ec);
        if (ec == U_BUFFER_OVERFLOW_ERROR) {
            /* enlarge buffer if it was too small */
            enif_release_binary_compat(env, &norm);
            size = used*2;
        }
    } while (ec == U_BUFFER_OVERFLOW_ERROR);

    if (U_FAILURE(ec)) {
        enif_release_binary_compat(env, &norm);
        return error_tuple(env, u_errorName(ec));
    }

    if (used*2 != size) {
        /* shrink binary if it was too large */
        enif_realloc_binary_compat(env, &norm, used*2);
    }

    return enif_make_binary(env, &norm);
}

/*
 * Compare two strings for bitwise equality. Non-equality sort
 * ordering is done in code unit order.
 * Inputs:
 *   0: binary(), first string to be compared
 *   1: binary(), second string to be compared
 * Outputs (one of):
 *   integer(), 0 if equal,
 *              <0 if argv[0] < argv[1],
 *              >0 if argv[0] > argv[1]
 *   {'error', Reason::string()}, something went wrong
 */
ERL_NIF_TERM ustring_cmp(ErlNifEnv* env, int argc,
                         const ERL_NIF_TERM argv[]) {
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

/*
 * Compare two strings for case-insensitive equality. Non-equality
 * sort ordering is done in code unit order.
 * Inputs:
 *   0: binary(), first string to be compared
 *   1: binary(), second string to be compared
 * Outputs (one of):
 *   integer(), 0 if equal,
 *              <0 if argv[0] < argv[1],
 *              >0 if argv[0] > argv[1]
 *   {'error', Reason::string()}, something went wrong
 */
ERL_NIF_TERM ustring_casecmp(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]) {
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

/*
 * Convert the characters in a string to their upper-case
 * representations.
 * Inputs:
 *   0: binary(), string to be up-cased
 * Outputs (one of):
 *   binary(), the upper-case version of the input string
 *   {'error', Reason::string()}, something went wrong
 */
ERL_NIF_TERM ustring_toupper(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]) {
    ErlNifBinary upper, in;
    int32_t size, used;
    UErrorCode ec = U_ZERO_ERROR;

    if (!enif_inspect_binary(env, argv[0], &in))
        return enif_make_badarg(env);
    size = in.size;

    do {
        if (!enif_alloc_binary_compat(env, size, &upper))
            return error_tuple(env, "failed to alloc upper binary");

        used = u_strToUpper((UChar*)upper.data, upper.size/2,
                            (UChar*)in.data, in.size/2, NULL, &ec);
        if (ec == U_BUFFER_OVERFLOW_ERROR) {
            /* enlarge buffer if it was too small */
            enif_release_binary_compat(env, &upper);
            size = used*2;
        }
    } while (ec == U_BUFFER_OVERFLOW_ERROR);

    if (U_FAILURE(ec)) {
        enif_release_binary_compat(env, &upper);
        return error_tuple(env, u_errorName(ec));
    }

    if (used*2 != size) {
        /* shrink binary if it was too large */
        enif_realloc_binary_compat(env, &upper, used*2);
    }

    return enif_make_binary(env, &upper);
}

/*
 * Convert the characters in a string to their lowerr-case
 * representations.
 * Inputs:
 *   0: binary(), string to be down-cased
 * Outputs (one of):
 *   binary(), the lower-case version of the input string
 *   {'error', Reason::string()}, something went wrong
 */
ERL_NIF_TERM ustring_tolower(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]) {
    ErlNifBinary lower, in;
    int32_t size, used;
    UErrorCode ec = U_ZERO_ERROR;

    if (!enif_inspect_binary(env, argv[0], &in))
        return enif_make_badarg(env);
    size = in.size;

    do {
        if (!enif_alloc_binary_compat(env, size, &lower))
            return error_tuple(env, "failed to alloc lower binary");

        used = u_strToLower((UChar*)lower.data, lower.size/2,
                            (UChar*)in.data, in.size/2, NULL, &ec);
        if (ec == U_BUFFER_OVERFLOW_ERROR) {
            /* enlarge buffer if it was too small */
            enif_release_binary_compat(env, &lower);
            size = used*2;
        }
    } while (ec == U_BUFFER_OVERFLOW_ERROR);

    if (U_FAILURE(ec)) {
        enif_release_binary_compat(env, &lower);
        return error_tuple(env, u_errorName(ec));
    }

    if (used*2 != size) {
        /* shrink binary if it was too large */
        enif_realloc_binary_compat(env, &lower, used*2);
    }

    return enif_make_binary(env, &lower);
}

/*
 * Find the length of the input string.  Length is computed as the
 * number of 16-bit characters if the "length type" parameter is the
 * atom 'codeunits'.  Length is computed as the number of user-visible
 * characters if the "length type" parameter is the atom 'graphemes'.
 * Inputs:
 *   0: binary(), the string to find the length of
 *   1: atom(), either 'codeunits' or 'graphemes'
 * Outputs (one of):
 *   integer(), the length of the string
 *   {'error', Reason::string()}, something went wrong
 */
ERL_NIF_TERM ustring_length(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;
    char type[10];
    int len;
    UBreakIterator* iter;
    UErrorCode ec = U_ZERO_ERROR;

    if(!enif_inspect_binary(env, argv[0], &bin))
        return enif_make_badarg(env);
    if(!enif_get_atom_compat(env, argv[1], type, 10, ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    switch(type[0]) {
    case 'c': /* codeunits */
        /* this library assumes that binaries are whole-strings,
           not null-terminated strings */
        return enif_make_int(env, bin.size/2);
    case 'g': /* graphemes */
        iter = ubrk_open(UBRK_CHARACTER, NULL,
                         (UChar*)bin.data, bin.size/2, &ec);

        if(U_FAILURE(ec))
            return error_tuple(env, u_errorName(ec));

        len = 0;
        while(ubrk_next(iter) != UBRK_DONE)
            len++;

        ubrk_close(iter);
        return enif_make_int(env, len);
    default: /* unknown length type */
        return enif_make_badarg(env);
    }
}

/*
 * Initialization of this library.  Does nothing at the moment.
 */
static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
#if U_IS_BIG_ENDIAN
    ATOM_ENDIAN = enif_make_atom(env, "big");
#else
    ATOM_ENDIAN = enif_make_atom(env, "little");
#endif
    return 0;
}

ERL_NIF_INIT(ustring, nif_funcs, &on_load, NULL, NULL, NULL)
