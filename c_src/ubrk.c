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
 * NIF implementations for ubrk.erl
 *
 * All binaries passed to these functions should be UTF16-encoded,
 * in the native endian.
 */
#include "erl_nif_compat.h"
#include "icu4e.h"
#include "unicode/utypes.h"
#include "unicode/ustring.h"
#include "unicode/ubrk.h"

/* These should be exactly the same as their coutnerparts in ubrk.erl */
#define OPTION_DEFAULT      0
#define OPTION_SKIP_BREAKS  1
#define OPTION_INCLUDE_TAGS 2

/* Atoms (initialized in on_load) */
static ERL_NIF_TERM ATOM_BREAK;
static ERL_NIF_TERM ATOM_NUMBER;
static ERL_NIF_TERM ATOM_LETTER;
static ERL_NIF_TERM ATOM_KANA;
static ERL_NIF_TERM ATOM_IDEO;

/* Prototypes */
ERL_NIF_TERM ubrk_words_internal(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[]);
ERL_NIF_TERM atom_for_tag(ErlNifEnv* env, int status);

static ErlNifFunc nif_funcs[] =
{
    {"words_internal", 2, ubrk_words_internal}
};

/*
 * Split the input string on word boundaries, according to the
 * UBRK_WORD break iterator.
 * Inputs:
 *   0: binary(), the string to split
 *   1: int(), return value options
 * Outputs (one of):
 *   [binary()]|[{atom(), binary()}], the words, broken up
 *                                    possibly including tags
 *   {'error', Reason::string()}, something went wrong
 */
ERL_NIF_TERM ubrk_words_internal(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[]) {
    ErlNifBinary str;
    int options;
    ERL_NIF_TERM l, b;
    UBreakIterator* iter;
    int status, brk, lastbrk = 0;
    UErrorCode ec = U_ZERO_ERROR;

    if(!enif_inspect_binary(env, argv[0], &str))
        return enif_make_badarg(env);
    if(!enif_get_int(env, argv[1], &options))
        return enif_make_badarg(env);

    iter = ubrk_open(UBRK_WORD, NULL,
                     (UChar*)str.data, str.size/2, &ec);
    if(U_FAILURE(ec))
        return error_tuple(env, u_errorName(ec));

    l = enif_make_list(env, 0);
    while((brk=ubrk_next(iter)) != UBRK_DONE) {
        status = ubrk_getRuleStatus(iter);
        if(! ((options & OPTION_SKIP_BREAKS) &&
              (status >= UBRK_WORD_NONE
               && status < UBRK_WORD_NONE_LIMIT)) ) {
            b = enif_make_sub_binary(env, argv[0],
                                     lastbrk*2,
                                     (brk-lastbrk)*2);
            if(options & OPTION_INCLUDE_TAGS) {
                b = enif_make_tuple2(env, atom_for_tag(env, status), b);
            }

            l = enif_make_list_cell(env, b, l);
        }
        lastbrk = brk;
    }

    ubrk_close(iter);
    return reverse_list(env, l);
}

ERL_NIF_TERM atom_for_tag(ErlNifEnv* env, int status) {
    if(status >= UBRK_WORD_NONE &&
       status < UBRK_WORD_NONE_LIMIT)
        return ATOM_BREAK;
    else if(status >= UBRK_WORD_NUMBER &&
            status < UBRK_WORD_NUMBER_LIMIT)
        return ATOM_NUMBER;
    else if(status >= UBRK_WORD_LETTER &&
            status < UBRK_WORD_LETTER_LIMIT)
        return ATOM_LETTER;
    else if(status >= UBRK_WORD_KANA &&
            status < UBRK_WORD_KANA_LIMIT)
        return ATOM_KANA;
    else if(status >= UBRK_WORD_IDEO &&
            status < UBRK_WORD_IDEO_LIMIT)
        return ATOM_IDEO;
    else
        return enif_make_atom(env, "error");
}

/*
 * Initialization of this library.  Does nothing at the moment.
 */
static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ATOM_BREAK = enif_make_atom(env, "break");
    ATOM_NUMBER = enif_make_atom(env, "number");
    ATOM_LETTER = enif_make_atom(env, "letter");
    ATOM_KANA = enif_make_atom(env, "kana");
    ATOM_IDEO = enif_make_atom(env, "ideo");
    return 0;
}

ERL_NIF_INIT(ubrk, nif_funcs, &on_load, NULL, NULL, NULL)
