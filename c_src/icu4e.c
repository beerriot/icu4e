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
 * General utility functions
 */
#include "icu4e.h"

/*
 * Reverse an Erlang list.
 */
ERL_NIF_TERM reverse_list(ErlNifEnv *env, ERL_NIF_TERM l) {
    ERL_NIF_TERM r = enif_make_list(env, 0);
    ERL_NIF_TERM h;

    while(enif_get_list_cell(env, l, &h, &l))
        r = enif_make_list_cell(env, h, r);
    
    return r;
}

/*
 * Wrap up the common error-tuple creation.  Creates an error tuple
 * of the form {error, msg::string()}.
 */
ERL_NIF_TERM error_tuple(ErlNifEnv* env, const char* msg) {
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_string(env, msg, ERL_NIF_LATIN1));
}
