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
#include "erl_nif_compat.h"

#ifndef ICU4E_COMMON
ERL_NIF_TERM reverse_list(ErlNifEnv *env, ERL_NIF_TERM l);
ERL_NIF_TERM error_tuple(ErlNifEnv* env, const char* msg);
#define ICU4E_COMMON 1
#endif
