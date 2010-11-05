%% -------------------------------------------------------------------
%%
%% icu4e: Erlang NIF wrappers for ICU
%%
%% Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Expose unicode break iterators from ICU.
%%
%%      The ubrk module implements some unicode string tokenizing
%%      functionality as NIF calls to the ICU library.
%%
%%      The library expects strings to be represented as Erlang
%%      binaries, encoded as UTF16 in this machine's native
%%      endianness.  (Hint: use the 'ustring' module to create these
%%      binaries.)
-module(ubrk).

-export([words/1]).

-on_load(init/0).

-include("icu4e.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    case code:priv_dir(icu4e) of
        {error, bad_name} ->
            SoName = filename:join("../priv", ubrk_nifs);
        Dir ->
            SoName = filename:join(Dir, ubrk_nifs)
    end,
    erlang:load_nif(SoName, 0).

%% @spec words(ustring()) -> [ustring()]|{error, term()}
%% @doc Break the string along word barriers, as defined by the
%%      UBRK_WORD break iterator.
words(_String) -> throw("NIF library not loaded").

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

%% Make sure basic whitespace separation works
words_basic_test() ->
    Words = [<<"Does">>,<<" ">>,<<"this">>,<<" ">>,<<"work">>,<<"?">>],
    compare_words(Words).

%% Make sure number aggregation works ("1.25", not "1",".","2","5")
words_numbers_test() ->
    Words = [<<"Please">>,<<" ">>,<<"deposit">>,<<" ">>,
             <<"$">>,<<"1.25">>,<<" ">>,<<"to">>,<<" ">>,
             <<"continue">>,<<".">>],
    compare_words(Words).

%% Document punctuation brakes
words_punctuation_test() ->
    Words = [<<"Mr">>,<<".">>,<<" ">>,<<"Livingston">>,<<",">>,<<" ">>,
             <<"I'd">>,<<" ">>,<<"presume">>,<<".">>],
    compare_words(Words).

compare_words(Words) ->
    String = ustring:new(iolist_to_binary(Words), utf8),
    ?assertEqual(Words,
                 [ ?UCB(W, ustring:encoding(), utf8)
                   || W <- words(String) ]).

-endif.
