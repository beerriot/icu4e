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

-export([words/1, words/2]).

-on_load(init/0).

-include("icu4e.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% These should be exactly the same as their coutnerparts in ubrk.c
-define(OPTION_DEFAULT,      0).
-define(OPTION_SKIP_BREAKS,  1).
-define(OPTION_INCLUDE_TAGS, 2).

init() ->
    case code:priv_dir(icu4e) of
        {error, bad_name} ->
            SoName = filename:join("../priv", ubrk_nifs);
        Dir ->
            SoName = filename:join(Dir, ubrk_nifs)
    end,
    erlang:load_nif(SoName, 0).

%% @spec words(ustring()) -> [ustring()]|{error, term()}
%% @equiv words(String, []).
words(String) -> words(String, []).

%% @spec words(ustring(), [option()])
%%          -> [ustring()]|[{wordtype(), ustring()}]|{error, term()}
%% @type option() = include_tags|skip_breaks
%% @type wordtype() = break|number|letter|kana|ideo
%% @doc Break the string along word barriers, as defined by the
%%      UBRK_WORD break iterator.
%%
%%      Default return value is a list containing binaries, each binary
%%      being the run of characters between "word breaks".  The word
%%      breaks themselves will be included as binaries in the list.
%%
%%      Pass the 'skip_breaks' option to filter the breaks out of the
%%      result list.
%%
%%      Pass the 'include_tags' option to get a list of tuples instead
%%      of binaries.  The first element of the tuple will be an atom
%%      describing the tag assigned to the word by the break iterator
%%      (roughly the "type" of word), and the second element will be
%%      the binary of the word.
words(String, Options) ->
    IntOptions = lists:foldl(fun(skip_breaks, Acc) ->
                                     Acc bor ?OPTION_SKIP_BREAKS;
                                (include_tags, Acc) ->
                                     Acc bor ?OPTION_INCLUDE_TAGS
                             end,
                             ?OPTION_DEFAULT,
                             Options),
    words_internal(String, IntOptions).

%% @spec words_internal(ustring(), integer())
%%          -> [ustring()]|[{wordtype(), ustring()}]|{error, term()}
%% @doc nif placeholder for words/2
words_internal(_String, _Options) -> throw("NIF library not loaded").

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

%% Make sure 'skip_breaks' actually filters breaks out.
skip_breaks_test() ->
    AllWords = [<<"Mr">>,<<".">>,<<" ">>,<<"Livingston">>,<<",">>,<<" ">>,
                <<"I'd">>,<<" ">>,<<"presume">>,<<".">>],
    NonBreaks = [ W || W <- AllWords,
                       W /= <<".">>, W /= <<",">>, W /= <<" ">>],
    String = ustring:new(iolist_to_binary(AllWords), utf8),
    ?assertEqual(NonBreaks,
                 [ ?UCB(W, ustring:encoding(), utf8)
                   || W <- words(String, [skip_breaks]) ]).

%% Make sure 'include_tags' includes tags.
include_tags_test() ->
    Words = [{letter, <<"Please">>},{break, <<" ">>},
             {letter, <<"deposit">>},{break, <<" ">>},
             {break, <<"$">>},{number, <<"1.25">>},
             {break, <<" ">>},{letter, <<"to">>},
             {break, <<" ">>},{letter, <<"continue">>},{break, <<".">>}],
    String = ustring:new(iolist_to_binary([W || {_, W} <- Words]), utf8),
    ?assertEqual(Words,
                 [ {T, ?UCB(W, ustring:encoding(), utf8)}
                   || {T, W} <- words(String, [include_tags]) ]).

%% Make sure both options work together
both_options_test() ->
    Words = [{letter, <<"Please">>},{break, <<" ">>},
             {letter, <<"deposit">>},{break, <<" ">>},
             {break, <<"$">>},{number, <<"1.25">>},
             {break, <<" ">>},{letter, <<"to">>},
             {break, <<" ">>},{letter, <<"continue">>},{break, <<".">>}],
    String = ustring:new(iolist_to_binary([W || {_, W} <- Words]), utf8),
    ?assertEqual([ {T, W} || {T, W} <- Words, T /= break ],
                 [ {T, ?UCB(W, ustring:encoding(), utf8)}
                   || {T, W} <- words(String, [include_tags, skip_breaks]) ]).

-endif.
