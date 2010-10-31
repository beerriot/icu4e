%% @doc Expose unicode string manipulation from ICU.
%%
%%      The ustring module implements some unicode string manipuation
%%      functionality as NIF calls to the ICU library.
%%
%%      The library expects strings to be represented as Erlang
%%      binaries, encoded as UTF16 in this machine's native
%%      endianness.
%%      TODO: expose ?NATIVE as a compile-time variable
%%
%%      The new/2 function will convert to the native UTF16 format
%%      from many common encodings (e.g. latin1, utf8, etc.).  The
%%      new/2 function also normalizes the input string to the
%%      canonical unicode representation, as defined by the NFC
%%      algorithm in "Unicode Standard Annex #15: Unicode
%%      Normalization Forms"
%%      http://www.unicode.org/unicode/reports/tr15/
%%
%%      This module assumes that binaries are whole strings, rather
%%      than expecting them to be null-terminated.
-module(ustring).

-export([new/2,
         encoding/0,
         cmp/2,
         casecmp/2,
         toupper/1,
         tolower/1,
         length/2]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(NATIVE, {utf16, little}).
-define(UCB, unicode:characters_to_binary).

init() ->
    case code:priv_dir(icu4e) of
        {error, bad_name} ->
            SoName = filename:join("../priv", ustring_nifs);
        Dir ->
            SoName = filename:join(Dir, ustring_nifs)
    end,
    erlang:load_nif(SoName, 0).

%% @spec encoding() -> {utf16, little|big}
%% @doc Get the native encoding format - UTF16, either 'big' or
%%      'little' endian.
%%
%%      This is useful for transcoding ustring output to other
%%      formats:
%%      ```
%%      unicode:characters_to_binary(UstringOutput,
%%                                   ustring:encoding(),
%%                                   utf8)
%%      '''
encoding() ->
    ?NATIVE.

%% @spec new(binary(), encoding()) -> ustring()|{error, term()}
%% @type encoding() = latin1 | unicode | utf8 | utf16 | utf32 |
%%                    {utf16,little} | {utf16,big} |
%%                    {utf32,little} | {utf32,big}
%% @type ustring() = binary()
%% @doc Create a new ustring-compatible string.  The input String will
%%      be re-encoded as the native UTF16 format, and its characters
%%      will be normalized (NFC).
%%
%%      The input Encoding may be any atom understood by the 'unicode'
%%      Erlang module.
new(String, Encoding) when is_binary(String); is_list(String) ->
    case Encoding of
        ?NATIVE ->
            new(String);
        _ ->
            new(?UCB(String, Encoding, ?NATIVE))
    end.

%% @spec new(ustring()) -> ustring()|{error, term()}
%% @doc Handle the normalization step of new/2.
new(_String) -> throw("NIF library not loaded").

%% @spec cmp(ustring(), ustring()) -> integer()|{error, term()}
%% @doc Compare the two strings for bit-wise equality.  Returns 0 if
%%      the strings are equal, less than 0 if String1 sorts before
%%      String2, or greater than 0 if String2 sorts before String1.
cmp(_String1, _String2) -> throw("NIF library not loaded").

%% @spec casecmp(ustring(), ustring()) -> integer()|{error, term()}
%% @doc Compare the two strings for case-insensitive equality.
%%      Returns 0 if the strings are equal, less than 0 if String1
%%      sorts before String2, or greater than 0 if String2 sorts
%%      before String1.
casecmp(_String, _String2) -> throw("NIF library not loaded").

%% @spec toupper(ustring()) -> ustring()|{error, term()}
%% @doc Convert all characters in the input string to their upper-case
%%      equivalents.
toupper(_String) -> throw("NIF library not loaded").

%% @spec tolower(ustring()) -> ustring()|{error, term()}
%% @doc Convert all characters in the input string to their lower-case
%%      equivalents.
tolower(_String) -> throw("NIF library not loaded").

%% @spec length(ustring(), LengthType::length_type())
%%          -> integer()|{error, term()}
%% @type length_type() = codeunits|graphemes

%% @doc Find the length of the given string.  If LengthType is the
%%      atom 'codeunits', then the length returned is the number of
%%      16-bit elements in the string.  If LengthType is the atom
%%      'graphemes', then the length returned is the number of
%%      user-visible characters.
length(String, codeunits) when is_binary(String) ->
    size(String) div 2;
length(_String, graphemes) ->
    throw("NIF library not loaded").

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

%% Make sure that basic string construction is working
basic_test() ->
    Str = <<"hello">>,
    UStr = new(Str, latin1),
    %% just make sure data came through in utf16
    ?assertEqual(?UCB(UStr, ?NATIVE, latin1), Str).

%% Make sure that new/2 is normalizing inputs correctly
norm_test() ->
    %% from unorm.h documentation
    Str1 = [16#00c1],          % Latin Capital A With Acute
    Str2 = [16#0041, 16#0301], % Latin Captial A, Combining Acute Accent
    UStr1 = new(Str1, utf16),
    UStr2 = new(Str2, utf16),
    %% Str1 should be canonical - renormalization shouldn't change it
    ?assertEqual(UStr1, ?UCB(Str1, utf32, ?NATIVE)),
    %% Str2 should canonicalize to Str1
    ?assertEqual(UStr2, UStr1).

%% Check that toupper/1 up-cases basic ASCII
upper_test() ->
    UStr = toupper(new(<<"hello">>, latin1)),
    ?assertEqual(?UCB(UStr, ?NATIVE, latin1), <<"HELLO">>).

%% Check that toupper/1 up-cases basic non-ASCII Latin1
nonascii_upper_test() ->
    UStr = toupper(new(<<"ü">>, latin1)),
    ?assertEqual(?UCB(UStr, ?NATIVE, latin1), <<"Ü">>).

%% Check that tolower/1 down-cases basic ASCII
lower_test() ->
    UStr = tolower(new(<<"GOODBYE">>, latin1)),
    ?assertEqual(?UCB(UStr, ?NATIVE, latin1), <<"goodbye">>).

%% Check that tolower/1 down-cases basic non-ASCII Latin1
nonascii_lower_test() ->
    UStr = tolower(new(<<"Ä">>, latin1)),
    ?assertEqual(?UCB(UStr, ?NATIVE, latin1), <<"ä">>).

%% Make sure cmp/2 gets basic ASCII sorting correct
cmp_test() ->
    _apple = new(<<"apple">>, latin1),
    _banana = new(<<"banana">>, latin1),
    Apple = new(<<"Apple">>, latin1),
    ?assert(cmp(_apple, _banana) < 0),
    ?assert(cmp(_apple, _apple) == 0),
    ?assert(cmp(_banana, _apple) > 0),
    ?assert(cmp(Apple, _apple) < 0).

%% Make sure the cascmp/2 gets basic case-insensitive sorting correct
casecmp_test() ->
    ?assert(casecmp(new(<<"Apple">>, latin1),
                    new(<<"apple">>, latin1)) == 0),
    ?assert(casecmp(new(<<"Apple">>, latin1),
                    new(<<"banana">>, latin1)) < 0),
    ?assert(casecmp(new(<<"apple">>, latin1),
                    new(<<"Banana">>, latin1)) < 0).

%% Make sure encoding/0 isn't lying
encoding_test() ->
    ?assertEqual(encoding(), ?NATIVE).

%% Check the different types of length against each other
length_test() ->
    %% these strings have different numbers of 16-bit codes,
    %% but the same number of user-visible characters

    %% have to specify literally, becuase new/2 will normalize
    %% these strings to the same string
    %% {Latin Capital A With Acute,
    %%  Latin Captial A, Combining Acute Accent}
    {Str1, Str2} = case ?NATIVE of
                       {utf16,little} ->
                           {<<16#c1, 16#00>>,
                            <<16#41, 16#00, 16#01, 16#03>>};
                       {utf16,big} ->
                           {<<16#00, 16#c1>>,
                            <<16#00, 16#41, 16#03, 16#01>>}
                   end,

    %% count 16-bit codes
    ?assertEqual(1, ?MODULE:length(Str1, codeunits)),
    ?assertEqual(2, ?MODULE:length(Str2, codeunits)),

    %% count user-visible characters
    ?assertEqual(1, ?MODULE:length(Str1, graphemes)),
    ?assertEqual(1, ?MODULE:length(Str2, graphemes)).

-endif.
