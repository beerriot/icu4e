-module(ustring).

-export([new/1, new/2,
         encoding/0,
         cmp/2,
         casecmp/2,
         toupper/1,
         tolower/1]).

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

encoding() ->
    ?NATIVE.

new(String, Encoding) when is_binary(String); is_list(String) ->
    case Encoding of
        ?NATIVE ->
            new(String);
        _ ->
            new(?UCB(String, Encoding, ?NATIVE))
    end.

new(_String) -> throw("NIF library not loaded").
cmp(_String1, _String2) -> throw("NIF library not loaded").
casecmp(_String, _String2) -> throw("NIF library not loaded").
toupper(_String) -> throw("NIF library not loaded").
tolower(_String) -> throw("NIF library not loaded").

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    Str = <<"hello">>,
    UStr = new(Str, latin1),
    %% just make sure data came through in utf16
    ?assertEqual(?UCB(UStr, ?NATIVE, latin1), Str).

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

upper_test() ->
    UStr = toupper(new(<<"hello">>, latin1)),
    ?assertEqual(?UCB(UStr, ?NATIVE, latin1), <<"HELLO">>).

nonascii_upper_test() ->
    UStr = toupper(new(<<"ü">>, latin1)),
    ?assertEqual(?UCB(UStr, ?NATIVE, latin1), <<"Ü">>).

lower_test() ->
    UStr = tolower(new(<<"GOODBYE">>, latin1)),
    ?assertEqual(?UCB(UStr, ?NATIVE, latin1), <<"goodbye">>).

nonascii_lower_test() ->
    UStr = tolower(new(<<"Ä">>, latin1)),
    ?assertEqual(?UCB(UStr, ?NATIVE, latin1), <<"ä">>).

cmp_test() ->
    _apple = new(<<"apple">>, latin1),
    _banana = new(<<"banana">>, latin1),
    Apple = new(<<"Apple">>, latin1),
    ?assert(cmp(_apple, _banana) < 0),
    ?assert(cmp(_apple, _apple) == 0),
    ?assert(cmp(_banana, _apple) > 0),
    ?assert(cmp(Apple, _apple) < 0).

casecmp_test() ->
    ?assert(casecmp(new(<<"Apple">>, latin1),
                    new(<<"apple">>, latin1)) == 0),
    ?assert(casecmp(new(<<"Apple">>, latin1),
                    new(<<"banana">>, latin1)) < 0),
    ?assert(casecmp(new(<<"apple">>, latin1),
                    new(<<"Banana">>, latin1)) < 0).

encoding_test() ->
    ?assertEqual(encoding(), ?NATIVE).

-endif.
