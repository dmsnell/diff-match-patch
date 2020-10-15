-module(dmp).
-author("Dennis Snell <dennis.snell@automattic.com>").

-include_lib("eunit/include/eunit.hrl").

-define(S(X), <<X/utf16>>).

%% Interface functions

%% Internal functions

-spec common_prefix(Left :: binary(), Right :: binary()) -> non_neg_integer().

common_prefix(Left, Right) ->
    cp_surrogate_shift(
        binary:longest_common_prefix([Left, Right]) div 2,
        char_type(suffix(Left, 2))
    ).

cp_surrogate_shift(CodeUnits, high_surrogate) -> CodeUnits - 1;
cp_surrogate_shift(CodeUnits, _)              -> CodeUnits.

-spec common_suffix(Left :: binary(), Right :: binary()) -> non_neg_integer().

common_suffix(Left, Right) ->
    cs_surrogate_shift(
        binary:longest_common_suffix([Left, Right]) div 2,
        char_type(prefix(Left, 2))
    ).

cs_surrogate_shift(CodeUnits, low_surrogate) -> CodeUnits + 1;
cs_surrogate_shift(CodeUnits, _)             -> CodeUnits.

-spec common_overlap(Left :: binary(), Right :: binary()) -> non_neg_integer().

common_overlap(Left, Right) ->
    common_overlap(Left, Right, 0, 2) div 2.

common_overlap(Left, Right, Best, Length)
    when byte_size(Left) =< Length; byte_size(Right) =< Length ->
    Best;
common_overlap(Left, Right, Best, Length) ->
    Suffix = suffix(Left, Length),
    co_scan(Left, Right, Best, Length, binary:match(Right, [Suffix])).

co_scan(_Left, _Right, Best, _Length, nomatch) ->
    Best;
co_scan(Left, Right, _Best, Length, {0, Length}) ->
    common_overlap(Left, Right, Length, Length + 2);
co_scan(Left, Right, Best, Length, {P, Length}) ->
    co_scan(Left, Right, Best, Length, {P, suffix(Left, Length + P), prefix(Right, Length + P)});
co_scan(Left, Right, _Best, Length, {P, Suffix, Prefix}) when Suffix == Prefix ->
    common_overlap(Left, Right, Length + P, Length + P + 2);
co_scan(Left, Right, Best, Length, {P, _Suffix, _Prefix}) ->
    common_overlap(Left, Right, Best, Length + P).

prefix(Binary, Bytes) -> binary:part(Binary, 0, Bytes).
suffix(Binary, Bytes) -> binary:part(Binary, byte_size(Binary), -Bytes).

char_type(<<A:16>>) when A >= 16#D800, A =< 16#DBFF -> high_surrogate;
char_type(<<A:16>>) when A >= 16#DC00, A =< 16#DFFF -> low_surrogate;
char_type(<<_:16>>)                                 -> normal.

%% EUnit tests
-ifdef(EUNIT).

common_prefix_test_() -> [
    {"Null case", ?_assertEqual(0, common_prefix(?S("abc"), ?S("xyz")))},
    {"Non-null case", ?_assertEqual(4, common_prefix(?S("1234abcdef"), ?S("1234xyz")))},
    {"Whole case", ?_assertEqual(4, common_prefix(?S("1234"), ?S("1234xyz")))},
    {"Surrogate half", ?_assertEqual(2, common_prefix(?S("🅰🅰"), ?S("🅰🅱")))}
].

common_suffix_test_() -> [
    {"Null case", ?_assertEqual(0, common_suffix(?S("abc"), ?S("xyz")))},
    {"Non-null case", ?_assertEqual(4, common_suffix(?S("abcdef1234"), ?S("xyz1234")))},
    {"Whole case", ?_assertEqual(4, common_suffix(?S("1234"), ?S("xyz1234")))},
    {"Surrogate half", ?_assertEqual(2, common_suffix(?S("🅰🅰"), ?S("🕰🅰")))}
].

common_overlap_test_() -> [
    {"Null case", ?_assertEqual(0, common_overlap(?S(""), ?S("abcd")))},
    {"Whole case", ?_assertEqual(3, common_overlap(?S("abc"), ?S("abcd")))},
    {"No overlap", ?_assertEqual(0, common_overlap(?S("123456"), ?S("abcde")))},
    {"Overlap", ?_assertEqual(3, common_overlap(?S("123456xxx"), ?S("xxxabcd")))},
    {"Unicode", ?_assertEqual(0, common_overlap(?S("fi"), ?S("ﬁi")))}
].

-endif.