-module(dmp).
-author("Dennis Snell <dennis.snell@automattic.com>").

-include_lib("eunit/include/eunit.hrl").

-define(S(X), <<X/utf16>>).
-record(half_match, {
    l_prefix :: binary(),
    l_suffix :: binary(),
    r_prefix :: binary(),
    r_suffix :: binary(),
    m_common :: binary()
}).

%% Interface functions

%% Internal functions

-spec half_match(Left :: binary(), Right :: binary()) -> #half_match{} | nomatch.

half_match(Left, Right) ->
    ok.

-spec common_prefix(Left :: binary(), Right :: binary()) -> non_neg_integer().

common_prefix(Left, Right) ->
    CodeUnits = binary:longest_common_prefix([Left, Right]) div 2,
    cp_surrogate_shift(CodeUnits, char_type(suffix(prefix(Left, CodeUnits * 2), 2))).

cp_surrogate_shift(CodeUnits, high_surrogate) -> CodeUnits - 1;
cp_surrogate_shift(CodeUnits, _)              -> CodeUnits.

-spec common_suffix(Left :: binary(), Right :: binary()) -> non_neg_integer().

common_suffix(Left, Right) ->
    CodeUnits = binary:longest_common_suffix([Left, Right]) div 2,
    cs_surrogate_shift(CodeUnits, char_type(prefix(suffix(Left, CodeUnits * 2), 2))).

cs_surrogate_shift(CodeUnits, low_surrogate) -> CodeUnits - 1;
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

prefix(_Binary, 0) -> <<"">>;
prefix(Binary, Bytes) when byte_size(Binary) =< Bytes -> Binary;
prefix(Binary, Bytes) -> binary:part(Binary, 0, Bytes).

suffix(_Binary, 0) -> <<"">>;
suffix(Binary, Bytes) when byte_size(Binary) =< Bytes -> Binary;
suffix(Binary, Bytes) -> binary:part(Binary, byte_size(Binary), -Bytes).

char_type(<<A:16>>) when A >= 16#D800, A =< 16#DBFF -> high_surrogate;
char_type(<<A:16>>) when A >= 16#DC00, A =< 16#DFFF -> low_surrogate;
char_type(_)                                        -> normal.

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

half_match_test_() -> [
    {"No match", [
        ?_assertEqual(nomatch, half_match(?S("1234567890"), ?S("abcdef"))),
        ?_assertEqual(nomatch, half_match(?S("12345"), ?S("23")))
    ]},
    {"Single match", [
        ?_assertEqual(#half_match{
            l_prefix = ?S("12"),
            l_suffix = ?S("90"),
            r_prefix = ?S("a"),
            r_suffix = ?S("z"),
            m_common = ?S("345678")
        }, half_match(?S("1234567890"), ?S("a345678z"))),
        ?_assertEqual(#half_match{
            l_prefix = ?S("a"),
            l_suffix = ?S("z"),
            r_prefix = ?S("12"),
            r_suffix = ?S("90"),
            m_common = ?S("345678")
        }, half_match(?S("a23456789z"), ?S("1234567890"))),
        ?_assertEqual(#half_match{
            l_prefix = ?S("abc"),
            l_suffix = ?S("z"),
            r_prefix = ?S("1234"),
            r_suffix = ?S("0"),
            m_common = ?S("56789")
        }, half_match(?S("abc56789z"), ?S("1234567890"))),
        ?_assertEqual(#half_match{
            l_prefix = ?S("a"),
            l_suffix = ?S("xyz"),
            r_prefix = ?S("1"),
            r_suffix = ?S("7890"),
            m_common = ?S("23456")
        }, half_match(?S("a23456xyz"), ?S("1234567890")))
    ]},
    {"Multiple matches", [
        ?_assertEqual(#half_match{
            l_prefix = ?S("12123"),
            l_suffix = ?S("123121"),
            r_prefix = ?S("a"),
            r_suffix = ?S("z"),
            m_common = ?S("1234123451234")
        }, half_match(?S("121231234123451234123121"), ?S("a1234123451234z"))),
        ?_assertEqual(#half_match{
            l_prefix = ?S(""),
            l_suffix = ?S("-=-=-=-=-="),
            r_prefix = ?S("x"),
            r_suffix = ?S(""),
            m_common = ?S("x-=-=-=-=-=-=-=")
        }, half_match(?S("x-=-=-=-=-=-=-=-=-=-=-=-="), ?S("xx-=-=-=-=-=-=-="))),
        ?_assertEqual(#half_match{
            l_prefix = ?S("-=-=-=-=-="),
            l_suffix = ?S(""),
            r_prefix = ?S(""),
            r_suffix = ?S("y"),
            m_common = ?S("-=-=-=-=-=-=-=y")
        }, half_match(?S("-=-=-=-=-=-=-=-=-=-=-=-=y"), ?S("-=-=-=-=-=-=-=yy")))
    ]},
    % Optimal diff would be -q+x=H-i+e=lloHe+Hu=llo-Hew+y not -qHillo+x=HelloHe-w+Hulloy
    {"Non-optimal match", ?_assertEqual(#half_match{
        l_prefix = ?S("qHillo"),
        l_suffix = ?S("w"),
        r_prefix = ?S("x"),
        r_suffix = ?S("Hulloy"),
        m_common = ?S("HelloHe")
    }, half_match(?S("qHilloHelloHew"), ?S("xHelloHeHulloy")))}
].

-endif.