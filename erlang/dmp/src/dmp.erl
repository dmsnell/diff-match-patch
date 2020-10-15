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

-record(line_hashes, {
    left   :: binary(),
    right  :: binary(),
    lookup :: tuple()
}).

%% Interface functions

%% Internal functions

-spec lines_to_chars(Left :: binary(), Right :: binary()) -> #line_hashes{}.

lines_to_chars(Left, Right) ->
    {Left1, Lookup1, Hash1} = ltc_munge(
        Left,
        [],
        [?S("")],
        #{?S("") => <<0:16>>},
        40000,
        0,
        binary:match(Left, [?S("\n")])
    ),
    {Right1, Lookup2, _Hash2} = ltc_munge(
        Right,
        [],
        Lookup1,
        Hash1,
        65535,
        0,
        binary:match(Right, [?S("\n")])
    ),
    #line_hashes{
        left   = Left1,
        right  = Right1,
        lookup = list_to_tuple(lists:reverse(Lookup2))
    }.

ltc_munge(<<"">>, Chars, Lookup, Hash, _Max, _I, _) ->
    {list_to_binary(lists:reverse(Chars)), Lookup, Hash};
ltc_munge(Text, Chars, Lookup, Hash, _Max, I, _) when byte_size(Text) == I ->
    {list_to_binary(lists:reverse(Chars)), Lookup, Hash};
ltc_munge(Text, Chars, Lookup, Hash, _Max, I, nomatch) ->
    Line = binary:part(Text, I, byte_size(Text) - I),
    Next = <<(length(Lookup)):16>>,
    case maps:get(Line, Hash, missing) of
        missing -> {list_to_binary(lists:reverse([Next | Chars])), [Line | Lookup], maps:put(Line, Next, Hash)};
        Char    -> {list_to_binary(lists:reverse([Char | Chars])), Lookup, Hash}
    end;
ltc_munge(Text, Chars, Lookup, Hash, Max, I, _) when length(Lookup) == Max - 1 ->
    Line = binary:part(Text, I, byte_size(Text) - I),
    Next = <<(length(Lookup)):16>>,
    case maps:get(Line, Hash, missing) of
        missing -> {list_to_binary(lists:reverse([Next | Chars])), [Line | Lookup], maps:put(Line, Next, Hash)};
        Char    -> {list_to_binary(lists:reverse([Char | Chars])), Lookup, Hash}
    end;
ltc_munge(Text, Chars, Lookup, Hash, Max, I, {P, _L}) ->
    Line = binary:part(Text, I, P - I + 2),
    Next = <<(length(Lookup)):16>>,
    case maps:get(Line, Hash, missing) of
        missing -> ltc_munge(
            Text,
            [Next | Chars],
            [Line | Lookup],
            maps:put(Line, Next, Hash),
            Max,
            P + 2,
            binary:match(Text, [?S("\n")], [{scope, {P + 2, byte_size(Text) - (P + 2)}}])
        );
        Char -> ltc_munge(
            Text,
            [Char | Chars],
            Lookup,
            Hash,
            Max,
            P + 2,
            binary:match(Text, [?S("\n")], [{scope, {P + 2, byte_size(Text) - (P + 2)}}])
        )
    end.

-spec half_match(Left :: binary(), Right :: binary()) -> #half_match{} | nomatch.

half_match(Left, Right) when byte_size(Left) > byte_size(Right) ->
    hm_prepare(left, Left, Right);
half_match(Left, Right) ->
    hm_prepare(right, Right, Left).

hm_prepare(_, Long, Short) when byte_size(Long) < 8; byte_size(Short) * 2 < byte_size(Long) ->
    nomatch;
hm_prepare(Longer, Long, Short) ->
    Length = byte_size(Long),
    hm_join(
        Longer,
        hm_split(Long, Short, ceil(Length / 2 / 4)),
        hm_split(Long, Short, ceil(Length / 2 / 2))
    ).

hm_split(Long, Short, I) ->
    Seed = binary:part(Long, I * 2, floor(byte_size(Long) / 2 / 4)),
    Best = #half_match{
        l_prefix = <<"">>,
        l_suffix = <<"">>,
        r_prefix = <<"">>,
        r_suffix = <<"">>,
        m_common = <<"">>
    },
    hm_split(Long, Short, I, Seed, Best, binary:match(Short, [Seed])).

hm_split(Long, Short, I, Seed, Best, {JBytes, _L}) ->
    J = JBytes div 2,
    PrefixLength = common_prefix(
        binary:part(Long, I * 2, byte_size(Long) - I * 2),
        binary:part(Short, J * 2, byte_size(Short) - J * 2)
    ),
    SuffixLength = common_suffix(
        binary:part(Long, 0, I * 2),
        binary:part(Short, 0, J * 2)
    ),
    Best1 = case byte_size(Best#half_match.m_common) / 2 < PrefixLength + SuffixLength of
        true  -> #half_match{
            l_prefix = binary:part(Long, 0, (I - SuffixLength) * 2),
            l_suffix = binary:part(Long, (I + PrefixLength) * 2, byte_size(Long) - (I + PrefixLength) * 2),
            r_prefix = binary:part(Short, 0, (J - SuffixLength) * 2),
            r_suffix = binary:part(Short, (J + PrefixLength) * 2, byte_size(Short) - (J + PrefixLength) * 2),
            m_common = begin
                A = binary:part(Short, (J - SuffixLength) * 2, SuffixLength * 2),
                B = binary:part(Short, J * 2, PrefixLength * 2),
                <<A/binary, B/binary>>
            end
        };
        false -> Best
    end,
    hm_split(
        Long, Short, I, Seed, Best1,
        binary:match(Short, [Seed], [{scope, {(J + 1) * 2, byte_size(Short) - (J + 1) * 2}}])
    );
hm_split(Long, _Short, _I, _Seed, #half_match{m_common = Common} = Best, nomatch)
    when byte_size(Common) * 2 >= byte_size(Long) ->
    Best;
hm_split(_Long, _Short, _I, _Seed, _Best, nomatch) ->
    nomatch.

hm_join(_, nomatch, nomatch) -> nomatch;
hm_join(Longer, Q2, nomatch) -> hm_unswap(Longer, Q2);
hm_join(Longer, nomatch, Q3) -> hm_unswap(Longer, Q3);
hm_join(Longer, Q2, Q3)
    when byte_size(Q2#half_match.m_common) > byte_size(Q3#half_match.m_common) ->
    hm_unswap(Longer, Q2);
hm_join(Longer, _Q2, Q3) ->
    hm_unswap(Longer, Q3).

hm_unswap(left, HalfMatch) -> HalfMatch;
hm_unswap(right, HalfMatch) -> #half_match{
    l_prefix = HalfMatch#half_match.r_prefix,
    l_suffix = HalfMatch#half_match.r_suffix,
    r_prefix = HalfMatch#half_match.l_prefix,
    r_suffix = HalfMatch#half_match.l_suffix,
    m_common = HalfMatch#half_match.m_common
}.

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
        }, half_match(?S("a345678z"), ?S("1234567890"))),
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

lines_to_chars_test_() -> [
    ?_assertEqual(#line_hashes{
        left   = <<1:16, 2:16, 1:16>>,
        right  = <<2:16, 1:16, 2:16>>,
        lookup = {?S(""), ?S("alpha\n"), ?S("beta\n")}
    }, lines_to_chars(?S("alpha\nbeta\nalpha\n"), ?S("beta\nalpha\nbeta\n"))),
    ?_assertEqual(#line_hashes{
        left   = <<>>,
        right  = <<1:16, 2:16, 3:16, 3:16>>,
        lookup = {?S(""), ?S("alpha\r\n"), ?S("beta\r\n"), ?S("\r\n")}
    }, lines_to_chars(?S(""), ?S("alpha\r\nbeta\r\n\r\n\r\n"))),
    ?_assertEqual(#line_hashes{
        left   = <<1:16>>,
        right  = <<2:16>>,
        lookup = {?S(""), ?S("a"), ?S("b")}
    }, lines_to_chars(?S("a"), ?S("b"))),
    begin
        N = 300,
        Chars = <<<<C:16>> || C <- lists:seq(1, N)>>,
        Lines = [<<(unicode:characters_to_binary(io_lib:format("~p", [I]), utf8, utf16))/binary, "\n"/utf16>> || I <- lists:seq(1, N)],
        Text  = <<<<Line/binary>> || Line <- Lines>>,
        [
            ?_assertEqual(N, length(Lines)),
            ?_assertEqual(N, byte_size(Chars) div 2),
            ?_assertEqual(#line_hashes{
                left   = Chars,
                right  = ?S(""),
                lookup = erlang:list_to_tuple([<<>> | Lines])
            }, lines_to_chars(Text, ?S("")))
        ]
    end
].

-endif.