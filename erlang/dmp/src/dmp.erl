%%%-----------------------------------------------------------------------------
%%% @doc diff-match-patch implementation in Erlang
%%%
%%% This library matches the behavior of most of the existing
%%% implementations by operating on UTF-16 code units. More
%%% efficient or different diffs could be provided by working
%%% with byte-level diffing or with Unicode code point diffs,
%%% but this decision ensures consistency with existing libraries
%%% in other languages.
%%%
%%% TODO:
%%%   - un-skip chars_to_lines_test_skip_slow()
%%%     it's disabled for development convenience
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(dmp).
-author("Dennis Snell <dennis.snell@automattic.com>").

-include_lib("eunit/include/eunit.hrl").

-define(S(X), <<X/utf16>>).
-define(EQ(X), {equal, ?S(X)}).
-define(INS(X), {insert, ?S(X)}).
-define(DEL(X), {delete, ?S(X)}).

-type diff_op() :: equal | delete | insert.
-type diff() :: {diff_op(), binary()}.

-record(bisect_state, {
    left     :: binary(),
    right    :: binary(),
    l_length :: non_neg_integer(),
    r_length :: non_neg_integer(),
    v_length :: non_neg_integer(),
    max_d    :: non_neg_integer(),
    v1       :: atomics:atomics_ref(),
    v2       :: atomics:atomics_ref(),
    delta    :: integer(),
    front    :: boolean(),
    state    :: atomics:atomics_ref()
}).

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

-export([main/2]).

%% Interface functions

main(<<>>, Right) ->
    [{insert, Right}];
main(Left, <<>>) ->
    [{delete, Left}];
main(Same, Same) ->
    [{equal, Same}];
main(Left, Right) ->
    bisect(Left, Right).

%% Internal functions

bisect(Left, Right) ->
    LLength = byte_size(Left) div 2,
    RLength = byte_size(Right) div 2,
    MaxD    = ceil((LLength + RLength + 1) / 2),
    VLength = MaxD * 2,
    % I don't understand why this +1 is necessary
    % but for short strings it crashes with OOB
    V1      = atomics:new(VLength + 1, [{signed, true}]),
    V2      = atomics:new(VLength + 1, [{signed, true}]),
    ok      = atomic_set(V1, -1),
    ok      = atomic_set(V2, -1),
    ok      = a_set(V1, MaxD + 1, 0),
    ok      = a_set(V2, MaxD + 1, 0),
    Delta   = LLength - RLength,
    Front   = Delta rem 2 =/= 0,
    % k1start, k1end, k2start, k2end
    State   = atomics:new(4, [{signed, true}]),
    case bisect_d_loop(#bisect_state{
        left     = Left,
        right    = Right,
        l_length = LLength,
        r_length = RLength,
        v_length = VLength,
        max_d    = MaxD,
        v1       = V1,
        v2       = V2,
        delta    = Delta,
        front    = Front,
        state    = State
    }, 0, continue_forward) of
        timeout -> [{delete, Left}, {insert, Right}];
        Diffs   -> Diffs
    end.

bisect_d_loop(#bisect_state{max_d = MaxD}, D, _) when D >= MaxD ->
    timeout;
bisect_d_loop(_Args, _D, {diffs, Diffs}) ->
    Diffs;
bisect_d_loop(#bisect_state{state = S} = Args, D, continue_forward) ->
    bisect_d_loop(Args, D, bisect_forward(Args, D, -D + a_get(S, k1start), D - a_get(S, k1end)));
bisect_d_loop(#bisect_state{state = S} = Args, D, continue_reverse) ->
    bisect_d_loop(Args, D + 1, bisect_reverse(Args, D, -D + a_get(S, k2start), D - a_get(S, k2end))).

bisect_forward(_Args, _D, K1, MaxK1) when K1 > MaxK1 ->
    continue_reverse;
bisect_forward(#bisect_state{
    left     = Left,
    right    = Right,
    l_length = LLength,
    r_length = RLength,
    v_length = VLength,
    delta    = Delta,
    front    = Front,
    max_d    = VOffset,
    state    = S,
    v1       = V1,
    v2       = V2
} = Args, D, K1, _MaxK1) ->
    K1Offset = VOffset + K1,
    {X0, Y0} = bisect_get_xy(K1, D, V1, K1Offset),
    {X1, Y1} = bisect_advance_forward(Left, Right, LLength, RLength, X0, Y0),
    ok       = a_set(V1, K1Offset, X1),
    RanOffR  = X1 > LLength,
    RanOffB  = Y1 > RLength,
    K2Offset = VOffset + Delta - K1,
    AtV2     = a_get(V2, K2Offset),
    X2       = LLength - AtV2,
    case if
        RanOffR             -> a_add(S, k1end, 2),
                               continue;
        RanOffB             -> a_add(S, k1start, 2),
                               continue;
        not Front           -> continue;
        K2Offset < 0        -> continue;
        K2Offset >= VLength -> continue;
        AtV2 == -1          -> continue;
        X1 >= X2            -> bisect_split(Left, Right, X1, Y1);
        true                -> continue
    end of
        continue            -> bisect_forward(Args, D, K1 + 2, D - a_get(S, k1end));
        Diffs               -> {diffs, Diffs} 
    end.

bisect_advance_forward(_Left, _Right, LLength, RLength, X, Y)
    when X >= LLength; Y >= RLength ->
    {X, Y};
bisect_advance_forward(Left, Right, LLength, RLength, X, Y) ->
    case {utf16_at(Left, X), utf16_at(Right, Y)} of
        {C, C} -> bisect_advance_forward(Left, Right, LLength, RLength, X + 1, Y + 1);
        _      -> {X, Y}
    end.

bisect_reverse(_Args, _D, K2, MaxK2) when K2 > MaxK2 ->
    continue_reverse;
bisect_reverse(#bisect_state{
    left     = Left,
    right    = Right,
    l_length = LLength,
    r_length = RLength,
    v_length = VLength,
    delta    = Delta,
    front    = Front,
    max_d    = VOffset,
    state    = S,
    v1       = V1,
    v2       = V2
} = Args, D, K2, _MaxK2) ->
    K2Offset = VOffset + K2,
    {X0, Y0} = bisect_get_xy(K2, D, V2, K2Offset),
    {X2, Y2} = bisect_advance_reverse(Left, Right, LLength, RLength, X0, Y0),
    ok       = a_set(V2, K2Offset, X2),
    RanOffL  = X2 > LLength,
    RanOffT  = Y2 > RLength,
    K1Offset = VOffset + Delta - K2,
    AtV1     = a_get(V1, K1Offset),
    X1       = AtV1,
    Y1       = VOffset + X1 - K1Offset,
    X2Mirror = LLength - X2,
    case if
        RanOffL             -> a_add(S, k2end, 2),
                               continue;
        RanOffT             -> a_add(S, k2start, 2),
                               continue;
        Front               -> continue;
        K1Offset < 0        -> continue;
        K1Offset >= VLength -> continue;
        AtV1 == -1          -> continue;
        X1 >= X2Mirror      -> bisect_split(Left, Right, X1, Y1);
        true                -> continue
    end of
        continue            -> bisect_reverse(Args, D, K2 + 2, D - a_get(S, k2end));
        Diffs               -> {diffs, Diffs} 
    end.

bisect_advance_reverse(_Left, _Right, LLength, RLength, X, Y)
    when X >= LLength; Y >= RLength ->
    {X, Y};
bisect_advance_reverse(Left, Right, LLength, RLength, X, Y) ->
    case {
        utf16_at(Left, LLength - X - 1),
        utf16_at(Right, RLength - Y - 1)
    } of
        {C, C} -> bisect_advance_reverse(Left, Right, LLength, RLength, X + 1, Y + 1);
        _      -> {X, Y}
    end.

bisect_get_xy(K, D, V, KOffset) ->
    Prev = a_get(V, KOffset - 1),
    Next = a_get(V, KOffset + 1),
    X = if
        K == -D -> Next;
        K =/= D andalso Prev < Next -> Next;
        true -> Prev + 1
    end,
    {X, X - K}.

bisect_split(Left, Right, X, Y) ->
    LL =    prefix(Left,  X * 2),
    LR = no_prefix(Left,  X * 2),
    RL =    prefix(Right, Y * 2),
    RR = no_prefix(Right, Y * 2),
    lists:append(main(LL, RL), main(LR, RR)).

bs_state_arg(k1start) -> 0;
bs_state_arg(k1end)   -> 1;
bs_state_arg(k2start) -> 2;
bs_state_arg(k2end)   -> 3.

a_add(Ref, Arg, Inc) when is_atom(Arg) -> a_add(Ref, bs_state_arg(Arg), Inc);
a_add(Ref, Index, Inc) -> atomics:add(Ref, Index + 1, Inc).

a_get(Ref, Arg) when is_atom(Arg) -> a_get(Ref, bs_state_arg(Arg));
a_get(Ref, Index) -> atomics:get(Ref, Index + 1).

a_set(Ref, Arg, Value) when is_atom(Arg) -> a_set(Ref, bs_state_arg(Arg), Value);
a_set(Ref, Index, Value) -> atomics:put(Ref, Index + 1, Value).

atomic_set(Ref, Value) ->
    #{size := Size} = atomics:info(Ref),
    atomic_set(Ref, Value, Size).

atomic_set(_Ref, _Value, 0) ->
    ok;
atomic_set(Ref, Value, Index) ->
    ok = atomics:put(Ref, Index, Value),
    atomic_set(Ref, Value, Index - 1).

-spec cleanup_semantic_lossless(Diffs) -> Diffs
    when Diffs :: list(diff()).

cleanup_semantic_lossless(Diffs) ->
    csl_scan(Diffs, 0).

csl_scan(Diffs, N) when N >= length(Diffs) ->
    Diffs;
csl_scan(Diffs, N) ->
    {Front, Diffs1} = lists:split(N, Diffs),
    {Diffs2, NN} = csl_reduce(Diffs1),
    csl_scan(lists:append(Front, Diffs2), N + 1 + NN).

csl_reduce([{equal, A}, {Op, B}, {equal, C} | Diffs]) when Op =/= equal ->
    {A1, B1, C1} = case common_suffix(A, B) of
        0  -> {A, B, C};
        NS -> {
            no_suffix(A, NS * 2),
            <<(suffix(B, NS * 2))/binary, (no_suffix(B, NS * 2))/binary>>,
            <<(suffix(B, NS * 2))/binary, C/binary>>
        }
    end,
    Score = cleanup_semantic_score(A1, B1) + cleanup_semantic_score(B1, C1),
    case csl_find_best({A1, B1, C1}, {A1, B1, C1, Score}) of
        {<<>>, B2, <<>>} -> {[{Op, B2} | Diffs], -1};
        {<<>>, B2, C2}   -> {[{Op, B2}, {equal, C2} | Diffs], -1};
        {A2, B2, <<>>}   -> {[{equal, A2}, {Op, B2} | Diffs], 0};
        {A2, B2, C2}     -> {[{equal, A2}, {Op, B2}, {equal, C2} | Diffs], 0}
    end;
csl_reduce(Diffs) ->
    {Diffs, 0}.

csl_find_best({A, <<X:16, B/binary>>, <<X:16, C1/binary>>}, {BA, BB, BC, BestScore}) ->
    A1 = <<A/binary, X:16>>,
    B1 = <<B/binary, X:16>>,
    case cleanup_semantic_score(A1, B1) + cleanup_semantic_score(B1, C1) of
        Score when Score >= BestScore ->
            csl_find_best({A1, B1, C1}, {A1, B1, C1, Score});
        _ ->
            csl_find_best({A1, B1, C1}, {BA, BB, BC, BestScore})
    end;
csl_find_best(_, {BA, BB, BC, _}) ->
    {BA, BB, BC}.

-spec cleanup_semantic_score(Left :: binary(), Right :: binary()) -> non_neg_integer().

cleanup_semantic_score(Left, Right) when Left == <<"">>; Right == <<"">> -> 6;
cleanup_semantic_score(Left, Right) ->
    L_C = suffix(Left, 2),
    R_C = prefix(Right, 2),
    L_NA = c_non_alphanum(L_C),
    R_NA = c_non_alphanum(R_C),
    L_WS = L_NA andalso c_space(L_C),
    R_WS = R_NA andalso c_space(R_C),
    L_LB = L_WS andalso c_linebreak(L_C),
    R_LB = R_WS andalso c_linebreak(R_C),
    L_BL = L_LB andalso css_blank_line(left, Left),
    R_BL = R_LB andalso css_blank_line(right, Right),
    if
        L_BL; R_BL -> 5;
        L_LB; R_LB -> 4;
        L_NA, not L_WS, R_WS -> 3;
        L_WS; R_WS -> 2;
        L_NA; R_NA -> 1;
        true -> 0
    end.

css_blank_line(left, Text) when byte_size(Text) >= 6 ->
    case suffix(Text, 6) of
        <<"\n\r\n"/utf16>> -> true;
        <<_:16, "\n\n"/utf16>> -> true;
        _ -> false
    end;
css_blank_line(left, Text) when byte_size(Text) >= 4 ->
    case suffix(Text, 4) of
        <<"\n\n"/utf16>> -> true;
        _ -> false
    end;
css_blank_line(right, <<"\n\n"/utf16, _>>) -> true;
css_blank_line(right, <<"\n\r\n"/utf16, _>>) -> true;
css_blank_line(right, <<"\r\n\n"/utf16, _>>) -> true;
css_blank_line(right, <<"\r\n\r\n"/utf16, _>>) -> true;
css_blank_line(_, _) -> false.

c_upper(<<C:16>>) -> C >= $A andalso C =< $Z.
c_lower(<<C:16>>) -> C >= $a andalso C =< $z.
c_number(<<C:16>>) -> C >= $0 andalso C =< $9.
c_alpha(C) -> c_upper(C) orelse c_lower(C).
c_alphanum(C) -> c_alpha(C) orelse c_number(C).
c_non_alphanum(C) -> not c_alphanum(C).

c_space(<<" "/utf16>>) -> true;
c_space(<<"\f"/utf16>>) -> true;
c_space(<<"\n"/utf16>>) -> true;
c_space(<<"\r"/utf16>>) -> true;
c_space(<<"\t"/utf16>>) -> true;
c_space(<<"\v"/utf16>>) -> true;
c_space(<<16#a0:16>>) -> true;
c_space(<<16#1680:16>>) -> true;
c_space(<<C:16>>) when C >= 16#2000 andalso C =< 16#200a -> true;
c_space(<<16#2028:16>>) -> true;
c_space(<<16#2029:16>>) -> true;
c_space(<<16#202f:16>>) -> true;
c_space(<<16#205f:16>>) -> true;
c_space(<<16#3000:16>>) -> true;
c_space(<<16#feff:16>>) -> true;
c_space(_) -> false.

c_linebreak(<<"\n"/utf16>>) -> true;
c_linebreak(<<"\r"/utf16>>) -> true;
c_linebreak(_) -> false.

-spec cleanup_merge(Diffs) -> Diffs
    when Diffs :: list(diff()).

cleanup_merge(Diffs) ->
    Pass1 = cm_merge(cm_combine(Diffs, [], [], [])),
    case cm_merge(cm_single_edits(Pass1, [])) of
        Pass1 -> Pass1;
        Next2 -> cleanup_merge(Next2)
    end.

cm_single_edits([], Output) ->
    lists:reverse(Output);
cm_single_edits([{equal, A}, {Op, B}, {equal, C} | Diffs], Output) when Op =/= equal ->
    case suffix(B, byte_size(A)) of
        A -> cm_single_edits(Diffs, [
            {equal, <<A/binary, C/binary>>},
            {Op, <<A/binary, (no_suffix(B, byte_size(A)))/binary>>} |
            Output
        ]);
        _ -> case prefix(B, byte_size(C)) of
            C -> cm_single_edits(Diffs, [
                {Op, <<(no_prefix(B, byte_size(C)))/binary, C/binary>>},
                {equal, <<A/binary, C/binary>>} |
                Output
            ]);
            _ -> cm_single_edits([{Op, B}, {equal, C} | Diffs], [{equal, A} | Output])
        end
    end;
cm_single_edits([Diff | Diffs], Output) ->
    cm_single_edits(Diffs, [Diff | Output]).

cm_merge(Diffs) ->
    cm_merge(Diffs, []).

cm_merge([], Output) ->
    lists:reverse(Output);
cm_merge([{_Op, <<>>} | Diffs], Output) ->
    cm_merge(Diffs, Output);
cm_merge([{Op, A}, {Op, B} | Diffs], Output) ->
    cm_merge(Diffs, [{Op, <<A/binary, B/binary>>} | Output]);
cm_merge([Diff | Diffs], Output) ->
    cm_merge(Diffs, [Diff | Output]).

cm_combine([], [], [], Output) ->
    lists:reverse(Output);
cm_combine([], Delete, Insert, Output) ->
    cm_combine([{equal, <<>>}], Delete, Insert, Output);
cm_combine([{equal, E0} | Diffs], Delete, Insert, Output) ->
    D0 = <<<<S/binary>> || S <- lists:reverse(Delete)>>,
    I0 = <<<<S/binary>> || S <- lists:reverse(Insert)>>,
    {EB, D1, I1} = case common_prefix(D0, I0) of
        0 -> {<<>>, D0, I0};
        NP -> {
            prefix(I0, NP * 2),
            no_prefix(D0, NP * 2),
            no_prefix(I0, NP * 2)
        }
    end,
    {EA, D2, I2} = case common_suffix(D1, I1) of
        0 -> {E0, D1, I1};
        NS -> {
            <<(suffix(I0, NS * 2))/binary, E0/binary>>,
            no_suffix(D1, NS * 2),
            no_suffix(I1, NS * 2)
        }
    end,
    cm_combine(Diffs, [], [], [
        {equal, EA},
        {insert, I2},
        {delete, D2},
        {equal, EB} |
        Output
    ]);
cm_combine([{insert, A} | Diffs], Delete, Insert, Output) ->
    cm_combine(Diffs, Delete, [A | Insert], Output);
cm_combine([{delete, A} | Diffs], Delete, Insert, Output) ->
    cm_combine(Diffs, [A | Delete], Insert, Output).

-spec chars_to_lines(CharDiffs, Lookup) -> LineDiffs
    when CharDiffs :: list({diff_op(), binary()}),
         Lookup    :: tuple(),
         LineDiffs :: list({diff_op(), binary()}).

chars_to_lines(Diffs, Lookup) ->
    [{Op, ctl_line(Chars, Lookup, [])} || {Op, Chars} <- Diffs].

ctl_line(<<>>, _Lookup, Lines) ->
    erlang:list_to_binary(lists:reverse(Lines));
ctl_line(<<N:16, Rest/binary>>, Lookup, Lines) ->
    ctl_line(Rest, Lookup, [erlang:element(N + 1, Lookup) | Lines]).

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
    PrefixLength = common_prefix(no_prefix(Long, I * 2), no_prefix(Short, J * 2)),
    SuffixLength = common_suffix(prefix(Long, I * 2), prefix(Short, J * 2)),
    Best1 = case byte_size(Best#half_match.m_common) / 2 < PrefixLength + SuffixLength of
        true  -> #half_match{
            l_prefix =    prefix(Long,  (I - SuffixLength) * 2),
            l_suffix = no_prefix(Long,  (I + PrefixLength) * 2),
            r_prefix =    prefix(Short, (J - SuffixLength) * 2),
            r_suffix = no_prefix(Short, (J + PrefixLength) * 2),
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

no_prefix(Binary, 0) -> Binary;
no_prefix(Binary, Bytes) when byte_size(Binary) =< Bytes -> <<>>;
no_prefix(Binary, Bytes) -> binary:part(Binary, Bytes, byte_size(Binary) - Bytes).

no_suffix(Binary, 0) -> Binary;
no_suffix(Binary, Bytes) when byte_size(Binary) =< Bytes -> <<>>;
no_suffix(Binary, Bytes) -> binary:part(Binary, 0, byte_size(Binary) - Bytes).

utf16_at(Text, Index) when is_binary(Text), is_integer(Index) ->
    binary:part(Text, Index * 2, 2).

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

chars_to_lines_test_skip_slow() -> [
    begin
        CDiffs  = [{equal, <<1:16, 2:16, 1:16>>}, {insert, <<2:16, 1:16, 2:16>>}],
        Lookup1 = {<<>>, ?S("alpha\n"), ?S("beta\n")},
        LDiffs  = [{equal, ?S("alpha\nbeta\nalpha\n")}, {insert, ?S("beta\nalpha\nbeta\n")}],
        ?_assertEqual(LDiffs, chars_to_lines(CDiffs, Lookup1))
    end,
    begin
        N2      = 3,
        Chars2  = <<<<C:16>> || C <- lists:seq(1, N2)>>,
        Lines2  = [<<(unicode:characters_to_binary(io_lib:format("~p", [I]), utf8, utf16))/binary, "\n"/utf16>> || I <- lists:seq(1, N2)],
        Text2   = <<<<Line/binary>> || Line <- Lines2>>,
        Lookup2 = erlang:list_to_tuple([<<>> | Lines2]),
        [
            ?_assertEqual(N2, length(Lines2)),
            ?_assertEqual(N2, byte_size(Chars2) div 2),
            ?_assertEqual([{delete, Text2}], chars_to_lines([{delete, Chars2}], Lookup2))
        ]
    end,
    begin
        N3     = 66000,
        Lines3 = [<<(unicode:characters_to_binary(io_lib:format("~p", [I]), utf8, utf16))/binary, "\n"/utf16>> || I <- lists:seq(1, N3)],
        Text3  = <<<<Line/binary>> || Line <- Lines3>>,
        #line_hashes{left = Chars3, lookup = Lookup3} = lines_to_chars(Text3, ?S("")),
        ?_assertEqual([{insert, Text3}], chars_to_lines([{insert, Chars3}], Lookup3))
    end
].

cleanup_merge_test_() -> [
    {"Null case", ?_assertEqual([], cleanup_merge([]))},
    {"No change case", ?_assertEqual(
        [?EQ("a"), ?DEL("b"), ?INS("c")],
        cleanup_merge([?EQ("a"), ?DEL("b"), ?INS("c")])
    )},
    {"Merge equalities", ?_assertEqual(
        [?EQ("abc")],
        cleanup_merge([?EQ("a"), ?EQ("b"), ?EQ("c")])
    )},
    {"Merge deletions", ?_assertEqual(
        [?DEL("abc")],
        cleanup_merge([?DEL("a"), ?DEL("b"), ?DEL("c")])
    )},
    {"Merge insertions", ?_assertEqual(
        [?INS("abc")],
        cleanup_merge([?INS("a"), ?INS("b"), ?INS("c")])
    )},
    {"Merge interweave", ?_assertEqual(
        [?DEL("ac"), ?INS("bd"), ?EQ("ef")],
        cleanup_merge([?DEL("a"), ?INS("b"), ?DEL("c"), ?INS("d"), ?EQ("e"), ?EQ("f")])
    )},
    {"Prefix and suffix detection", ?_assertEqual(
        [?EQ("a"), ?DEL("d"), ?INS("b"), ?EQ("c")],
        cleanup_merge([?DEL("a"), ?INS("abc"), ?DEL("dc")])
    )},
    {"Prefix and suffix detection with equalities", ?_assertEqual(
        [?EQ("xa"), ?DEL("d"), ?INS("b"), ?EQ("cy")],
        cleanup_merge([?EQ("x"), ?DEL("a"), ?INS("abc"), ?DEL("dc"), ?EQ("y")])
    )},
    {"Slide edit left", ?_assertEqual(
        [?INS("ab"), ?EQ("ac")],
        cleanup_merge([?EQ("a"), ?INS("ba"), ?EQ("c")])
    )},
    {"Slide edit right", ?_assertEqual(
        [?EQ("ca"), ?INS("ba")],
        cleanup_merge([?EQ("c"), ?INS("ab"), ?EQ("a")])
    )},
    {"Slide edit left recursive", ?_assertEqual(
        [?DEL("abc"), ?EQ("acx")],
        cleanup_merge([?EQ("a"), ?DEL("b"), ?EQ("c"), ?DEL("ac"), ?EQ("x")])
    )},
    {"Slide edit right recursive", ?_assertEqual(
        [?EQ("xca"), ?DEL("cba")],
        cleanup_merge([?EQ("x"), ?DEL("ca"), ?EQ("c"), ?DEL("b"), ?EQ("a")])
    )},
    {"Empty merge", ?_assertEqual(
        [?INS("a"), ?EQ("bc")],
        cleanup_merge([?DEL("b"), ?INS("ab"), ?EQ("c")])
    )},
    {"Empty equality", ?_assertEqual(
        [?INS("a"), ?EQ("b")],
        cleanup_merge([?EQ(""), ?INS("a"), ?EQ("b")])
    )}
].

cleanup_semantic_lossless_test_() -> [
    {"Null case", ?_assertEqual([], cleanup_semantic_lossless([]))},
    {"Blank lines", ?_assertEqual(
        [?EQ("AAA\r\n\r\n"), ?INS("BBB\r\nDDD\r\n\r\n"), ?EQ("BBB\r\nEEE")],
        cleanup_semantic_lossless([?EQ("AAA\r\n\r\nBBB"), ?INS("\r\nDDD\r\n\r\nBBB"), ?EQ("\r\nEEE")])
    )},
    {"Line boundaries", ?_assertEqual(
        [?EQ("AAA\r\n"), ?INS("BBB DDD\r\n"), ?EQ("BBB EEE")],
        cleanup_semantic_lossless([?EQ("AAA\r\nBBB"), ?INS(" DDD\r\nBBB"), ?EQ(" EEE")])
    )},
    {"Word boundaries", ?_assertEqual(
        [?EQ("The "), ?INS("cow and the "), ?EQ("cat.")],
        cleanup_semantic_lossless([?EQ("The c"), ?INS("ow and the c"), ?EQ("at.")])
    )},
    {"Alphanumeric boundaries", ?_assertEqual(
        [?EQ("The-"), ?INS("cow-and-the-"), ?EQ("cat.")],
        cleanup_semantic_lossless([?EQ("The-c"), ?INS("ow-and-the-c"), ?EQ("at.")])
    )},
    {"Hitting the start", ?_assertEqual(
        [?DEL("a"), ?EQ("aax")],
        cleanup_semantic_lossless([?EQ("a"), ?DEL("a"), ?EQ("ax")])
    )},
    {"Hitting the end", ?_assertEqual(
        [?EQ("xaa"), ?DEL("a")],
        cleanup_semantic_lossless([?EQ("xa"), ?DEL("a"), ?EQ("a")])
    )},
    {"Sentence boundaries", ?_assertEqual(
        [?EQ("The xxx."), ?INS(" The zzz."), ?EQ(" The yyy.")],
        cleanup_semantic_lossless([?EQ("The xxx. The "), ?INS("zzz. The "), ?EQ("yyy.")])
    )}
].

-endif.