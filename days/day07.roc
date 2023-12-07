app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day07.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483
    """

expect
    got = part1 exampleInput
    got == "6440"

part1 = \input ->
    input
    |> parseInput
    |> List.map addType
    |> sortHands handOrder1
    |> List.mapWithIndex \(_, _, bid), idx -> ((idx |> Num.toU32) + 1) * bid
    |> List.sum
    |> Num.toStr

HandWithBid : (List U8, U32)
HandWithTypeWithBid : (List U8, Type, U32)
Type : [
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
]

typeOrder = \t ->
    when t is
        FiveOfAKind -> 6
        FourOfAKind -> 5
        FullHouse -> 4
        ThreeOfAKind -> 3
        TwoPair -> 2
        OnePair -> 1
        HighCard -> 0

handOrder1 = \u ->
    when u is
        'A' -> 12
        'K' -> 11
        'Q' -> 10
        'J' -> 9
        'T' -> 8
        '9' -> 7
        '8' -> 6
        '7' -> 5
        '6' -> 4
        '5' -> 3
        '4' -> 2
        '3' -> 1
        '2' -> 0
        _ -> crash "invalid card"

handOrder2 = \u ->
    when u is
        'A' -> 12
        'K' -> 11
        'Q' -> 10
        'J' -> 0
        'T' -> 9
        '9' -> 8
        '8' -> 7
        '7' -> 6
        '6' -> 5
        '5' -> 4
        '4' -> 3
        '3' -> 2
        '2' -> 1
        _ -> crash "invalid card"

parseInput : Str -> List HandWithBid
parseInput = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        when Str.split line " " is
            [p1, p2] ->
                when (p1 |> Str.toUtf8, p2 |> Str.toU32) is
                    (hand, Ok bid) -> (hand, bid)
                    _ -> crash "invalid number in input"

            _ -> crash "invalid input on \(line)"

addType : HandWithBid -> HandWithTypeWithBid
addType = \(hand, bid) ->
    sortedHand = List.sortDesc hand |> getter

    if isFiveOfAKind sortedHand then
        (hand, FiveOfAKind, bid)
    else if isFourOfAKind sortedHand then
        (hand, FourOfAKind, bid)
    else if isFullHouse sortedHand then
        (hand, FullHouse, bid)
    else if isThreeOfAKind sortedHand then
        (hand, ThreeOfAKind, bid)
    else if isTwoPair sortedHand then
        (hand, TwoPair, bid)
    else if isOnePair sortedHand then
        (hand, OnePair, bid)
    else
        (hand, HighCard, bid)

isFiveOfAKind = \h ->
    h 1 == h 5

isFourOfAKind = \h ->
    h 1 == h 4 || h 2 == h 5

isFullHouse = \h ->
    (h 1 == h 3 && h 4 == h 5) || h 3 == h 5 && h 1 == h 2

isThreeOfAKind = \h ->
    h 1 == h 3 || h 2 == h 4 || h 3 == h 5

isTwoPair = \h ->
    (h 1 == h 2 && (h 3 == h 4 || h 4 == h 5))
    ||
    (h 2 == h 3 && h 4 == h 5)

isOnePair = \h ->
    h 1 == h 2 || h 2 == h 3 || h 3 == h 4 || h 4 == h 5

sortHands = \list, handOrder ->
    List.sortWith
        list
        \(handA, typeA, _), (handB, typeB, _) ->
            typeCmp = Num.compare (typeOrder typeA) (typeOrder typeB)
            when typeCmp is
                EQ -> compList handA handB handOrder
                _ -> typeCmp

compList = \l1, l2, handOrder ->
    g1 = getter l1
    g2 = getter l2
    List.walkWithIndex
        l1
        EQ
        \state, _, idx ->
            when state is
                EQ ->
                    Num.compare (g1 (idx + 1) |> handOrder) (g2 (idx + 1) |> handOrder)

                _ -> state

expect
    got = part2 exampleInput
    got == "5905"

part2 = \input ->
    input
    |> parseInput
    |> List.map addTypeWithJoker
    |> sortHands handOrder2
    |> List.mapWithIndex \(_, _, bid), idx -> ((idx |> Num.toU32) + 1) * bid
    |> List.sum
    |> Num.toStr

addTypeWithJoker : HandWithBid -> HandWithTypeWithBid
addTypeWithJoker = \(handWithJoker, bid) ->
    (hand, jokers) = findJoker handWithJoker
    sortedHand =
        List.sortDesc hand |> getter

    kindWithoutJoker =
        if isFiveOfAKind sortedHand then
            FiveOfAKind
        else if isFourOfAKind sortedHand then
            FourOfAKind
        else if isFullHouse sortedHand then
            FullHouse
        else if isThreeOfAKind sortedHand then
            ThreeOfAKind
        else if isTwoPair sortedHand then
            TwoPair
        else if isOnePair sortedHand then
            OnePair
        else
            HighCard

    kindWithJoker =
        if jokers > 0 then
            when kindWithoutJoker is
                FourOfAKind if jokers == 1 -> FiveOfAKind
                ThreeOfAKind if jokers == 2 -> FiveOfAKind
                ThreeOfAKind if jokers == 1 -> FourOfAKind
                TwoPair if jokers == 1 -> FullHouse
                OnePair if jokers < 4 ->
                    if jokers == 3 then
                        FiveOfAKind
                    else if jokers == 2 then
                        FourOfAKind
                    else
                        ThreeOfAKind

                HighCard if jokers <= 5 ->
                    if jokers >= 4 then
                        FiveOfAKind
                    else if jokers == 3 then
                        FourOfAKind
                    else if jokers == 2 then
                        ThreeOfAKind
                    else
                        OnePair

                _ -> crash "impossible, jokers \(jokers |> Num.toStr)"
        else
            kindWithoutJoker

    (handWithJoker, kindWithJoker, bid)

findJoker = \list ->
    List.walk
        list
        ([], 0)
        \(hand, jokers), elem ->
            if elem == 'J' then
                (hand, jokers + 1)
            else
                (List.append hand elem, jokers)

getter = \list ->
    \idx ->
        when List.get list (idx - 1) is
            Ok e ->
                e |> Num.toU32

            _ ->
                # This is a hack for part 2 to get a unique number if hand has less then 5 cards
                1000 + (idx |> Num.toU32)
