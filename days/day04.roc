app "day0"
    packages {
        pf: "../platform/main.roc",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        "day04.input" as puzzleInput : Str,
        parser.String.{ digits, parseStr, string, codeunit },
        parser.Core.{ const, skip, keep, maybe, oneOrMore, sepBy },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

example1Input =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

part1 = \input ->
    when parseStr inputParser input is
        Ok parsed ->
            parsed
            |> List.map measureCardWorth
            |> List.sum
            |> Num.toStr

        Err (ParsingFailure msg) -> "failure \(msg)"
        Err (ParsingIncomplete msg) -> "incomplete \(msg)"

expect
    got = part1 example1Input
    got == "13"

cardParser =
    const (\winning -> \youHave -> (Set.fromList winning, Set.fromList youHave))
    |> skip (string "Card")
    |> skip (oneOrMore (codeunit ' '))
    |> skip (digits)
    |> skip (string ":")
    |> skip (oneOrMore (codeunit ' '))
    |> keep (digits |> sepBy (oneOrMore (codeunit ' ')))
    |> skip (string " |")
    |> skip (oneOrMore (codeunit ' '))
    |> keep (digits |> sepBy (oneOrMore (codeunit ' ')))

expect
    input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    got = parseStr cardParser input
    got == Ok (Set.fromList [41, 48, 83, 86, 17], Set.fromList [83, 86, 6, 31, 17, 9, 48, 53])

inputParser =
    const (\l -> l)
    |> keep (cardParser |> sepBy (codeunit '\n'))
    |> skip (maybe (codeunit '\n'))

expect
    input = "Card 1: 41 17 | 83 53\nCard 2: 13 61 | 61 19"
    got = parseStr inputParser input
    got == Ok [(Set.fromList [41, 17], Set.fromList [83, 53]), (Set.fromList [13, 61], Set.fromList [61, 19])]

measureCardWorth = \(set1, set2) ->
    Set.intersection set1 set2
    |> Set.len
    |> (
        \c ->
            if c == 0 then
                0
            else
                Num.powInt 2 (c - 1)
    )

example2Input =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

expect
    got = part2 example2Input
    got == "30"

part2 = \input ->
    when parseStr inputParser input is
        Ok parsed ->
            parsed
            |> countCards
            |> List.sum
            |> Num.toStr

        Err (ParsingFailure msg) -> "failure \(msg)"
        Err (ParsingIncomplete msg) -> "incomplete \(msg)"

countCards = \input ->
    oneCardEach = List.map input \card -> (1, card)

    List.walk
        (List.range { start: At 0, end: Before (List.len oneCardEach) })
        oneCardEach
        (\card, idx ->
            when List.get card idx is
                Ok (num, (set1, set2)) ->
                    found = Set.intersection set1 set2 |> Set.len
                    addToCards card idx num found

                Err _ -> crash "element not found"

        )
    |> List.map .0

addToCards = \cards, idx, cardCount, found ->
    List.mapWithIndex
        cards
        (\(num, card), i ->
            if i < (idx + 1) || i > idx + found then
                (num, card)
            else
                (num + cardCount, card)

        )
