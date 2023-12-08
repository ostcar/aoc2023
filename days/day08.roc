app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day08.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput1 =
    """
    LLR

    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)
    """

expect
    got = part1 exampleInput1
    got == "6"

part1 = \input ->
    input
    |> parseInput
    |> walkNetwork
    |> Num.toStr

parseInput = \input ->
    (rawInstructions, rawNetwork) =
        input
        |> Str.trim
        |> Str.replaceEach "(" ""
        |> Str.replaceEach ")" ""
        |> Str.replaceEach " " ""
        |> cut "\n\n"

    instructions =
        rawInstructions
        |> Str.toUtf8
        |> List.map \i ->
            when i is
                'R' -> Right
                'L' -> Left
                _ -> crash "invalid char in instructions"

    network =
        rawNetwork
        |> Str.split "\n"
        |> List.walk
            (Dict.empty {})
            (\dict, line ->
                (key, values) = cut line "="
                (v1, v2) = cut values ","
                Dict.insert dict key (v1, v2)
            )

    (instructions, network)

walkNetwork = \(instructions, network) ->
    walkNetworkHelper network instructions isEndPart1 "AAA" 0

isEndPart1 = \node ->
    node == "ZZZ"

walkNetworkHelper = \network, instructions, isEnd, current, idx ->
    if isEnd current then
        idx
    else
        (left, right) =
            when Dict.get network current is
                Ok v -> v
                _ -> crash "\(current) not in dict"

        next =
            when getRem instructions idx is
                Left -> left
                Right -> right

        walkNetworkHelper network instructions isEnd next (idx + 1)

getRem = \list, idx ->
    r = List.get
        list
        (idx % (List.len list))
    when r is
        Ok e -> e
        _ -> crash "this can not happen"

exampleInput2 =
    """
    LR

    11A = (11B, XXX)
    11B = (XXX, 11Z)
    11Z = (11B, XXX)
    22A = (22B, XXX)
    22B = (22C, 22C)
    22C = (22Z, 22Z)
    22Z = (22B, 22B)
    XXX = (XXX, XXX)
    """

expect
    got = part2 exampleInput2
    got == "6"

part2 = \input ->
    input
    |> parseInput
    |> findSameEnd
    |> Num.toStr

findSameEnd = \(instructions, network) ->
    network
    |> Dict.keys
    |> List.keepIf \e -> Str.endsWith e "A"
    |> List.map \node -> walkNetworkHelper network instructions isEndPart2 node 0
    |> lcmList

isEndPart2 = \node ->
    Str.endsWith node "Z"

gcd = \a, b ->
    if a == 0 then
        b
    else
        gcd (b % a) a

lcm = \a, b ->
    (a // (gcd a b)) * b

expect
    got = lcm 15 20
    got == 60

# Inspired by https://www.geeksforgeeks.org/lcm-of-given-array-elements/?ref=lbp
lcmList = \list ->
    when list is
        [] -> 0
        [a] -> a
        [a, b, .. as rest] ->
            List.walk rest (lcm a b) lcm

expect
    got = lcmList [2, 7, 3, 9, 4]
    got == 252

cut = \input, c ->
    when Str.splitFirst input c is
        Ok { before, after } -> (before, after)
        _ -> crash "can find \(c)"
