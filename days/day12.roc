app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day12.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    ???.### 1,1,3
    .??..??...?##. 1,1,3
    ?#?#?#?#?#?#?#? 1,3,1,6
    ????.#...#... 4,1,1
    ????.######..#####. 1,6,5
    ?###???????? 3,2,1
    """

expect
    got = part1 exampleInput
    got == "21"

part1 = \input ->
    input
    |> Str.trim
    |> parseInput
    |> List.map \(line, groups) -> doLine line groups
    |> debug
    |> List.sum
    |> Num.toStr

parseInput : Str -> List (List U8, List Nat)
parseInput = \input ->
    input
    |> Str.split "\n"
    |> List.map \line ->
        Str.split line " "
        |> \parts ->
            v1 = List.first parts |> unwrap "first part" |> Str.toUtf8 |> List.append '\n'
            v2 =
                List.last parts
                |> unwrap "second part 1"
                |> Str.split ","
                |> List.map (\e -> Str.toNat e |> unwrap "second part 2 '\(e)'")
            (
                v1,
                v2,
            )

doLine : List U8, List Nat -> Nat
doLine = \line, groups ->
    List.walk
        groups
        [line]
        \inputList, group ->
            List.joinMap
                inputList
                \input ->
                    findAllPlacesForGroup input group
    |> List.len

expect
    input = "???.###\n" |> Str.toUtf8
    groups = [1, 1, 3]
    got = doLine input groups
    got == 1

expect
    input = ".??..??...?##.\n" |> Str.toUtf8
    groups = [1, 1, 3]
    got = doLine input groups
    got == 4

expect
    input = "?###????????\n" |> Str.toUtf8
    groups = [3, 2, 1]
    got = doLine input groups
    got == 10

findAllPlacesForGroup = \input, group ->
    findAllPlacesForGroupHelper input group []

findAllPlacesForGroupHelper : List U8, Nat, List (List U8) -> List (List U8)
findAllPlacesForGroupHelper = \input, group, result ->
    if List.len input > 0 then
        when findFirstIndex input group is
            Ok idx ->
                inputFromIdxWithoutOneChar = List.dropFirst input (idx + 1)
                beforeInput = List.takeFirst input (idx + group)
                restInput = List.dropFirst input (idx + group + 1)
                newResult = List.append result restInput

                if List.contains beforeInput '#' then
                    newResult
                else
                    findAllPlacesForGroupHelper inputFromIdxWithoutOneChar group newResult

            Err Impossible ->
                result
    else
        result

expect
    input = "???.##.\n" |> Str.toUtf8
    got = findAllPlacesForGroup input 1
    got
    == [
        "?.##.\n" |> Str.toUtf8,
        ".##.\n" |> Str.toUtf8,
        "##.\n" |> Str.toUtf8,
        "\n" |> Str.toUtf8,
    ]

expect
    input = "???\n" |> Str.toUtf8
    got = findAllPlacesForGroup input 3
    got
    == [
        "" |> Str.toUtf8,
    ]

expect
    input = "?###????????\n" |> Str.toUtf8
    got = findAllPlacesForGroup input 3
    got
    == [
        "???????\n" |> Str.toUtf8,
    ]

findFirstIndex : List U8, Nat -> Result Nat [Impossible]
findFirstIndex = \input, group ->
    findFirstIndexHelper input group 0

findFirstIndexHelper : List U8, Nat, Nat -> Result Nat [Impossible]
findFirstIndexHelper = \input, group, index ->
    if List.len input > group then
        sliceGroup = List.takeFirst input group
        if List.countIf sliceGroup isDamaged == group && List.get input (group) |> unwrap "deep" |> isOperational then
            Ok index
        else
            findFirstIndexHelper (List.dropFirst input 1) group (index + 1)
    else
        Err Impossible

expect
    input = "???.###\n" |> Str.toUtf8
    got = findFirstIndex input 1
    got == Ok 0

expect
    input = "???.###\n" |> Str.toUtf8
    got = findFirstIndex input 4
    got == Err Impossible

expect
    input = "???.###\n" |> Str.toUtf8
    got = findFirstIndex input 3
    got == Ok 0

expect
    input = ".???.###\n" |> Str.toUtf8
    got = findFirstIndex input 3
    got == Ok 1

expect
    input = ".???.####\n" |> Str.toUtf8
    got = findFirstIndex input 4
    got == Ok 5

expect
    input = "?###????????\n" |> Str.toUtf8
    got = findFirstIndex input 3
    got == Ok 1

isDamaged = \c ->
    c == '#' || c == '?'

isOperational = \c ->
    c == '.' || c == '?' || c == '\n'

part2 = \input ->
    "Not implemented yet"

unwrap = \r, msg ->
    when r is
        Ok v -> v
        _ -> crash "impossible \(msg)"

debug = \v ->
    dbg
        v

    v
