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
    |> List.map \(line, groups) ->
        doLine line groups
    |> List.sum
    |> Num.toStr

parseInput : Str -> List (List U8, List Nat)
parseInput = \input ->
    input
    |> Str.split "\n"
    |> List.map \line ->
        Str.split line " "
        |> \parts ->
            when parts is
                [first, last] ->
                    (
                        first |> Str.toUtf8 |> List.append '\n',
                        last |> Str.split "," |> List.map (\e -> Str.toNat e |> unwrap "second part 2 '\(e)'"),
                    )

                _ -> crash "invalid input"

doLine : List U8, List Nat -> Nat
doLine = \line, groups ->
    List.walk
        groups
        [(line, 1)]
        \inputList, group ->
            List.map
                inputList
                (
                    \(input, cound) ->
                        findAllPlacesForGroup input group
                        |> List.map \v -> (v, cound)

                )
            |> resultjoin
    |> List.dropIf \(rest, _) -> List.contains rest '#'
    |> List.map .1
    |> List.sum

resultjoin : List (List (List U8, Nat)) -> List (List U8, Nat)
resultjoin = \list ->
    resultjoinHelper list []

resultjoinHelper = \list, result ->
    when list is
        [] -> result
        [sublist, .. as rest] ->
            resultjoinHelper rest (resultConcat result sublist)

resultConcat : List (a, Nat), List (a, Nat) -> List (a, Nat) where a implements Eq
resultConcat = \baseList, toIncludeList ->
    List.walk
        toIncludeList
        baseList
        \state, elem ->
            resultAppend state elem []

resultAppend : List (a, Nat), (a, Nat), List (a, Nat) -> List (a, Nat) where a implements Eq
resultAppend = \list, (elem, elemCount), result ->
    when list is
        [] -> List.append result (elem, elemCount)
        [(baseElem, baseCount), .. as rest] ->
            if elem == baseElem then
                result
                |> List.append (baseElem, baseCount + elemCount)
                |> List.concat rest
            else
                resultAppend rest (elem, elemCount) (List.append result (baseElem, baseCount))

expect
    l1 = [("A", 1), ("B", 1)]
    l2 = [("B", 1), ("C", 1)]
    got = resultConcat l1 l2
    got == [("A", 1), ("B", 2), ("C", 1)]

expect
    got = resultAppend [("A", 1), ("B", 1)] ("B", 1) []
    got == [("A", 1), ("B", 2)]

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

expect
    input = ".?..??#?##????#...?" |> Str.toUtf8
    groups = [4, 1]
    got = doLine input groups
    got == 1

expect
    input = "?.#???#? ???#???#." |> Str.toUtf8
    groups = [2, 1, 4, 1]
    got = doLine input groups
    got == 3

findAllPlacesForGroup = \input, group ->
    findAllPlacesForGroupHelper input group []

findAllPlacesForGroupHelper : List U8, Nat, List (List U8) -> List (List U8)
findAllPlacesForGroupHelper = \input, group, result ->
    if List.len input > 0 then
        when findFirstIndex input group is
            Ok idx ->
                inputFromIdxWithoutOneChar = List.dropFirst input (idx + 1)
                beforeInput = List.takeFirst input (idx + 1)

                newResult = List.append result (List.dropFirst input (idx + group + 1))

                if List.contains beforeInput '#' then
                    newResult
                else
                    findAllPlacesForGroupHelper inputFromIdxWithoutOneChar group newResult

            Err Impossible ->
                result
    else
        result

expect
    input = "???#???#." |> Str.toUtf8
    got = findAllPlacesForGroup input 4
    List.len got == 3

expect
    input = "???.##.\n" |> Str.toUtf8
    got = findAllPlacesForGroup input 1
    got
    == [
        "?.##.\n" |> Str.toUtf8,
        ".##.\n" |> Str.toUtf8,
        "##.\n" |> Str.toUtf8,
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
        if List.countIf sliceGroup isDamaged == group && List.get input group |> unwrap "deep" |> isOperational then
            Ok index
        else
            when input is
                [a, .. as rest] ->
                    if a == '#' then
                        Err Impossible
                    else
                        findFirstIndexHelper rest group (index + 1)

                [] -> crash "impossible"
    else
        Err Impossible

expect
    input = "#???#." |> Str.toUtf8
    got = findFirstIndex input 4
    got == Err Impossible

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

expect
    input = ".##\n" |> Str.toUtf8
    got = findFirstIndex input 1
    got == Err Impossible

isDamaged = \c ->
    c == '#' || c == '?'

isOperational = \c ->
    c == '.' || c == '?' || c == '\n'

expect
    got = part2 exampleInput
    got == "525152"

part2 = \input ->
    input
    |> Str.trim
    |> parseInput
    |> List.map unfold
    |> List.map \(line, groups) ->
        doLine line groups
    |> List.sum
    |> Num.toStr

unfold = \(line, groups) ->
    newLine =
        line
        |> List.dropLast 1
        |> List.repeat 5
        |> List.map \e -> List.append e '?'
        |> List.join
        |> List.dropLast 1
        |> List.append '\n'

    newGroups =
        groups
        |> List.repeat 5
        |> List.join

    (newLine, newGroups)

unwrap = \r, msg ->
    when r is
        Ok v -> v
        _ -> crash "impossible \(msg)"
