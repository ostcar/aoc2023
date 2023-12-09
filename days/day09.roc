app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day09.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45
    """

expect
    got = part1 exampleInput
    got == "114"

part1 = \input ->
    input
    |> parseInput
    |> List.map findNext
    |> List.sum
    |> Num.toStr

parseInput = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \e ->
        e
        |> Str.split " "
        |> List.map Str.toI64
        |> List.map unwrap

findNext = \list ->
    if List.all list \e -> e == 0 then
        0
    else
        when list is
            [first, .. as rest] if List.len rest > 0 ->
                List.walk rest (first, []) \(before, result), elem ->
                    (elem, List.append result (elem - before))
                |> .1
                |> findNext
                |> \n -> (List.last rest |> unwrap) + n

            _ -> crash "list to short"

expect
    got = part2 exampleInput
    got == "2"

part2 = \input ->
    input
    |> parseInput
    |> List.map findBefore
    |> List.sum
    |> Num.toStr

findBefore = \list ->
    if List.all list \e -> e == 0 then
        0
    else
        when list is
            [first, .. as rest] if List.len rest > 0 ->
                List.walk rest (first, []) \(before, result), elem ->
                    (elem, List.append result (elem - before))
                |> .1
                |> findBefore
                |> \n -> first - n

            _ -> crash "list to short"

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"
