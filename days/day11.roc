app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day11.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput 1000000

exampleInput =
    """
    ...#......
    .......#..
    #.........
    ..........
    ......#...
    .#........
    .........#
    ..........
    .......#..
    #...#.....
    """

expect
    got = part1 exampleInput
    got == "374"

part1 = \input ->
    input
    |> parseInput
    |> sumDistance 2
    |> Num.toStr

Galaxy : (Nat, Nat)
EmptySpace : (List Nat, List Nat)

parseInput : Str -> (List Galaxy, EmptySpace)
parseInput = \input ->
    galaxyList =
        input
        |> Str.toUtf8
        |> List.walk
            (0, 0, [])
            (\(line, column, list), c ->
                when c is
                    '\n' ->
                        (line + 1, 0, list)

                    '.' ->
                        (line, column + 1, list)

                    '#' ->
                        (line, column + 1, List.append list (line, column))

                    _ -> crash "invalid input"
            )
        |> .2

    lines =
        input
        |> Str.trim
        |> Str.split "\n"

    emptyLines =
        lines
        |> List.mapWithIndex
            (\line, idx ->
                if Str.contains line "#" then
                    Err NotEmpty
                else
                    Ok idx
            )
        |> List.keepOks \r -> r

    width =
        input
        |> Str.toUtf8
        |> List.findFirstIndex \e -> e == '\n'
        |> unwrap

    emptyColumns =
        List.range { start: At 0, end: Before width }
        |> List.map
            (\idx ->
                if List.all lines (\line -> List.get (Str.toUtf8 line) idx == Ok '.') then
                    Ok idx
                else
                    Err NotEmpty
            )
        |> List.keepOks \r -> r
    (galaxyList, (emptyLines, emptyColumns))

galaxytDistance : Galaxy, Galaxy, EmptySpace, Nat -> Nat
galaxytDistance = \(xG1, yG1), (xG2, yG2), (emptyLines, emptyColumns), emptyMultiplier ->
    passesEmptyLine = List.countIf
        emptyLines
        \n -> ((n < xG1 && n > xG2) || (n > xG1 && n < xG2))

    passesEmptyColumn = List.countIf
        emptyColumns
        \n -> ((n < yG1 && n > yG2) || (n > yG1 && n < yG2))

    diff = Num.absDiff xG1 xG2 + Num.absDiff yG1 yG2

    diff + passesEmptyColumn * (emptyMultiplier - 1) + passesEmptyLine * (emptyMultiplier - 1)

sumDistance : (List Galaxy, EmptySpace), Nat -> Nat
sumDistance = \(list, emptySpace), emptyMultiplier ->
    sumDistanceHelper list emptySpace emptyMultiplier 0

sumDistanceHelper : List Galaxy, EmptySpace, Nat, Nat -> Nat
sumDistanceHelper = \list, emptySpace, emptyMultiplier, res ->
    when list is
        [head, .. as tail] if !(List.isEmpty tail) ->
            v = List.walk tail 0 \state, other -> state + (galaxytDistance head other emptySpace emptyMultiplier)
            sumDistanceHelper tail emptySpace emptyMultiplier (res + v)

        _ -> res

expect
    got = part2 exampleInput 10
    got == "1030"

expect
    got = part2 exampleInput 100
    got == "8410"

part2 = \input, emptyMultiplier ->
    input
    |> parseInput
    |> sumDistance emptyMultiplier
    |> Num.toStr

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "unreachable"
