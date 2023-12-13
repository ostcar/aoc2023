app "day"
    packages {
        pf: "../platform/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        "day13.input" as puzzleInput : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    #.##..##.
    ..#.##.#.
    ##......#
    ##......#
    ..#.##.#.
    ..##..##.
    #.#.##.#.

    #...##..#
    #....#..#
    ..##..###
    #####.##.
    #####.##.
    ..##..###
    #....#..#
    """

expect
    got = part1 exampleInput
    got == "405"

part1 = \input ->
    input
    |> Str.trim
    |> Str.split "\n\n"
    |> List.map (\shape -> doShape shape findReflectionPart1)
    |> List.sum
    |> Num.toStr

doShape = \input, findReflection ->
    lines =
        input
        |> Str.split "\n"
        |> List.map Str.toUtf8

    findReflection lines
    |> Result.map \x -> x * 100
    |> Result.onErr \_ ->
        Array2D.fromLists lines FitShortest
        |> Array2D.rotateClockwise
        |> Array2D.toLists
        |> findReflection
    |> unwrap

expect
    input =
        """
        #####...####.#.
        ##..###..#..#.#
        ...#.###.....#.
        ...#.###.....#.
        ##..###..#..#.#
        #####...####.#.
        ##....####.....
        #..##.....#..#.
        #..##.....#..#.
        ##....####.....
        #####...######.
        ##..###..#..#.#
        ...#.###.....#.
        """
    got = doShape input findReflectionPart1
    got == 300

findReflectionPart1 = \lines ->
    List.findFirst
        (List.range { start: At 0, end: At ((List.len lines) - 2) })
        (\idx ->
            checkReflection lines idx 0 NoIgnore
        )
    |> Result.map (\n -> n + 1)

checkReflection = \lines, idx, gab, ignore ->
    if idx < gab then
        Bool.true
    else
        when ignore is
            Ignore thisIdx if thisIdx == idx - gab ->
                checkReflection lines idx (gab + 1) ignore

            NoIgnore | Ignore _ ->
                when (List.get lines (idx - gab), List.get lines (idx + 1 + gab)) is
                    (Ok line1, Ok line2) ->
                        if line1 == line2 then
                            checkReflection lines idx (gab + 1) ignore
                        else
                            Bool.false

                    _ ->
                        Bool.true

part2 = \input ->
    input
    |> Str.trim
    |> Str.split "\n\n"
    |> List.map (\shape -> doShape shape findReflectionPart2)
    |> List.sum
    |> Num.toStr

expect
    got = part2 exampleInput
    got == "400"

expect
    input =
        """
        .####..#..#..##
        ..##...#..#....
        ......###...###
        ......##.##..#.
        #....#.#.#.#...
        ######.###...##
        .####.#.#..##.#
        .......#.#..#..
        .......#.#..#..
        .####.#.#..##.#
        ######.###...##
        #....#.#.#.#...
        ......##.##.##.
        """

    got = doShape input findReflectionPart2
    got == 800

findReflectionPart2 = \lines ->
    List.range { start: At 0, end: At ((List.len lines) - 2) }
    |> List.walkUntil
        (Err NotFound)
        (\_, idx ->
            when checkReflectionOneDiff lines idx is
                Ok v -> Break (Ok v)
                Err NotFound -> Continue (Err NotFound)
        )
    |> Result.map (\n -> n + 1)

checkReflectionOneDiff = \lines, idx ->
    rest = List.takeLast lines ((List.len lines) - idx - 1)
    line = List.get lines idx |> unwrap

    List.mapWithIndex
        rest
        (
            \otherLine, innerIdx ->
                if innerIdx % 2 != 0 then
                    Err NotFound
                else if oneDiff line otherLine then
                    e = (innerIdx + idx + 1)
                    Ok ((idx + e) // 2)
                else
                    Err NotFound
        )
    |> List.keepOks \r -> r
    |> List.findFirst
        (\e ->
            checkReflection lines e 0 (Ignore idx))

oneDiff = \l1, l2 ->
    List.map2
        l1
        l2
        (\e1, e2 ->
            e1 == e2
        )
    |> List.countIf \a -> a
    |> (\sum -> sum == (List.len l1) - 1)

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"
