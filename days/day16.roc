app "day"
    packages {
        pf: "../platform/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        "day16.input" as puzzleInput : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    .|...\\....
    |.-.\\.....
    .....|-...
    ........|.
    ..........
    .........\\
    ..../.\\\\..
    .-.-/..|..
    .|....-|.\\
    ..//.|....
    """

expect
    got = part1 exampleInput
    got == "46"

part1 = \input ->
    input
    |> parseInput
    |> energiesBeam ({ x: 0, y: 0 }, Right)
    |> Num.toStr

parseInput = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        Str.toUtf8 line
    |> Array2D.fromLists FitShortest
    |> Array2D.map \e ->
        when e is
            '.' -> Empty
            '/' -> Mirror Mirror1
            '\\' -> Mirror Mirror2
            '|' -> UpDown
            '-' -> LeftRight
            _ -> crash "invalid input"

energiesBeam = \beam, start ->
    beam
    |> walkBeam start
    |> List.map .0
    |> listCountUnique

walkBeam = \grid, start ->
    walkBeamHelper grid [start] []

walkBeamHelper = \grid, todo, result ->
    when todo is
        [] -> result
        [(index, direction), .. as rest] ->
            mayNextResult =
                if List.contains result (index, direction) then
                    Err AlreadyDone
                else
                    Ok (List.append result (index, direction))

            when mayNextResult is
                Ok nextResult ->
                    nextTODO =
                        when Array2D.get grid index is
                            Ok Empty ->
                                addNext rest grid index direction

                            Ok (Mirror mirror) ->
                                newDirection = changeDirection direction mirror
                                addNext rest grid index newDirection

                            Ok UpDown ->
                                when direction is
                                    Top | Bottom ->
                                        addNext rest grid index direction

                                    Left | Right ->
                                        addNext rest grid index Top
                                        |> addNext grid index Bottom

                            Ok LeftRight ->
                                when direction is
                                    Top | Bottom ->
                                        addNext rest grid index Left
                                        |> addNext grid index Right

                                    Left | Right ->
                                        addNext rest grid index direction

                            Err OutOfBounds ->
                                crash "index out of bounds"

                    walkBeamHelper grid nextTODO nextResult

                Err AlreadyDone ->
                    walkBeamHelper grid rest result

addNext = \rest, grid, index, direction ->
    nextIdx = nextIndex grid index direction
    listTryAddUnique rest (unwrapFirst (nextIdx, direction))

unwrapFirst = \(mayFirst, second) ->
    when mayFirst is
        Ok first -> Ok (first, second)
        Err err -> Err err

listAddUnique = \list, e ->
    if List.contains list e then
        list
    else
        List.append list e

listCountUnique = \list ->
    List.walk
        list
        []
        listAddUnique
    |> List.len

listTryAddUnique = \list, r ->
    when r is
        Ok e ->
            listAddUnique list e

        Err _ ->
            list

nextIndex = \grid, index, direction ->
    when direction is
        Right ->
            if Array2D.isRowEnd grid index then
                Err Outside
            else
                Ok { index & y: index.y + 1 }

        Left ->
            if Array2D.isRowStart index then
                Err Outside
            else
                Ok { index & y: index.y - 1 }

        Top ->
            if Array2D.isColStart index then
                Err Outside
            else
                Ok { index & x: index.x - 1 }

        Bottom ->
            if Array2D.isColEnd grid index then
                Err Outside
            else
                Ok { index & x: index.x + 1 }

changeDirection = \fromDirection, mirror ->
    when (fromDirection, mirror) is
        (Right, Mirror1) -> Top
        (Right, Mirror2) -> Bottom
        (Left, Mirror1) -> Bottom
        (Left, Mirror2) -> Top
        (Top, Mirror1) -> Right
        (Top, Mirror2) -> Left
        (Bottom, Mirror1) -> Left
        (Bottom, Mirror2) -> Right

expect
    got = part2 exampleInput
    got == "51"

part2 = \input ->
    input
    |> parseInput
    |> walkFromSides
    |> Num.toStr

walkFromSides = \beam ->
    { dimX, dimY } = Array2D.shape beam

    me1 = List.walk
        (List.range { start: At 0, end: Before dimY })
        0
        \state, y ->
            dbg ("top", y, state)

            Num.max
                (energiesBeam beam ({ x: 0, y: y }, Bottom))
                state

    me2 = List.walk
        (List.range { start: At 0, end: Before dimY })
        me1
        \state, y ->
            dbg ("bottom", y, state)

            Num.max
                (energiesBeam beam ({ x: dimX - 1, y: y }, Top))
                state

    me3 = List.walk
        (List.range { start: At 0, end: Before dimX })
        me2
        \state, x ->
            dbg ("left", x, state)

            Num.max
                (energiesBeam beam ({ y: 0, x: x }, Right))
                state

    me4 = List.walk
        (List.range { start: At 0, end: Before dimX })
        me3
        \state, x ->
            dbg ("right", x, state)

            Num.max
                (energiesBeam beam ({ y: dimY - 1, x: x }, Left))
                state

    me4
