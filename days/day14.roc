app "day"
    packages {
        pf: "../platform/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        "day14.input" as puzzleInput : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    O....#....
    O.OO#....#
    .....##...
    OO.#O....O
    .O.....O#.
    O.#..O.#.#
    ..O..#O..O
    .......O..
    #....###..
    #OO..#....
    """

expect
    got = part1 exampleInput
    got == "136"

part1 = \input ->
    input
    |> parseInput
    |> move North
    |> countPoints
    |> Num.toStr

Bord : Array2D [Rock, Ball, Empty]

parseInput : Str -> Bord
parseInput = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        line
        |> Str.toUtf8
        |> List.map \element ->
            when element is
                'O' -> Ball
                '#' -> Rock
                '.' -> Empty
                _ -> crash "invalid input"
    |> Array2D.fromLists FitShortest

move : Bord, [North, East, South, West] -> Bord
move = \bord, direction ->
    walkOptoins =
        when direction is
            North -> { direction: Forwards, orientation: Rows }
            West -> { direction: Forwards, orientation: Cols }
            South -> { direction: Backwards, orientation: Rows }
            East -> { direction: Backwards, orientation: Cols }

    Array2D.walk
        bord
        bord
        walkOptoins
        \state, elem, idx ->
            when elem is
                Rock | Empty -> state
                Ball ->
                    when findFree state idx direction is
                        Err NotFree -> state
                        Ok newIdx ->
                            state
                            |> Array2D.set newIdx Ball
                            |> Array2D.set idx Empty

findFree = \bord, index, direction ->
    (
        when direction is
            North ->
                if Array2D.isColStart index then
                    Err NotFree
                else
                    Ok { index & x: index.x - 1 }

            East ->
                if Array2D.isRowEnd bord index then
                    Err NotFree
                else
                    Ok { index & y: index.y + 1 }

            South ->
                if Array2D.isColEnd bord index then
                    Err NotFree
                else
                    Ok { index & x: index.x + 1 }

            West ->
                if Array2D.isRowStart index then
                    Err NotFree
                else
                    Ok { index & y: index.y - 1 }
    )
    |> Result.try \newIdx ->
        when Array2D.get bord newIdx is
            Ok elem -> Ok (newIdx, elem)
            Err OutOfBounds -> Err NotFree
    |> Result.try \(newIdx, elem) ->
        when elem is
            Empty ->
                when findFree bord newIdx direction is
                    Err NotFree -> Ok newIdx
                    Ok idxAbove -> Ok idxAbove

            Ball | Rock ->
                Err NotFree

countPoints : Bord -> Nat
countPoints = \bord ->
    { dimX: height } = Array2D.shape bord

    Array2D.walk bord 0 { direction: Forwards } \state, elem, { x } ->
        when elem is
            Ball ->
                state + height - x

            Empty | Rock -> state

expect
    got = part2 exampleInput
    got == "64"

part2 = \input ->
    input
    |> parseInput
    |> moveCircle 1000 # TODO: find the loop
    # moveCircle loopPrefix + (1.000.000.000 - loopPrefix) % loopSize
    |> countPoints
    |> Num.toStr

moveCircle = \bord, num ->
    List.range { start: At 0, end: Before num }
    |> List.walk
        (bord, 0)
        \(state, _), _ ->
            newBord =
                state
                |> move North
                |> move West
                |> move South
                |> move East
            points = countPoints newBord
            dbg points

            (newBord, points)
    |> .0

# debug = \v ->
#     dbg v

#     v

# debugBord = \b ->
#     v =
#         Array2D.toLists b
#         |> List.map \line ->
#             List.map line \e ->
#                 when e is
#                     Ball -> 'O'
#                     Rock -> '#'
#                     Empty -> '.'
#             |> Str.fromUtf8
#             |> unwrap
#         |> Str.joinWith "\n"

#     dbg v

#     b

# unwrap = \r ->
#     when r is
#         Ok v -> v
#         _ -> crash "impossible"
