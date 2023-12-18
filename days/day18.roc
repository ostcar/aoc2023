app "day"
    packages {
        pf: "../platform/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        "day18.input" as puzzleInput : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    R 6 (#70c710)
    D 5 (#0dc571)
    L 2 (#5713f0)
    D 2 (#d2c081)
    R 2 (#59c680)
    D 2 (#411b91)
    L 5 (#8ceee2)
    U 2 (#caa173)
    L 1 (#1b58a2)
    U 2 (#caa171)
    R 2 (#7807d2)
    U 3 (#a77fa3)
    L 2 (#015232)
    U 2 (#7a21e3)
    """

expect
    got = part1 exampleInput
    got == "62"

part1 = \input ->
    input
    |> parseInput
    |> instructionsToMap
    |> printMap
    |> fillOutside [{ x: 0, y: 0 }] # |> printMap
    |> countMap
    |> Num.toStr

parseInput = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        when Str.split line " " is
            [a, b, _] ->
                m =
                    when Str.toU64 b is
                        Ok n -> n
                        _ -> crash "invalid number for meter"

                d =
                    when a is
                        "L" -> Left
                        "R" -> Right
                        "U" -> Up
                        "D" -> Down
                        _ -> crash "invalid char for direction"

                (d, m)

            _ -> crash "invalid line"

instructionsToMap = \instructions ->
    (start, shape) = getStartAndShape instructions

    array = Array2D.init shape (\_ -> Empty) |> Array2D.set start Dig
    List.walk
        instructions
        (start, array)
        \(cur1, state1), (direction, meter) ->
            List.walk
                (List.range { start: At 0, end: Before meter })
                (cur1, state1)
                \(cur2, state2), _ ->
                    newCur =
                        when direction is
                            Right -> { cur2 & y: cur2.y + 1 }
                            Left -> { cur2 & y: cur2.y - 1 }
                            Down -> { cur2 & x: cur2.x + 1 }
                            Up -> { cur2 & x: cur2.x - 1 }
                    (
                        newCur,
                        Array2D.set state2 newCur Dig,
                    )
    |> .1

getStartAndShape = \instractions ->
    List.walk
        instractions
        ((0, 0, 0), (0, 0, 0))
        \((curX, maxX, minX), (curY, maxY, minY)), (direction, meter) ->
            when direction is
                Right ->
                    y = curY + (meter |> Num.toI64)
                    ((curX, maxX, minX), (y, Num.max y maxY, Num.min y minY))

                Left ->
                    y = curY - (meter |> Num.toI64)
                    ((curX, maxX, minX), (y, Num.max y maxY, Num.min y minY))

                Down ->
                    x = curX + (meter |> Num.toI64)
                    ((x, Num.max x maxX, Num.min x minX), (curY, maxY, minY))

                Up ->
                    x = curX - (meter |> Num.toI64)
                    ((x, Num.max x maxX, Num.min x minX), (curY, maxY, minY))
    |> \((_, maxX, minX), (_, maxY, minY)) ->
        ({ x: -minX + 1 |> Num.toNat, y: -minY + 1 |> Num.toNat }, { dimX: (maxX - minX + 3) |> Num.toNat, dimY: (maxY - minY + 3) |> Num.toNat })

fillOutside = \mace, nextPositions ->
    when nextPositions is
        [] -> mace
        [index, .. as next] ->
            updatedMace = Array2D.set mace index Outside
            newNextPositions =
                [Up, Down, Left, Right]
                |> List.walk
                    next
                    (\state, direction ->
                        when nextPositionSafe updatedMace index direction is
                            Ok nextIndex ->
                                if (Array2D.get updatedMace nextIndex |> unwrap) != Empty then
                                    state
                                else
                                    listAddUnique state nextIndex

                            Err OutOfBound ->
                                state

                    )

            fillOutside updatedMace newNextPositions

nextPositionSafe = \mace, index, direction ->
    when direction is
        Up ->
            if Array2D.isColStart index then
                Err OutOfBound
            else
                Ok ({ index & x: index.x - 1 })

        Down ->
            if Array2D.isColEnd mace index then
                Err OutOfBound
            else
                Ok ({ index & x: index.x + 1 })

        Left ->
            if Array2D.isRowStart index then
                Err OutOfBound
            else
                Ok ({ index & y: index.y - 1 })

        Right ->
            if Array2D.isRowEnd mace index then
                Err OutOfBound
            else
                Ok ({ index & y: index.y + 1 })

countMap = \array ->
    Array2D.countIf array \e ->
        e == Empty || e == Dig

expect
    got = part2 exampleInput
    got == "952408144115"

part2 = \input ->
    input
    |> parseInput2
    |> countByInstructions 0
    |> Num.toStr

parseInput2 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        when Str.split line " " is
            [_, _, color] ->
                meter =
                    Str.toUtf8 color
                    |> List.dropFirst 2
                    |> List.dropLast 2
                    |> parseHexToNum

                Str.toUtf8 color
                |> List.dropLast 1
                |> List.last
                |> unwrap
                |> \c ->
                    when c is
                        '0' -> (DimY, meter)
                        '1' -> (DimX, meter)
                        '2' -> (DimY, -meter)
                        '3' -> (DimX, -meter)
                        _ -> crash "invalid direction"

            _ -> crash "invalid line"

parseHexToNum = \list ->
    List.walk
        list
        0
        \state, elem ->
            n =
                when elem is
                    '0' -> 0
                    '1' -> 1
                    '2' -> 2
                    '3' -> 3
                    '4' -> 4
                    '5' -> 5
                    '6' -> 6
                    '7' -> 7
                    '8' -> 8
                    '9' -> 9
                    'a' -> 10
                    'b' -> 11
                    'c' -> 12
                    'd' -> 13
                    'e' -> 14
                    'f' -> 15
                    _ -> crash "invalid element"
            state * 16 + n

countByInstructions = \instructions, result ->
    when instructions is
        [(_, v1), (_, v2), _, _] ->
            dbg v1 * v2

            result + v1 * v2

        _ ->
            (newInstructions, value) = reduceStep instructions
            countByInstructions (removeDouble newInstructions) (result + value)

reduceStep = \instructions ->
    when instructions is
        [(d1, v1), (d2, v2), (d3, v3), .. as rest] if (d1 == d3 && otherSign v1 v3) ->
            dbg ((d1, v1), (d2, v2), (d3, v3))

            v = (List.concat [(d2, v2), (d1, v1 + v3)] rest, v2 * absSmaller v1 v3)
            dbg v

            v

        [first, .. as rest] ->
            (list, value) = reduceStep rest
            (List.prepend list first, value)

        [] ->
            ([], 0)

removeDouble = \instructions ->
    when instructions is
        [(d1, v1), (d2, v2), .. as rest] if d1 == d2 ->
            List.prepend rest (d1, v1 + v2)

        [first, .. as rest] ->
            List.prepend (removeDouble rest) first

        [] ->
            []

expect
    input = [
        (DimY, 10),
        (DimX, 10),
        (DimY, 3),
        (DimX, -5),
    ]
    (instructions, value) = input |> reduceStep
    got = (instructions |> removeDouble, value)

    got == ([(DimY, 13), (DimX, 5)], 15)

absSmaller = \v1, v2 ->
    Num.min (Num.abs v1) (Num.abs v2)

otherSign = \v1, v2 ->
    (v1 < 0) != (v2 < 0)

debug = \v ->
    dbg v

    v

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"

printMap = \map ->
    v =
        Array2D.toLists map
        |> List.map \line ->
            line
            |> List.map \c ->
                when c is
                    Empty -> '.'
                    Dig -> '#'
                    Outside -> '_'
            |> Str.fromUtf8
            |> unwrap
        |> Str.joinWith "\n"
        |> Str.withPrefix "\n"
    dbg v

    map

listAddUnique = \list, e ->
    if List.contains list e then
        list
    else
        List.append list e
