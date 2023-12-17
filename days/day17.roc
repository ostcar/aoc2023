app "day"
    packages {
        pf: "../platform/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        "day17.input" as puzzleInput : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    2413432311323
    3215453535623
    3255245654254
    3446585845452
    4546657867536
    1438598798454
    4457876987766
    3637877979653
    4654967986887
    4564679986453
    1224686865563
    2546548887735
    4322674655533
    """

expect
    got = part1 exampleInput
    got == "102"

part1 = \input ->
    input
    |> parseInput
    |> shortestPath Part1
    |> Num.toStr

parseInput : Str -> Array2D.Array2D U64
parseInput = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        line
        |> Str.toUtf8
        |> List.map \c ->
            Num.toU64 (c - '0')
    |> Array2D.fromLists FitShortest

Direction : (U8, [Left, Right, Top, Bottom])
MapResult : Array2D.Array2D [Visited U64, Unvisited U64 Direction]

shortestPath = \map, part ->
    firstValue = Array2D.get map { x: 0, y: 0 } |> unwrap
    startNode =
        Array2D.repeat (Unvisited Num.maxU64 (0, Left)) (Array2D.shape map)
        |> Array2D.set { x: 0, y: 0 } (Unvisited firstValue (1, Right))

    mapResult = shortestPathHelper map { x: 0, y: 0 } startNode part

    dbg debugNodes mapResult

    { dimX, dimY } = Array2D.shape mapResult
    endIndex = { x: dimX - 1, y: dimY - 1 }
    when Array2D.get mapResult endIndex is
        Ok (Visited n) -> n
        _ ->
            dbg Array2D.get mapResult endIndex

            crash "this does not look good"

shortestPathHelper : Array2D.Array2D U64, Array2D.Index, MapResult, [Part1, Part2] -> MapResult
shortestPathHelper = \map, curIndex, nodes, part ->
    when Array2D.get nodes curIndex is
        Ok (Unvisited curVal direction) ->
            updated =
                List.walk
                    (getNeighbors map curIndex direction part)
                    nodes
                    \state, (neighborIndex, newDirection) ->
                        Array2D.update
                            state
                            neighborIndex
                            \neighborValue ->
                                newDistance = curVal + (Array2D.get map neighborIndex |> unwrap)
                                when neighborValue is
                                    Visited _ ->
                                        neighborValue

                                    Unvisited oldDistance _ ->
                                        if oldDistance > newDistance then
                                            Unvisited newDistance newDirection
                                        else
                                            neighborValue
                |> Array2D.set
                    curIndex
                    (Visited curVal)

            when findSmalestIndex updated is
                Ok idx ->
                    shortestPathHelper map idx updated part

                Err NotFound ->
                    updated

        _ ->
            crash "impossible value in nodes"

getNeighbors = \map, index, (directionCount, direction), part ->
    when part is
        Part1 ->
            List.withCapacity 4
            |> \list -> if Array2D.isRowStart index || direction == Left && directionCount > 3 then
                    list
                else
                    List.append list ({ index & y: index.y - 1 }, updateDirection directionCount direction Left)
            |> \list -> if Array2D.isRowEnd map index || direction == Right && directionCount > 3 then
                    list
                else
                    List.append list ({ index & y: index.y + 1 }, updateDirection directionCount direction Right)
            |> \list -> if Array2D.isColStart index || direction == Top && directionCount > 3 then
                    list
                else
                    List.append list ({ index & x: index.x - 1 }, updateDirection directionCount direction Top)
            |> \list -> if Array2D.isColEnd map index || direction == Bottom && directionCount > 3 then
                    list
                else
                    List.append list ({ index & x: index.x + 1 }, updateDirection directionCount direction Bottom)

        Part2 ->
            if directionCount < 4 then
                continueDirection map index directionCount direction
            else
                List.withCapacity 4
                |> \list -> if Array2D.isRowStart index || direction == Left && directionCount > 10 then
                        list
                    else
                        List.append list ({ index & y: index.y - 1 }, updateDirection directionCount direction Left)
                |> \list -> if Array2D.isRowEnd map index || direction == Right && directionCount > 10 then
                        list
                    else
                        List.append list ({ index & y: index.y + 1 }, updateDirection directionCount direction Right)
                |> \list -> if Array2D.isColStart index || direction == Top && directionCount > 10 then
                        list
                    else
                        List.append list ({ index & x: index.x - 1 }, updateDirection directionCount direction Top)
                |> \list -> if Array2D.isColEnd map index || direction == Bottom && directionCount > 10 then
                        list
                    else
                        List.append list ({ index & x: index.x + 1 }, updateDirection directionCount direction Bottom)

continueDirection = \map, index, count, direction ->
    when direction is
        Left if !(Array2D.isRowStart index) ->
            [({ index & y: index.y - 1 }, (count + 1, direction))]

        Right if !(Array2D.isRowEnd map index) ->
            [({ index & y: index.y + 1 }, (count + 1, direction))]

        Top if !(Array2D.isColStart index) ->
            [({ index & x: index.x - 1 }, (count + 1, direction))]

        Bottom if !(Array2D.isColEnd map index) ->
            [({ index & x: index.x + 1 }, (count + 1, direction))]

        _ -> []

updateDirection = \directionCount, oldDirection, newDirection ->
    if oldDirection == newDirection then
        (directionCount + 1, oldDirection)
    else
        (1, newDirection)

findSmalestIndex = \mapResult ->
    smalest =
        Array2D.walk
            mapResult
            (Num.maxU64, { x: 0, y: 0 })
            { direction: Forwards }
            \(state, stateIdx), value, index ->
                when value is
                    Unvisited v _ if v < state ->
                        (v, index)

                    _ -> (state, stateIdx)
    if smalest.0 == Num.maxU64 then
        Err NotFound
    else
        Ok smalest.1

expect
    got = part2 exampleInput
    got == "94"

part2 = \input ->
    input
    |> parseInput
    |> shortestPath Part2
    |> Num.toStr

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "inpossible"

debugNodes : Array2D.Array2D [Visited U64, Unvisited U64 Direction] -> Str
debugNodes = \nodes ->
    nodes
    |> Array2D.toLists
    |> List.map \line ->
        List.map line \c ->
            when c is
                Visited v ->
                    formattedNum v

                Unvisited _ _ -> " uuu"
        |> Str.joinWith ""
    |> Str.joinWith "\n"
    |> Str.withPrefix "\n"

formattedNum = \n ->
    s = Num.toStr n
    Str.concat (Str.repeat " " (4 - Str.countUtf8Bytes s)) s
