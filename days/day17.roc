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
    |> shortestPath 0 3
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

shortestPath = \map, minSteps, maxSteps ->
    queue = [
        (0, { x: 0, y: 0 }, Right, 0),
    ]

    seen = List.withCapacity (Array2D.size map)

    target =
        { dimX, dimY } = Array2D.shape map
        { x: dimX - 1, y: dimY - 1 }

    shortestPathHelper map queue seen target minSteps maxSteps

expect
    map = Array2D.fromLists
        [
            [1, 2, 3],
            [1, 2, 3],
            [1, 2, 3],
        ]
        FitShortest
    got = shortestPath map 0 10
    got == 7

# shortestPathHelper : Array2D U64, List (U64, Array2D.Index, [Top, Bottom, Left, Right], U64), Set (Array2D.Index, [Top, Bottom, Left, Right], U64), Array2D.Index, U64, U64 -> U64
shortestPathHelper = \map, queue, seen, target, minSteps, maxSteps ->
    ((cost, index, direction, numSteps), restQueue) = getShortest queue

    if index == target && numSteps >= minSteps then
        cost
    else if List.contains seen (index, direction, numSteps) then
        shortestPathHelper map restQueue seen target minSteps maxSteps
    else
        newSeen = List.append seen (index, direction, numSteps)
        newQueue = updateQueue restQueue map index direction numSteps minSteps maxSteps cost
        shortestPathHelper map newQueue newSeen target minSteps maxSteps

# sortQueue = \queue ->
#     List.sortWith queue \a, b ->
#         Num.compare a.0 b.0

getShortest = \queue ->
    (minIndex, _) =
        List.walkWithIndex
            queue
            (0, Num.maxU64)
            \(stateIndex, stateCost), elem, index ->
                cost = elem.0
                if cost < stateCost then
                    (index, cost)
                else
                    (stateIndex, stateCost)
    (List.get queue minIndex |> unwrap, List.dropAt queue minIndex)

updateQueue = \queue, map, index, direction, numSteps, minSteps, maxSteps, cost ->
    List.withCapacity 3
    |> \directions -> (
            if numSteps < minSteps then
                directions
            else
                when direction is
                    Left | Right -> List.concat directions [Bottom, Top]
                    Top | Bottom -> List.concat directions [Right, Left]
        )
    |> \directions ->
        if numSteps < maxSteps then
            List.append directions direction
        else
            directions
    |> List.keepOks \newDir ->
        newIndex index map newDir
        |> Result.map \idx -> (idx, newDir)
    |> List.walk
        queue
        \state, (newIdx, newDirection) ->
            newCost = cost + (Array2D.get map newIdx |> Result.withDefault 0)
            List.append state (newCost, newIdx, newDirection, if direction == newDirection then numSteps + 1 else 1)

newIndex = \index, map, direction ->
    when direction is
        Left ->
            if Array2D.isRowStart index then
                Err OutOfRange
            else
                Ok { index & y: index.y - 1 }

        Right ->
            if Array2D.isRowEnd map index then
                Err OutOfRange
            else
                Ok { index & y: index.y + 1 }

        Top ->
            if Array2D.isColStart index then
                Err OutOfRange
            else
                Ok { index & x: index.x - 1 }

        Bottom ->
            if Array2D.isColEnd map index then
                Err OutOfRange
            else
                Ok { index & x: index.x + 1 }

expect
    got = part2 exampleInput
    got == "94"

part2 = \input ->
    input
    |> parseInput
    |> shortestPath 4 10
    |> Num.toStr

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"
