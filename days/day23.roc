app "day"
    packages {
        pf: "../platform/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        "day23.input" as puzzleInput : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    #.#####################
    #.......#########...###
    #######.#########.#.###
    ###.....#.>.>.###.#.###
    ###v#####.#v#.###.#.###
    ###.>...#.#.#.....#...#
    ###v###.#.#.#########.#
    ###...#.#.#.......#...#
    #####.#.#.#######.#.###
    #.....#.#.#.......#...#
    #.#####.#.#.#########v#
    #.#...#...#...###...>.#
    #.#.#v#######v###.###v#
    #...#.>.#...>.>.#.###.#
    #####v#.#.###v#.#.###.#
    #.....#...#...#.#.#...#
    #.#########.###.#.#.###
    #...###...#...#...#.###
    ###.###.#.###v#####v###
    #...#...#.#.>.>.#.>.###
    #.###.###.#.###.#.#v###
    #.....###...###...#...#
    #####################.#
    """

expect
    got = part1 exampleInput
    got == "94"

part1 = \input ->
    input
    |> parseInput
    |> findLongest
    |> Num.toStr

Map : Array2D [Empty, Wall, OneWay [Up, Left, Down, Right]]

parseInput : Str -> Map
parseInput = \input ->
    lines = input |> Str.trim |> Str.split "\n"
    dimY = List.first lines |> unwrap |> Str.countUtf8Bytes
    dimX = List.len lines
    initArray = Array2D.init { dimY, dimX } \_ -> Empty

    List.walkWithIndex
        lines
        initArray
        \state, line, x ->
            List.walkWithIndex
                (Str.toUtf8 line)
                state
                \array, elem, y ->
                    v =
                        when elem is
                            '#' -> Wall
                            '.' -> Empty
                            '>' -> OneWay Right
                            'v' -> OneWay Down
                            '<' -> OneWay Left
                            '^' -> OneWay Up
                            _ -> crash "invalid element"

                    Array2D.set array { x, y } v

findLongest : Map -> U64
findLongest = \map ->
    findLongestHelper ([[{ x: 0, y: 1 }]], 0) map (findGoal map)

Way : List Array2D.Index

findLongestHelper : (List Way, U64), Map, Array2D.Index -> U64
findLongestHelper = \(cur, found), map, goal ->
    if List.len cur == 0 then
        found
    else
        List.walk
            cur
            ([], found)
            \state, curWay ->
                List.walk
                    (nextSteps map goal curWay)
                    state
                    \(next, foundState), elem ->
                        when elem is
                            Finish n ->
                                (next, n)

                            GoOn list ->
                                (List.append next list, foundState)
        |> findLongestHelper map goal

findGoal = \map ->
    { dimX, dimY } = Array2D.shape map
    { x: dimX - 1, y: dimY - 2 }

nextSteps = \map, goal, way ->
    currentIndex = List.last way |> unwrap
    directions =
        if currentIndex == { x: 0, y: 1 } then
            [Down]
        else
            when Array2D.get map currentIndex is
                Ok (OneWay direction) -> [direction]
                _ -> [Up, Right, Down, Left]

    List.walk
        directions
        []
        \state, direction ->
            nextIdx =
                when direction is
                    Up -> { currentIndex & x: currentIndex.x - 1 }
                    Down -> { currentIndex & x: currentIndex.x + 1 }
                    Left -> { currentIndex & y: currentIndex.y - 1 }
                    Right -> { currentIndex & y: currentIndex.y + 1 }

            if nextIdx == goal then
                List.append state (Finish (List.len way |> Num.toU64))
            else
                when Array2D.get map nextIdx is
                    Ok Empty | Ok (OneWay _) ->
                        if List.contains way nextIdx then
                            state
                        else
                            List.append state (GoOn (List.append way nextIdx))

                    _ ->
                        state

expect
    got = part2 exampleInput
    got == "154"

part2 = \input ->
    input
    |> parseInput
    |> convertForPart2
    |> findLongest
    |> Num.toStr

convertForPart2 = \input ->
    Array2D.map input \e ->
        when e is
            OneWay _ -> Empty
            _ -> e

unwrap = \r ->
    when r is
        Ok v -> v
        Err err ->
            dbg err

            crash "impossible"
