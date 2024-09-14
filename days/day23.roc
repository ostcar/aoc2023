app [solution] {
    pf: platform "../platform/main.roc",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.0/je3X2cSdUa6b24fO1SS_vGNS5MwU-a-3r1niP_7iG6k.tar.br",
}

import "day23.input" as puzzleInput : Str
import array2d.Array2D exposing [Array2D]

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
    cols = List.first lines |> unwrap |> Str.countUtf8Bytes
    rows = List.len lines
    initArray = Array2D.init { cols, rows } \_ -> Empty

    List.walkWithIndex
        lines
        initArray
        \state, line, row ->
            List.walkWithIndex
                (Str.toUtf8 line)
                state
                \array, elem, col ->
                    v =
                        when elem is
                            '#' -> Wall
                            '.' -> Empty
                            '>' -> OneWay Right
                            'v' -> OneWay Down
                            '<' -> OneWay Left
                            '^' -> OneWay Up
                            _ -> crash "invalid element"

                    Array2D.set array { row, col } v

findLongest : Map -> U64
findLongest = \map ->
    findLongestHelper ([[{ row: 0, col: 1 }]], 0) map (findGoal map)

#Way : List Array2D.Index

#findLongestHelper : (List Way, U64), Map, Array2D.Index -> U64
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
    { rows, cols } = Array2D.shape map
    { row: rows - 1, col: cols - 2 }

nextSteps = \map, goal, way ->
    currentIndex = List.last way |> unwrap
    directions =
        if currentIndex == { row: 0, col: 1 } then
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
                    Up -> { currentIndex & row: currentIndex.row - 1 }
                    Down -> { currentIndex & row: currentIndex.row + 1 }
                    Left -> { currentIndex & col: currentIndex.col - 1 }
                    Right -> { currentIndex & col: currentIndex.col + 1 }

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
