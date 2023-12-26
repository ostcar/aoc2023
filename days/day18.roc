app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports [
        "day18.input" as puzzleInput : Str,
        Inspect.{ Inspect, Inspector, InspectFormatter },
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
    |> countPlaces
    |> Num.toStr

parseInput = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        when Str.split line " " is
            [a, b, _] ->
                m =
                    when Str.toI64 b is
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

countPlaces = \instructions ->
    List.walk
        instructions
        (0,0,0)
        \(y, result, meterSum), (direction, meter) ->
            when direction is
                Right -> (y + meter, result, meterSum + meter)
                Left -> (y - meter, result, meterSum + meter)
                Down -> (y, result + y * meter, meterSum + meter)
                Up -> (y, result - y * meter, meterSum + meter)
    |> \(_, v1, v2) ->
        v1 + v2 // 2 + 1

expect
    got = part2 exampleInput
    got == "952408144115"

part2 = \input ->
    input
    |> parseInput2
    |> countPlaces
    |> Num.toStr

parseInput2 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        when Str.split line " " is
            [_, _, color] ->
                Str.toUtf8 color
                |> List.dropFirst 2
                |> List.dropLast 1
                |> colorToDirectionMeter

            _ -> crash "invalid line"

expect
    got = parseInput2 exampleInput
    got
    == [
        (Right, 461937),
        (Down, 56407),
        (Right, 356671),
        (Down, 863240),
        (Right, 367720),
        (Down, 266681),
        (Left, 577262),
        (Up, 829975),
        (Left, 112010),
        (Down, 829975),
        (Left, 491645),
        (Up, 686074),
        (Left, 5411),
        (Up, 500254),
    ]

colorToDirectionMeter = \input ->
    when input is
        [.. as hexMeter, d] ->
            meter = parseHexToNum hexMeter
            when d is
                '0' -> (Right, meter)
                '1' -> (Down, meter)
                '2' -> (Left, meter)
                '3' -> (Up, meter)
                _ ->
                    crash "invalid direction \(Inspect.toStr d)"

        _ -> crash "invalid color"

expect
    got = colorToDirectionMeter ("70c710" |> Str.toUtf8)
    got == (Right, 461937)

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
                    _ -> crash "invalid element \(Inspect.toStr elem)"
            state * 16 + n
