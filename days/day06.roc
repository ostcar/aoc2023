app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day06.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    Time:      7  15   30
    Distance:  9  40  200
    """

expect
    got = part1 exampleInput
    got == "288"

part1 = \input ->
    input
    |> parseInput
    |> List.map \(time, distance) -> fnForTime time distance
    |> List.product
    |> Num.toStr

parseInput : Str -> List (F64, F64)
parseInput = \input ->
    data =
        input
        |> Str.split "\n"
        |> List.map \line ->
            line
            |> Str.split " "
            |> List.dropIf Str.isEmpty
            |> List.dropFirst 1
            |> List.map \e -> e |> Str.toF64 |> unwrap

    List.first data
    |> unwrap
    |> List.mapWithIndex \_, idx ->
        (
            List.get data 0 |> unwrap |> List.get idx |> unwrap,
            List.get data 1 |> unwrap |> List.get idx |> unwrap,
        )

fnForTime : F64, F64 -> U64
fnForTime = \time, distance ->
    # time = distance * x - x²
    # x1, x2 = (-b ± sqrt ( b² - 4ac)) / 2a
    a = -1
    b = time
    c = -distance
    lowest =
        ((-b + Num.sqrt (b * b - 4 * a * c)) / (2 * a))
        |> Num.floor
        |> (\v -> v + 1)
    highest =
        ((-b - Num.sqrt (b * b - 4 * a * c)) / (2 * a))
        |> Num.ceiling
        |> (\v -> v - 1)
    highest - lowest + 1

expect
    got = part2 exampleInput
    got == "71503"

part2 = \input ->
    input
    |> parseInput2
    |> \(time, distance) -> fnForTime time distance
    |> Num.toStr

parseInput2 : Str -> (F64, F64)
parseInput2 = \input ->
    data =
        input
        |> Str.trim
        |> Str.split "\n"
        |> List.map \line ->
            line
            |> Str.split ":"
            |> List.get 1
            |> unwrap
            |> Str.replaceEach " " ""
            |> Str.toF64
            |> unwrap

    (
        List.get data 0 |> unwrap,
        List.get data 1 |> unwrap,
    )

unwrap = \r ->
    when r is
        Ok v -> v
        Err v ->
            dbg
                v

            crash "unreachable"
