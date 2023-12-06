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
    got == "foo" # "288"

part1 = \input ->
    # [
    #     (7,9),
    #     (15,40),
    #     (30,200),
    # ]
    # [
    #     (63,411),
    #     (78,1274),
    #     (94,2047),
    #     (68,1035),
    # ]
    # [(71530, 940200)]
    [(63789468, 411127420471035)]
    |> List.map
        (\(time, distance) ->
            fn = fnForTime time
            (lowest, highest) = fn distance
            highest - lowest + 1
        )
    |> List.product
    |> Num.toStr

# parseInput = \input ->
#     lines = Str.split "\n"
#     times =

fnForTime : F64 -> (F64 -> (U64, U64))
fnForTime = \time ->
    \y ->
        # y = 7x - x²
        a = -1
        b = time
        c = -1 * y
        x1 = (-b + Num.sqrt (b * b - 4 * a * c)) / (2 * a)
        x2 = (-b - Num.sqrt (b * b - 4 * a * c)) / (2 * a)
        dbg
            (x1, x2)

        r = (x1 |> Num.floor |> (\v -> v + 1), x2 |> Num.ceiling |> (\v -> v - 1))
        dbg
            r

        r

expect
    fn = fnForTime 7
    (got1, got2) = fn 9
    got1 == 2 && got2 == 5

part2 = \input ->
    "Not implemented yet"

debug = \v ->
    dbg
        v

    v
