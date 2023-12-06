app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    """

expect
    got = part1 exampleInput
    got == ""

part1 = \input ->
    "Not implemented yet \(input)"

part2 = \input ->
    "Not implemented yet \(input)"
