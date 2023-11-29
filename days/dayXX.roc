app "day0"
    packages {
        pf: "../platform/main.roc",
    }
    # imports ["day.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1
        Part2 -> part2

part1 = "Not implemented yet"

part2 = "Not implemented yet"
