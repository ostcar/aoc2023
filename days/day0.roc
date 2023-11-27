app "day0"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day0.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1
        Part2 -> part2

part1 = puzzleInput |> Str.trimEnd

part2 =
    part1
    |> Str.toScalars
    |> List.reverse
    |> List.walk
        ""
        (\state, element -> Str.appendScalar state element
            |> Result.withDefault "?")
