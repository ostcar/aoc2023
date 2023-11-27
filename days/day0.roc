app "day0"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day0.input" as puzzleInput : Str]
    provides [solution] to pf

solution = {
    part1: \_ -> part1,
    part2: \_ -> part2,
}

part1 = puzzleInput |> Str.trimEnd

part2 = 
    part1
    |> Str.trimEnd
    |> Str.toScalars
    |> List.reverse
    |> List.walk
        ""
        (\state, element -> Str.appendScalar state element
            |> Result.withDefault "?")
