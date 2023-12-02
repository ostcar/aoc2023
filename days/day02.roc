app "day0"
    packages {
        pf: "../platform/main.roc",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        "day02.input" as puzzleInput : Str,
        parser.String.{ digits, parseStr, string },
        parser.Core.{ oneOf, apply, const, skip, maybe, map, oneOrMore },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """
    |> Str.trimStart

expect
    got = part1 exampleInput
    got == "8"

part1 = \input ->
    parseInput input
    |> List.map checkGame
    |> List.sum
    |> Num.toStr

parseInput = \input ->
    when parseStr (oneOrMore gameParser) input is
        Ok parsed -> parsed
        _ -> crash "Invalid input"

checkGame = \(gameNr, game) ->
    isOk = List.all
        game
        (\colorSet ->
            colors = mapColorSet colorSet
            Bool.not (colors.red > 12 || colors.green > 13 || colors.blue > 14)
        )
    if isOk then
        gameNr
    else
        0

mapColorSet = \rawColorSet ->
    List.walk
        rawColorSet
        { green: 0, red: 0, blue: 0 }
        (\state, (v, color) ->
            when color is
                Green -> { state & green: state.green + v }
                Red -> { state & red: state.red + v }
                Blue -> { state & blue: state.blue + v }
        )

colorParser =
    const (\n -> \color -> (n, color))
    |> skip (string " ")
    |> apply digits
    |> skip (string " ")
    |> apply
        (
            oneOf [
                string "blue" |> map \_ -> Blue,
                string "red" |> map \_ -> Red,
                string "green" |> map \_ -> Green,
            ]
        )
    |> skip (maybe (string ","))

colorSetParser =
    (oneOrMore colorParser)
    |> skip (maybe (string ";"))

gameParser =
    const (\n -> \colors -> (n, colors))
    |> skip (string "Game ")
    |> apply digits
    |> skip (string ":")
    |> apply (oneOrMore colorSetParser)
    |> skip (maybe (string "\n"))

expect
    got = part2 exampleInput
    got == "2286"

part2 = \input ->
    parseInput input
    |> List.map rateGame
    |> List.sum
    |> Num.toStr

rateGame = \(_, game) ->
    List.walk
        game
        { green: 0, red: 0, blue: 0 }
        (\state, colorSet ->
            colors = mapColorSet colorSet
            maxColorSet state colors
        )
    |> mulcolorSet

maxColorSet = \c1, c2 -> {
    green: Num.max c1.green c2.green,
    blue: Num.max c1.blue c2.blue,
    red: Num.max c1.red c2.red,
}

mulcolorSet = \c ->
    c.green * c.red * c.blue
