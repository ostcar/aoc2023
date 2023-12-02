app "day0"
    packages {
        pf: "../platform/main.roc",
    }
    imports [
        "day01.input" as puzzleInput : Str,
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

examplePart1 =
    """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """

expect part1 examplePart1 == "142"

part1 : Str -> Str
part1 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        graphemes = Str.graphemes line
        mayFirst = List.findFirst graphemes isDigit
        mayLast = List.findLast graphemes isDigit
        when (mayFirst, mayLast) is
            (Ok first, Ok last) ->
                when (Str.toU32 first, Str.toU32 last) is
                    (Ok f, Ok l) ->
                        10 * f + l

                    _ -> crash "found number is not a number..."

            _ -> 0
    |> List.walk 0 (\state, elem -> state + elem)
    |> Num.toStr

isDigit : Str -> Bool
isDigit = \s ->
    Str.toU8 s |> Result.isOk

examplePart2 =
    """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """

expect part2 examplePart2 == "281"

part2 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        first = firstDigit line
        last = lastDigit line
        10 * first + last
    |> List.walk 0 (\state, elem -> state + elem)
    |> Num.toStr

firstDigit : Str -> U32
firstDigit = \s ->
    if Str.startsWithScalar s '1' || Str.startsWith s "one" then
        1
    else if Str.startsWithScalar s '2' || Str.startsWith s "two" then
        2
    else if Str.startsWithScalar s '3' || Str.startsWith s "three" then
        3
    else if Str.startsWithScalar s '4' || Str.startsWith s "four" then
        4
    else if Str.startsWithScalar s '5' || Str.startsWith s "five" then
        5
    else if Str.startsWithScalar s '6' || Str.startsWith s "six" then
        6
    else if Str.startsWithScalar s '7' || Str.startsWith s "seven" then
        7
    else if Str.startsWithScalar s '8' || Str.startsWith s "eight" then
        8
    else if Str.startsWithScalar s '9' || Str.startsWith s "nine" then
        9
    else if Str.isEmpty s then
        crash "empty string"
    else
        s |> Str.graphemes |> List.dropFirst 1 |> Str.joinWith "" |> firstDigit

lastDigit : Str -> U32
lastDigit = \s ->
    if Str.endsWith s "1" || Str.endsWith s "one" then
        1
    else if Str.endsWith s "2" || Str.endsWith s "two" then
        2
    else if Str.endsWith s "3" || Str.endsWith s "three" then
        3
    else if Str.endsWith s "4" || Str.endsWith s "four" then
        4
    else if Str.endsWith s "5" || Str.endsWith s "five" then
        5
    else if Str.endsWith s "6" || Str.endsWith s "six" then
        6
    else if Str.endsWith s "7" || Str.endsWith s "seven" then
        7
    else if Str.endsWith s "8" || Str.endsWith s "eight" then
        8
    else if Str.endsWith s "9" || Str.endsWith s "nine" then
        9
    else if Str.isEmpty s then
        crash "empty string"
    else
        s |> Str.graphemes |> List.dropLast 1 |> Str.joinWith "" |> lastDigit
