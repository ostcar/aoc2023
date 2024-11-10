app [solution] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.2/2Nf8SjH56jqpVp0uor3rqpUxS6ZuCDfeti_nzMn3_T4.tar.br",
}

import "day01.input" as puzzleInput : Str

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

expect part1 examplePart1 == ("142" |> Str.toUtf8)

part1 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        chars = Str.toUtf8 line
        mayFirst = List.findFirst chars isDigit
        mayLast = List.findLast chars isDigit
        when (mayFirst, mayLast) is
            (Ok first, Ok last) ->
                10 * (first |> toNum) + (last |> toNum)

            _ -> 0
    |> List.sum
    |> Num.toStr
    |> Str.toUtf8

isDigit : U8 -> Bool
isDigit = \c ->
    c >= '0' && c <= '9'

toNum : U8 -> U64
toNum = \c ->
    (c - '0')
    |> Num.toU64

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

expect part2 examplePart2 == ("281" |> Str.toUtf8)

part2 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        first = firstDigit (line |> Str.toUtf8)
        last = lastDigit (line |> Str.toUtf8)
        10 * first + last
    |> List.walk 0 (\state, elem -> state + elem)
    |> Num.toStr
    |> Str.toUtf8

firstDigit : List U8 -> U32
firstDigit = \s ->
    first = s |> List.first
    if first == Ok '1' || List.startsWith s ("one" |> Str.toUtf8) then
        1
    else if first == Ok '2' || List.startsWith s ("two" |> Str.toUtf8) then
        2
    else if first == Ok '3' || List.startsWith s ("three" |> Str.toUtf8) then
        3
    else if first == Ok '4' || List.startsWith s ("four" |> Str.toUtf8) then
        4
    else if first == Ok '5' || List.startsWith s ("five" |> Str.toUtf8) then
        5
    else if first == Ok '6' || List.startsWith s ("six" |> Str.toUtf8) then
        6
    else if first == Ok '7' || List.startsWith s ("seven" |> Str.toUtf8) then
        7
    else if first == Ok '8' || List.startsWith s ("eight" |> Str.toUtf8) then
        8
    else if first == Ok '9' || List.startsWith s ("nine" |> Str.toUtf8) then
        9
    else if List.isEmpty s then
        crash "empty string"
    else
        s |> List.dropFirst 1 |> firstDigit

lastDigit : List U8 -> U32
lastDigit = \s ->
    last = s |> List.last
    if last == Ok '1' || List.endsWith s ("one" |> Str.toUtf8) then
        1
    else if last == Ok '2' || List.endsWith s ("two" |> Str.toUtf8) then
        2
    else if last == Ok '3' || List.endsWith s ("three" |> Str.toUtf8) then
        3
    else if last == Ok '4' || List.endsWith s ("four" |> Str.toUtf8) then
        4
    else if last == Ok '5' || List.endsWith s ("five" |> Str.toUtf8) then
        5
    else if last == Ok '6' || List.endsWith s ("six" |> Str.toUtf8) then
        6
    else if last == Ok '7' || List.endsWith s ("seven" |> Str.toUtf8) then
        7
    else if last == Ok '8' || List.endsWith s ("eight" |> Str.toUtf8) then
        8
    else if last == Ok '9' || List.endsWith s ("nine" |> Str.toUtf8) then
        9
    else if List.isEmpty s then
        crash "empty string"
    else
        s |> List.dropLast 1 |> lastDigit
