app [solution] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.2/2Nf8SjH56jqpVp0uor3rqpUxS6ZuCDfeti_nzMn3_T4.tar.br",
}

import "day03.input" as puzzleInput : Str

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput |> Str.toUtf8
        Part2 -> part2 puzzleInput |> Str.toUtf8

exampleInput =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """
    |> Str.trimStart

expect
    got = part1 exampleInput
    got == "4361"

part1 = \input ->
    input
    |> findNumbers
    |> List.keepIf (\n -> isPartNumber n input)
    |> List.map .number
    |> List.sum
    |> Num.toStr

widthInput = \input ->
    input
    |> Str.toUtf8
    |> List.findFirstIndex \e -> e == '\n'
    |> Result.withDefault 0

expect widthInput exampleInput == 10

Number : { number : U32, width : I32, height : I32, length : I32 }

findNumbers : Str -> List Number
findNumbers = \input ->
    width = (widthInput input) + 1

    Str.toUtf8 input
    |> List.walkWithIndex
        (NotInNumber, [])
        (\(inNumber, numbers), elem, index ->
            when (toDigit elem, inNumber) is
                (Ok digit, NotInNumber) ->
                    (
                        InNumber,
                        List.append numbers {
                            number: digit,
                            width: index % width |> Num.toI32,
                            height: index // width |> Num.toI32,
                            length: 1 |> Num.toI32,
                        },
                    )

                (Ok digit, InNumber) ->
                    numbersWithoutLast = List.dropLast numbers 1
                    lastNumber = List.last numbers |> mustOk

                    (
                        InNumber,
                        List.append
                            numbersWithoutLast
                            { lastNumber &
                                number: lastNumber.number * 10 + digit,
                                length: lastNumber.length + 1,
                            },
                    )

                _ ->
                    (NotInNumber, numbers)
        )
    |> \(_, numbers) -> numbers

toDigit = \c ->
    if c >= '0' && c <= '9' then
        Ok (c - '0' |> Num.toU32)
    else
        Err NotANumber

mustOk = \r ->
    when r is
        Ok a -> a
        Err _ -> crash "unreachable"

isPartNumber : Number, Str -> Bool
isPartNumber = \number, input ->
    width = (widthInput input) + 1 |> Num.toI32

    lines = List.range { start: At (number.height - 1), end: At (number.height + 1) }
    columns = List.range { start: At (number.width - 1), end: At (number.width + number.length) }

    List.any
        lines
        (\line ->
            List.any
                columns
                (\column ->
                    List.get (input |> Str.toUtf8) ((line * width + column) |> Num.toU64) |> isSymbol
                )
        )

expect
    got = isPartNumber
        { height: 5, length: 2, number: 58, width: 7 }
        exampleInput

    got == Bool.false

isSymbol = \elem ->
    when elem is
        Ok c ->
            Bool.not (isDigit c || c == '.' || c == '\n')

        _ -> Bool.false

isDigit = \c ->
    c >= '0' && c <= '9'

expect
    got = part2 exampleInput
    got == "467835"

part2 = \input ->
    input
    |> findNumbers
    |> List.map (\n -> gearPos n input)
    |> listKeepOk
    |> gearRatio
    |> Num.toStr

gearPos = \number, input ->
    width = (widthInput input) + 1 |> Num.toI32

    lines : List (Int Num.Signed32)
    lines = List.range { start: At (number.height - 1), end: At (number.height + 1) }
    columns : List (Int Num.Signed32)
    columns = List.range { start: At (number.width - 1), end: At (number.width + number.length) }

    List.walk
        lines
        []
        (\state, line ->
            List.walk
                columns
                []
                (\cstate, column ->
                    pos = (line * width + column) |> Num.toU64
                    if List.get (input |> Str.toUtf8) pos |> isGear then
                        List.append cstate pos
                    else
                        cstate
                )
            |> List.concat state
        )
    |> List.first
    |> Result.mapErr \_ -> NoGear
    |> Result.map \pos -> (pos, number.number)

isGear = \elem ->
    when elem is
        Ok c ->
            c == '*'

        _ -> Bool.false

listKeepOk : List (Result a err) -> List a
listKeepOk = \lst ->
    List.walk
        lst
        []
        (\state, r ->
            when r is
                Ok a -> List.append state a
                _ -> state
        )

gearRatio = \gears ->
    List.walkWithIndex
        gears
        []
        (\state, (pos, number), index ->
            mayOther = List.findFirst
                (List.dropFirst gears (index + 1))
                \(otherPos, _) -> pos == otherPos
            when mayOther is
                Ok (_, otherNumber) -> List.append state (number * otherNumber)
                _ -> state
        )
    |> List.sum
