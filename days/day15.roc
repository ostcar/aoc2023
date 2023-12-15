app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day15.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

expect
    got = part1 exampleInput
    got == "1320"

part1 = \input ->
    input
    |> Str.trim
    |> Str.split ","
    |> List.map (\line -> line |> theHASHalgorithm)
    |> List.sum
    |> Num.toStr

theHASHalgorithm = \input ->
    input
    |> Str.toUtf8
    |> List.walk
        0
        \state, elem ->
            state
            |> Num.add (Num.toNat elem)
            |> Num.mul 17
            |> Num.rem 256

expect
    got = "HASH" |> theHASHalgorithm
    got == 52

expect
    got = part2 exampleInput
    got == "145"

part2 = \input ->
    input
    |> parseInput
    |> doHashmap
    |> focusingPower
    |> Num.toStr

Operation : (Str, [Add U8, Del])

KeyValue : (Str, U8)

Hashmap : List (List KeyValue)

parseInput = \input ->
    input
    |> Str.trim
    |> Str.split ","
    |> List.map \elem ->
        byEq =
            elem
            |> Str.toUtf8
            |> List.splitFirst '='
        when byEq is
            Ok { before, after } ->
                ((before |> Str.fromUtf8 |> unwrap), Add (after |> toVal))

            Err NotFound ->
                elem
                |> Str.toUtf8
                |> List.splitFirst '-'
                |> unwrap
                |> \{ before } ->
                    ((before |> Str.fromUtf8 |> unwrap), Del)

toVal = \list ->
    List.first list
    |> unwrap
    |> \v -> v - '0'

doHashmap : List Operation -> Hashmap
doHashmap = \operations ->
    List.walk
        operations
        initHashmap
        \hashmap, (key, op) ->
            boxID = theHASHalgorithm key
            box = List.get hashmap boxID |> unwrap
            newBox =
                when op is
                    Add n ->
                        when List.findFirstIndex box (\(k, _) -> k == key) is
                            Ok index ->
                                List.set box index (key, n)

                            Err NotFound ->
                                List.append box (key, n)

                    Del ->
                        List.dropIf box \(k, _) -> k == key

            List.set hashmap boxID newBox

initHashmap = List.repeat [] 256

focusingPower = \hashmap ->
    List.walkWithIndex
        hashmap
        0
        \state, box, idx ->
            List.walkWithIndex
                box
                state
                \state2, (_, v), idx2 ->
                    state2 + (idx + 1) * (idx2 + 1) * (v |> Num.toNat)

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"
