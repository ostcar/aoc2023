app "day"
    packages {
        pf: "../platform/main.roc",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        "day20.input" as puzzleInput : Str,
        parser.String.{ parseStr, string, codeunit },
        parser.Core.{ Parser, oneOf, const, skip, map, chompWhile, sepBy, keep },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    broadcaster -> a, b, c
    %a -> b
    %b -> c
    %c -> inv
    &inv -> a
    """

exampleInput2 =
    """
    broadcaster -> a
    %a -> inv, con
    &inv -> b
    %b -> con
    &con -> output
    """

expect
    got = part1 exampleInput
    got == "32000000"

expect
    got = part1 exampleInput2
    got == "11687500"

part1 = \input ->
    input
    |> parseInput
    |> pushTheButtonMany 1_000 (0, 0)
    |> \(low, high) -> (low * high)
    |> Num.toStr

parseInput = \input ->
    when parseStr inputParser (Str.trim input) is
        Ok v -> v
        Err err ->
            dbg err

            crash "wrong"

inputParser =
    lineParser
    |> sepBy (codeunit '\n')
    |> map parseAgain

lineParser =
    const \(name, type) -> \nameList -> (name, type, nameList)
    |> keep
        (
            oneOf [
                string "broadcaster" |> map \_ -> ("broadcaster", Broadcaster),
                flipFlopParser,
                conjuctionParser,
            ]
        )
    |> skip (string " -> ")
    |> keep (nameParser |> sepBy (string ", "))

flipFlopParser =
    const \name -> (name, FlipFlop)
    |> skip (codeunit '%')
    |> keep (nameParser)

conjuctionParser =
    const \name -> (name, Conjunction)
    |> skip (codeunit '&')
    |> keep (nameParser)

nameParser =
    (chompWhile (\c -> c >= 'a' && c <= 'z') |> map (\l -> l |> Str.fromUtf8 |> unwrap))

Signal : [Low, High]
Name : Str
Module : (
    [
        ModuleBroadcaster,
        ModuleFlipFlop [On, Off],
        ModuleConjunction (Dict Name Signal),
    ],
    List Name,
)

parseAgain : List (Str, [Broadcaster, FlipFlop, Conjunction], List Str) -> Dict Name Module
parseAgain = \input ->
    input
    |> List.walk
        ([], Dict.empty {})
        \(modules, parents), (name, type, destinations) ->
            module =
                when type is
                    Broadcaster -> ModuleBroadcaster
                    FlipFlop -> ModuleFlipFlop Off
                    Conjunction -> ModuleConjunction (Dict.empty {})

            newParents =
                List.walk
                    destinations
                    parents
                    \state, dest ->
                        Dict.update
                            state
                            dest
                            (\old ->
                                when old is
                                    Missing -> Present (Dict.single name Low)
                                    Present dict -> Present (Dict.insert dict name Low)
                            )

            (
                List.append modules (name, (module, destinations)),
                newParents,
            )
    |> \(modules, parents) ->
        List.map modules \(name, (module, destinations)) ->
            newModule =
                when module is
                    ModuleConjunction _ -> ModuleConjunction (parents |> Dict.get name |> Result.withDefault (Dict.empty {}))
                    _ -> module
            (name, (newModule, destinations))
        |> Dict.fromList

pushTheButtonMany = \modules, count, (rl, rh) ->
    if count > 0 then
        (newModules, (orl, orh), _rxStuffForPart2) = pushTheButton modules ""
        pushTheButtonMany newModules (count - 1) (rl + orl, rh + orh)
    else
        (rl, rh)

pushTheButton : Dict Str Module, Str -> (Dict Str Module, (Nat, Nat), Bool)
pushTheButton = \modules, outputStr ->
    pushTheButtonHelper modules [("button", Low, "broadcaster")] (0, 0) Bool.false outputStr

pushTheButtonHelper : Dict Str Module, List (Str, Signal, Str), (Nat, Nat), Bool, Str -> (Dict Str Module, (Nat, Nat), Bool)
pushTheButtonHelper = \modules, buffer, result, rxReached, outputStr ->
    when buffer is
        [] -> (modules, result, rxReached)
        [(originName, signal, targetName), .. as rest] ->
            newResult = addToResult result signal
            isRXReachedWithLow =
                rxReached || (targetName == outputStr && signal == Low)

            when Dict.get modules targetName is
                Ok (module, destinations) ->
                    (newModules, newRest) =
                        when module is
                            ModuleBroadcaster ->
                                (
                                    modules,
                                    List.concat rest (List.map destinations \d -> (targetName, signal, d)),
                                )

                            ModuleFlipFlop onOff ->
                                when (signal, onOff) is
                                    (High, On) -> (modules, rest)
                                    (High, Off) -> (modules, rest)
                                    (Low, Off) ->
                                        (
                                            Dict.insert modules targetName (ModuleFlipFlop On, destinations),
                                            List.concat rest (List.map destinations \d -> (targetName, High, d)),
                                        )

                                    (Low, On) ->
                                        (
                                            Dict.insert modules targetName (ModuleFlipFlop Off, destinations),
                                            List.concat rest (List.map destinations \d -> (targetName, Low, d)),
                                        )

                            ModuleConjunction parents ->
                                newParents = Dict.insert parents originName signal
                                newSignal = if newParents |> Dict.values |> List.all (\v -> v == High) then Low else High
                                (
                                    Dict.insert modules targetName (ModuleConjunction newParents, destinations),
                                    List.concat rest (List.map destinations \d -> (targetName, newSignal, d)),
                                )

                    pushTheButtonHelper newModules newRest newResult isRXReachedWithLow outputStr

                Err KeyNotFound ->
                    pushTheButtonHelper modules rest newResult isRXReachedWithLow outputStr

addToResult = \(resultLow, resultHigh), signal ->
    when signal is
        Low -> (resultLow + 1, resultHigh)
        High -> (resultLow, resultHigh + 1)

part2 = \input ->
    input
    |> parseInput
    |> findRxGrandParents
    |> \(modules, grandparents) ->
        List.map grandparents \g -> pushTheButtonUntilOutputStr modules 0 g
    |> lcmList
    |> Num.toStr

findRxGrandParents = \modules ->
    parent =
        Dict.walkUntil
            modules
            (Err NotFound)
            \_, name, (_, destinations) ->
                if List.contains destinations "rx" then
                    Break (Ok name)
                else
                    Continue (Err NotFound)
        |> \r ->
            when r is
                Ok name -> name
                Err NotFound -> crash "no rx in input"

    grandparents =
        Dict.walk
            modules
            []
            \state, name, (_, destinations) ->
                if List.contains destinations parent then
                    List.append state name
                else
                    state
    (modules, grandparents)

gcd = \a, b ->
    if a == 0 then
        b
    else
        gcd (b % a) a

lcm = \a, b ->
    (a // (gcd a b)) * b

expect
    got = lcm 15 20
    got == 60

# Inspired by https://www.geeksforgeeks.org/lcm-of-given-array-elements/?ref=lbp
lcmList = \list ->
    when list is
        [] -> 0
        [a] -> a
        [a, b, .. as rest] ->
            List.walk rest (lcm a b) lcm

pushTheButtonUntilOutputStr = \modules, counter, outputStr ->
    # dbg counter

    (newModules, _stuffForPart1, rxReached) = pushTheButton modules outputStr
    if rxReached then
        counter + 1
    else
        pushTheButtonUntilOutputStr newModules (counter + 1) outputStr

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"
