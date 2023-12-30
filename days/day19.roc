app "day"
    packages {
        pf: "../platform/main.roc",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        "day19.input" as puzzleInput : Str,
        parser.String.{ digits, parseStr, string, codeunit },
        parser.Core.{ Parser, oneOf, const, skip, map, chompUntil, chompWhile, sepBy, keep },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    px{a<2006:qkq,m>2090:A,rfg}
    pv{a>1716:R,A}
    lnx{m>1548:A,A}
    rfg{s<537:gd,x>2440:R,A}
    qs{s>3448:A,lnx}
    qkq{x<1416:A,crn}
    crn{x>2662:A,R}
    in{s<1351:px,qqz}
    qqz{s>2770:qs,m<1801:hdj,R}
    gd{a>3333:R,R}
    hdj{m>838:A,pv}

    {x=787,m=2655,a=1222,s=2876}
    {x=1679,m=44,a=2067,s=496}
    {x=2036,m=264,a=79,s=2244}
    {x=2461,m=1339,a=466,s=291}
    {x=2127,m=1623,a=2188,s=1013}
    """

expect
    got = part1 exampleInput
    got == "19114"

part1 = \input ->
    input
    |> parseInput
    |> \(workflows, parts) -> List.keepIf parts \part -> runPart workflows part
    |> sumParts
    |> Num.toStr

Part : { x : U64, m : U64, a : U64, s : U64 }
RuleResult : [Next Str, Accept, Reject]
Rule : [Check [X, M, A, S] [Gt, Lt] U64 RuleResult, Done RuleResult]
Workflows : Dict Str (List Rule)

parseInput : Str -> (Workflows, List Part)
parseInput = \input ->
    when input |> Str.trim |> Str.split "\n\n" is
        [workflows, parts] ->
            (
                parseWorkflows workflows,
                parseParts parts,
            )

        _ -> crash "invalid input"

parseWorkflows : Str -> Workflows
parseWorkflows = \input ->
    when parseStr (sepBy workflowParser (codeunit '\n')) input is
        Ok parsed ->
            Dict.fromList parsed

        Err err ->
            dbg err

            crash "can not parse workflows"

workflowParser : Parser (List U8) (Str, List Rule)
workflowParser =
    const (\name -> \rules -> (name |> Str.fromUtf8 |> unwrap, rules))
    |> keep (chompUntil '{')
    |> skip (codeunit '{')
    |> keep (sepBy ruleParser (codeunit ','))
    |> skip (codeunit '}')

expect
    got = parseStr workflowParser "px{a<2006:qkq,m>2090:A,rfg}"
    when got is
        Ok _ -> Bool.true
        Err _ -> Bool.false

ruleParser : Parser (List U8) Rule
ruleParser =
    oneOf [
        conditionParser,
        lastRuleParser |> map \e -> Done e,
    ]

conditionParser : Parser (List U8) Rule
conditionParser =
    const (\attr -> \sign -> \number -> \next -> Check attr sign number next)
    |> keep
        (
            oneOf [
                codeunit 'x' |> map \_ -> X,
                codeunit 'm' |> map \_ -> M,
                codeunit 'a' |> map \_ -> A,
                codeunit 's' |> map \_ -> S,
            ]
        )
    |> keep
        (
            oneOf [
                codeunit '>' |> map \_ -> Gt,
                codeunit '<' |> map \_ -> Lt,
            ]
        )
    |> keep (digits |> map Num.toU64)
    |> skip (codeunit ':')
    |> keep (lastRuleParser)

lastRuleParser =
    oneOf [
        codeunit 'A' |> map \_ -> Accept,
        codeunit 'R' |> map \_ -> Reject,
        nameParser |> map \s -> Next s,
    ]

nameParser =
    (chompWhile (\c -> c >= 'a' && c <= 'z') |> map (\l -> l |> Str.fromUtf8 |> unwrap))

parseParts : Str -> List Part
parseParts = \input ->
    when parseStr (sepBy partParser (codeunit '\n')) input is
        Ok parsed ->
            parsed

        Err _ ->
            crash "can not parse workflows"

partParser : Parser (List U8) Part
partParser =
    const (\x -> \m -> \a -> \s -> { x: x, m: m, a: a, s: s })
    |> skip (string "{x=")
    |> keep (digits |> map (\n -> Num.toU64 n))
    |> skip (string ",m=")
    |> keep (digits |> map (\n -> Num.toU64 n))
    |> skip (string ",a=")
    |> keep (digits |> map (\n -> Num.toU64 n))
    |> skip (string ",s=")
    |> keep (digits |> map (\n -> Num.toU64 n))
    |> skip (codeunit '}')

expect
    got = parseStr partParser "{x=787,m=2655,a=1222,s=2876}"
    when got is
        Ok _ -> Bool.true
        Err _ -> Bool.false

runPart : Workflows, Part -> Bool
runPart = \workflows, part ->
    applyWorkflows workflows part (Next "in")

applyWorkflows : Workflows, Part, RuleResult -> Bool
applyWorkflows = \workflows, part, cur ->
    when cur is
        Accept -> Bool.true
        Reject -> Bool.false
        Next key ->
            ruleSet = Dict.get workflows key |> unwrap
            next = applyWorkflow ruleSet part

            applyWorkflows workflows part next

applyWorkflow = \workflow, part ->
    when workflow is
        [] -> crash "no rule left"
        [rule, .. as rest] ->
            when rule is
                Check attr sign number next ->
                    if partAttr part attr |> compareBySign sign number then
                        next
                    else
                        applyWorkflow rest part

                Done r -> r

partAttr = \part, attr ->
    fn =
        when attr is
            X -> .x
            M -> .m
            A -> .a
            S -> .s
    fn part

compareBySign = \a, sign, b ->
    when sign is
        Gt -> a > b
        Lt -> a < b

sumParts : List Part -> U64
sumParts = \parts ->
    parts
    |> List.map \p -> p.x + p.m + p.a + p.s
    |> List.sum

expect
    got = part2 exampleInput
    got == "167409079868000"

part2 = \input ->
    input
    |> parseInput
    |> .0
    |> \workflows -> runMultiPart workflows
    |> countMultipart
    |> Num.toStr

Range : { fromN : U64, toN : U64 }
MultiPart : { x : Range, m : Range, a : Range, s : Range }

runMultiPart : Workflows -> List MultiPart
runMultiPart = \workflows ->
    part = {
        x: { fromN: 1, toN: 4000 },
        m: { fromN: 1, toN: 4000 },
        a: { fromN: 1, toN: 4000 },
        s: { fromN: 1, toN: 4000 },
    }
    applyMultiWorkflows workflows [(part, Next "in")] []

applyMultiWorkflows : Workflows, List (MultiPart, RuleResult), List MultiPart -> List MultiPart
applyMultiWorkflows = \workflows, query, result ->
    when query is
        [(part, ruleResult), .. as rest] ->
            (newQuery, newResult) =
                when ruleResult is
                    Accept -> (rest, List.append result part)
                    Reject -> (rest, result)
                    Next key ->
                        ruleSet = Dict.get workflows key |> unwrap
                        next = applyMultiWorkflow ruleSet part []
                        (List.concat rest next, result)

            applyMultiWorkflows workflows newQuery newResult

        [] -> result

applyMultiWorkflow : List Rule, MultiPart, List (MultiPart, RuleResult) -> List (MultiPart, RuleResult)
applyMultiWorkflow = \ruleList, part, result ->
    when ruleList is
        [] -> crash "no rule left"
        [rule, .. as rest] ->
            when rule is
                Check attr sign number next ->
                    (r1, r2) = splitRange part attr sign number
                    newResult = List.append result (r1, next)
                    applyMultiWorkflow rest r2 newResult

                Done next ->
                    List.append result (part, next)

splitRange = \range, attr, sign, number ->
    when (attr, sign) is
        (X, Lt) ->
            (
                { range & x: { fromN: range.x.fromN, toN: number - 1 } },
                { range & x: { toN: range.x.toN, fromN: number } },
            )

        (X, Gt) ->
            (
                { range & x: { toN: range.x.toN, fromN: number + 1 } },
                { range & x: { fromN: range.x.fromN, toN: number } },
            )

        (M, Lt) ->
            (
                { range & m: { fromN: range.m.fromN, toN: number - 1 } },
                { range & m: { toN: range.m.toN, fromN: number } },
            )

        (M, Gt) ->
            (
                { range & m: { toN: range.m.toN, fromN: number + 1 } },
                { range & m: { fromN: range.m.fromN, toN: number } },
            )

        (A, Lt) ->
            (
                { range & a: { fromN: range.a.fromN, toN: number - 1 } },
                { range & a: { toN: range.a.toN, fromN: number } },
            )

        (A, Gt) ->
            (
                { range & a: { toN: range.a.toN, fromN: number + 1 } },
                { range & a: { fromN: range.a.fromN, toN: number } },
            )

        (S, Lt) ->
            (
                { range & s: { fromN: range.s.fromN, toN: number - 1 } },
                { range & s: { toN: range.s.toN, fromN: number } },
            )

        (S, Gt) ->
            (
                { range & s: { toN: range.s.toN, fromN: number + 1 } },
                { range & s: { fromN: range.s.fromN, toN: number } },
            )

countMultipart = \list ->
    List.map list \{ x, m, a, s } ->
        countRange x * countRange m * countRange a * countRange s
    |> List.sum

countRange = \{ fromN, toN } ->
    toN - fromN + 1

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"
