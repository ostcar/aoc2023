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
    |> Str.trim
    |> parseInput
    |> \(workflows, parts) -> List.keepIf parts \part -> runPart workflows part
    |> sumParts
    |> Num.toStr

Part : { x : U64, m : U64, a : U64, s : U64 }
RuleResult : [Next Str, Accept, Reject]
Rule : [Check (Part -> [Found RuleResult, NotFound]), Done RuleResult]
Workflows : Dict Str (List Rule)

parseInput : Str -> (Workflows, List Part)
parseInput = \input ->
    when Str.split input "\n\n" is
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
    const
        (\attr -> \sign -> \number -> \next ->
                        Check
                            (\part ->
                                if sign (attr part) number then
                                    Found next
                                else
                                    NotFound)

        )
    |> keep
        (
            oneOf [
                codeunit 'x' |> map \_ -> .x,
                codeunit 'm' |> map \_ -> .m,
                codeunit 'a' |> map \_ -> .a,
                codeunit 's' |> map \_ -> .s,
            ]
        )
    |> keep
        (
            oneOf [
                codeunit '>' |> map \_ -> Num.isGt,
                codeunit '<' |> map \_ -> Num.isLt,
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
                Check fn ->
                    when fn part is
                        Found r -> r
                        NotFound -> applyWorkflow rest part

                Done r -> r

sumParts : List Part -> U64
sumParts = \parts ->
    parts
    |> List.map \p -> p.x + p.m + p.a + p.s
    |> List.sum

expect
    got = part2 exampleInput
    got == "167409079868000"

part2 = \_ ->
    "Not implemented yet"

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"
