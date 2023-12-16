app "day0"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day05.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4
    """

expect
    got = part1 exampleInput
    got == "35"

part1 = \input ->
    (seeds, groupList) = parseInput (Str.trim input)
    List.map seeds (\s -> applyGroups s groupList)
    |> List.walk Num.maxU64 (\state, n -> Num.min state n)
    |> Num.toStr

Map : { destination : U64, source : U64, length : U64 }
Group : List Map

parseInput : Str -> (List U64, List Group)
parseInput = \input ->
    inputGroups = Str.split input "\n\n"
    rawSeeds = List.first inputGroups |> mustSucceed "List.first inputGroups"
    rawGroups = List.dropFirst inputGroups 1

    seeds =
        rawSeeds
        |> Str.split ": "
        |> List.get 1
        |> mustSucceed "seeds = List.get 1"
        |> parseNumbers

    groups =
        rawGroups
        |> List.map \rawGroup ->
            rawGroup
            |> Str.split "\n"
            |> List.dropFirst 1
            |> List.map
                (\line ->
                    line
                    |> parseNumbers
                    |> parseMap
                )

    (seeds, groups)

parseNumbers : Str -> List U64
parseNumbers = \input ->
    input
    |> Str.split " "
    |> List.map \s ->
        when Str.toU64 s is
            Ok n -> n
            Err _ -> crash "invalid number '\(s)'"

parseMap : List U64 -> Map
parseMap = \input ->
    when input is
        [first, second, third] ->
            { destination: first, source: second, length: third }

        _ -> crash "invalid group input"

applyGroups : U64, List Group -> U64
applyGroups = \seed, groupList ->
    List.walk
        groupList
        seed
        (\state, group ->
            List.walkUntil
                group
                state
                (\state2, map ->
                    if state2 >= map.source && state2 <= map.source + map.length then
                        Break (applyMap state2 map)
                    else
                        Continue state2
                )
        )

applyMap : U64, Map -> U64
applyMap = \n, map ->
    map.destination + (n - map.source)

expect
    map = { destination: 50, source: 98, length: 2 }
    got = applyMap 98 map
    got == 50

expect
    map = { destination: 50, source: 98, length: 2 }
    got = applyMap 99 map
    got == 51

expect
    map = { destination: 52, source: 50, length: 48 }
    got = applyMap 53 map
    got == 55

part2 = \input ->
    (part1Seeds, groupList) = parseInput (Str.trim input)
    seedRanges = convertSeeds part1Seeds
    List.walk
        groupList
        seedRanges
        \seedState, group ->
            applyGroupOnRangeList seedState group
    |> minSeedFromRangeList
    |> Num.toStr

expect
    got = part2 exampleInput
    got == "46"

Range : { from : U64, rangeLength : U64 }

convertSeeds : List U64 -> List Range
convertSeeds = \list ->
    list
    |> List.chunksOf 2
    |> List.map \e -> {
        from: List.first e |> mustSucceed "has first",
        rangeLength: List.get e 1 |> mustSucceed "has second",
    }

applyGroupOnRangeList : List Range, Group -> List Range
applyGroupOnRangeList = \rangeList, group ->
    List.walk
        group
        (rangeList |> List.map \e -> NotApplyed e)
        \rangeState, map ->
            List.map
                rangeState
                \range ->
                    when range is
                        Applyed _ -> [range]
                        NotApplyed r ->
                            applyRangeOnMap r map
            |> listFlatten
    |> List.map \rangeWithTag ->
        when rangeWithTag is
            Applyed v -> v
            NotApplyed v -> v

applyRangeOnMap : Range, Map -> List [Applyed Range, NotApplyed Range]
applyRangeOnMap = \{ from, rangeLength }, map ->
    if from >= map.source && from + rangeLength <= map.source + map.length then
        [Applyed { from: applyMap from map, rangeLength: rangeLength }]
    else if from >= map.source && from <= map.source + map.length then
        newLength = map.source + map.length - from
        [
            Applyed { from: applyMap from map, rangeLength: newLength },
            NotApplyed { from: map.source + map.length + 1, rangeLength: rangeLength - newLength },
        ]
    else if from < map.source && from + rangeLength >= map.source && from + rangeLength <= map.source + map.length then
        newLength = map.source - from
        [
            NotApplyed { from: from, rangeLength: newLength },
            Applyed { from: applyMap map.source map, rangeLength: rangeLength - newLength },
        ]
    else if from < map.source && from + rangeLength >= map.source then
        newLength = map.source - from
        [
            NotApplyed { from: from, rangeLength: newLength },
            Applyed { from: applyMap map.source map, rangeLength: map.length },
            NotApplyed { from: map.source + map.length + 1, rangeLength: rangeLength - newLength - map.length },
        ]
    else if from + rangeLength < map.source || from > map.source + map.length then
        [
            NotApplyed { from: from, rangeLength: rangeLength },
        ]
    else
        crash "not implemented"

minSeedFromRangeList : List Range -> U64
minSeedFromRangeList = \rangeList ->
    rangeList
    |> List.map .from
    |> List.min
    |> mustSucceed "not empty"

mustSucceed = \r, str ->
    when r is
        Ok v -> v
        _ -> crash "unreachable at \(str)"

listFlatten : List (List a) -> List a
listFlatten = \lst ->
    List.walk
        lst
        []
        \state, elem ->
            List.concat state elem
