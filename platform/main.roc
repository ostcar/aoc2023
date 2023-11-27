platform "aoc"
    requires {} { solution : { part1 : {} -> Str, part2 : {} -> Str } }
    exposes []
    packages {}
    imports []
    provides [solutionForHost]

solutionForHost : [Part1, Part2] -> Str
solutionForHost = \part ->
    when part is
        Part1 -> solution.part1 {}
        Part2 -> solution.part2 {}
