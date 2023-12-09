platform "aoc"
    requires {} { solution : Part -> Str }
    exposes []
    packages {}
    imports []
    provides [solutionForHost]

Part : [Part1, Part2]

solutionForHost : Part -> Str
solutionForHost = \part ->
    when part is
        Part1 -> solution Part1
        Part2 -> solution Part2
