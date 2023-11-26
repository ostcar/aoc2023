platform "aoc"
    requires {} { solution : (Str, Str) }
    exposes []
    packages {}
    imports []
    provides [solutionForHost]

solutionForHost : (Str, Str)
solutionForHost = solution
