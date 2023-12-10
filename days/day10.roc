app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports ["day10.input" as puzzleInput : Str]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    ..F7.
    .FJ|.
    SJ.L7
    |F--J
    LJ...
    """

part1 = \input ->
    mace = parseInput input
    start = List.findFirstIndex mace.data (\c -> c == 'S') |> unwrap
    startDirection = findStartDirection mace start

    loopLength = walkLoop mace start startDirection 0
    Num.toStr (loopLength // 2)

expect
    got = part1 exampleInput
    got == "8"

Mace : { data : List U8, width : Nat }

parseInput = \input ->
    bytes = Str.toUtf8 input
    {
        data: List.dropIf bytes (\c -> c == '\n'),
        width: List.findFirstIndex bytes (\c -> c == '\n') |> unwrap,
    }

# findStartDirection returns a direction, where you can go from start.
#
# Since start has two direction and only one direction has to be returned, this
# functions only returns Top, Right or Bottom. There is no need to return Left.
findStartDirection = \mace, pos ->
    if List.contains ['7', '|', 'F'] (getPosition mace (nextPosition mace pos Top)) then
        Top
    else if List.contains ['7', '-', 'J'] (getPosition mace (nextPosition mace pos Right)) then
        Right
    else
        Bottom

# nextPosition returns the next position in one direction.
#
# This is not safe for border positions. For example `nextPosition mace 0 Left`
# returns -1. Use nextPositionSafe to handle this case.
nextPosition = \{ width }, pos, direction ->
    when direction is
        Top -> pos - width
        Bottom -> pos + width
        Left -> pos - 1
        Right -> pos + 1

# nextPositionSafe is like nextPosition but returns a Result that fails, if the
# new position if outside the mace.
nextPositionSafe = \{ data, width }, pos, direction ->
    when direction is
        Top ->
            if pos < width then
                Err OutOfBound
            else
                Ok (pos - width)

        Bottom ->
            if pos >= (List.len data) - width then
                Err OutOfBound
            else
                Ok (pos + width)

        Left ->
            if pos % width == 0 then
                Err OutOfBound
            else
                Ok (pos - 1)

        Right ->
            if pos % width == width - 1 then
                Err OutOfBound
            else
                Ok (pos + 1)

expect
    mace = parseInput
        """
        AB
        CD
        """
    got = nextPositionSafe mace 2 Bottom
    got == Err OutOfBound

expect
    mace = parseInput
        """
        AB
        CD
        """
    got = nextPositionSafe mace 2 Left
    got == Err OutOfBound

expect
    mace = parseInput
        """
        AB
        CD
        """
    got = nextPositionSafe mace 1 Right
    got == Err OutOfBound

getPosition = \mace, pos ->
    List.get mace.data pos |> unwrap

oppositeDirection = \direction ->
    when direction is
        Top -> Bottom
        Bottom -> Top
        Left -> Right
        Right -> Left

# walkLoop walks the path though the mace and returns its length.
walkLoop = \mace, pos, direction, steps ->
    newPos = nextPosition mace pos direction
    pipe = getPosition mace newPos

    if pipe == 'S' then
        steps + 1
    else
        nextDirection = followPipe pipe (oppositeDirection direction)
        walkLoop mace newPos nextDirection (steps + 1)

# followPipe returns an direction
#
# Each pipe has two directions, this function is given the direction, where you
# come from and returns the other direction of the pipe.
#
# e. G.
# * `followPipi 'L' Top` returns `Right`.
# * `followPipi 'L' Right` returns `Top`.
followPipe = \pipe, comeFrom ->
    directions = pipeDirections pipe
    if directions.0 == comeFrom then
        directions.1
    else
        directions.0

pipeDirections = \pipe ->
    when pipe is
        '|' -> (Top, Bottom)
        '-' -> (Left, Right)
        'L' -> (Top, Right)
        'J' -> (Top, Left)
        '7' -> (Bottom, Left)
        'F' -> (Bottom, Right)
        _ ->
            v = [pipe] |> Str.fromUtf8 |> unwrap
            crash "pipe without direction \(v)"

expect
    input =
        """
        ...........
        .S-------7.
        .|F-----7|.
        .||.....||.
        .||.....||.
        .|L-7.F-J|.
        .|..|.|..|.
        .L--J.L--J.
        ...........
        """
    got = part2 input

    got == "4"

expect
    input =
        """
        .F----7F7F7F7F-7....
        .|F--7||||||||FJ....
        .||.FJ||||||||L7....
        FJL7L7LJLJ||LJ.L-7..
        L--J.L7...LJS7F-7L7.
        ....F-J..F7FJ|L7L7L7
        ....L7.F7||L7|.L7L7|
        .....|FJLJ|FJ|F7|.LJ
        ....FJL-7.||.||||...
        ....L---J.LJ.LJLJ...
        """
    got = part2 input

    got == "8"

expect
    input =
        """
        FF7FSF7F7F7F7F7F---7
        L|LJ||||||||||||F--J
        FL-7LJLJ||||||LJL-77
        F--JF--7||LJLJ7F7FJ-
        L---JF-JLJ.||-FJLJJ7
        |F|F-JF---7F7-L7L|7|
        |FFJF7L7F-JF7|JL---7
        7-L-JL7||F7|L7F-7F7|
        L.L7LFJ|||||FJL7||LJ
        L7JLJL-JLJLJL--JLJ.L
        """
    got = part2 input

    got == "10"

# part2 is the solution for part2.
#
# The idea is, to replace the loop with the sign '#'. Then start outside, go
# from one place to its neighbors. If they are not '#', then also replace them
# with '#'. It like flood the map with water that is only stopped by '#'. The
# solution are the elements, that reached by the water and therefore are
# unchanged.
#
# For this to work, all border fields have to be empty spaces, or they would
# block the process. To solve this, the input gets a separat ring of empty
# places.
#
#            ......
# .###       ..###.
# ##.#  ->   .##.#.
# #..#       .#..#.
# ####       .####.
#            ......
#
# But there could be peaces, that can not be reached. The following drawing
# shows a mace, where there are two places at the top, are outside. But there is
# no free passage for the "water".
#
# ┌────┐
# │┌──┐│
# │|..|│
# │└┐┌┘│
# │.││.│
# └─┘└─┘
#
# To solve this, the map has to be scaled up.
#
# ┌───────┐
# │.......│
# │.┌───┐.│
# │.│...│.│
# │.|...|.│
# │.└┐.┌┘.│
# │..│.│..│
# │..│.│..│
# └──┘.└──┘
#
# Its the same mace, but this time, the method works.
#
# At the end, it has to be scaled down again.
part2 = \strInput ->
    mace =
        strInput
        |> Str.trim
        |> addEmptySpaceAroundInput
        |> parseInput

    start = List.findFirstIndex mace.data (\c -> c == 'S') |> unwrap
    startDirection = findStartDirection mace start

    directions =
        findPath mace start startDirection []
        |> upscaleDirections

    # Create a new (empty) mace that is twice as big as the original in both
    # dimensions.
    scaledMace = { data: List.repeat '.' (List.len mace.data * 4), width: mace.width * 2 }

    # Find the new start position on the bigger mace.
    startX = (start % mace.width) * 2
    startY = (start // mace.width) * 2
    scaledStart = startX + startY * scaledMace.width

    # Walk the loop and draw '#'
    List.walk
        directions
        (scaledStart, scaledMace)
        (\(pos, state), direction ->
            (
                nextPosition state pos direction,
                buildWall state pos,
            )

        )
    |> .1
    |> fillOutsideWithWall [0]
    |> downscaleMace
    |> .data
    |> List.countIf (\c -> c != wall)
    |> Num.toStr

wall = '#'

addEmptySpaceAroundInput = \input ->
    input
    |> Str.split "\n"
    |> List.map \line -> ".\(line)."
    |> (\lines ->
        length =
            List.first lines
            |> unwrap
            |> Str.countUtf8Bytes

        lines
        |> List.prepend (Str.repeat "." length)
        |> List.append (Str.repeat "." length)
    )
    |> Str.joinWith "\n"

expect
    input =
        """
        AB
        CD
        """
    got = addEmptySpaceAroundInput input
    shouldBe =
        """
        ....
        .AB.
        .CD.
        ....
        """
    got == shouldBe

# findPath returns all directions to go the loop from start to start.
findPath = \mace, pos, direction, result ->
    nextPos = nextPosition mace pos direction
    pipe = getPosition mace nextPos
    res = List.append result direction

    if pipe == 'S' then
        res
    else
        nextDirection = followPipe pipe (oppositeDirection direction)
        findPath mace nextPos nextDirection res

# upscaleDirections doubles all directions.
#
# `[Bottom,Right,Top,Left]` -> `[Bottom,Bottom,Right,Right,Top,Top,Left,Left]`
#
# This returns a loop with the same shape but twice as big in both dimensions.
upscaleDirections = \directions ->
    List.walk directions [] (\state, elem -> List.concat state [elem, elem])

expect
    mace =
        """
        S7
        LJ
        """
        |> addEmptySpaceAroundInput
        |> parseInput

    start = List.findFirstIndex mace.data (\c -> c == 'S') |> unwrap
    startDirection = findStartDirection mace start

    directions =
        (findPath mace start startDirection [] |> upscaleDirections)

    directions == [Right, Right, Bottom, Bottom, Left, Left, Top, Top]

downscaleMace : Mace -> Mace
downscaleMace = \{ data, width } ->
    newData =
        List.chunksOf data width
        |> List.walkWithIndex
            []
            (\state, elem, idx ->
                if idx % 2 != 0 then
                    state
                else
                    List.walkWithIndex
                        elem
                        state
                        (\state2, elem2, idx2 ->
                            if idx2 % 2 != 0 then
                                state2
                            else
                                List.append state2 elem2
                        )
            )
    { data: newData, width: width // 4 }

buildWall = \mace, pos ->
    { mace & data: List.set mace.data pos wall }

# fillOutsideWithWall replaces all neighbors of an element with '#' but stops,
# on neighbors, that are already '#'.
#
# It is implement as a breath first search to be tail recursive.
fillOutsideWithWall = \mace, nextPositions ->
    when nextPositions is
        [] -> mace
        [pos, .. as next] ->
            updatedMace = buildWall mace pos
            newNextPositions =
                [Top, Bottom, Left, Right]
                |> List.walk
                    next
                    (\state, direction ->
                        when nextPositionSafe updatedMace pos direction is
                            Ok nextPos ->
                                if getPosition updatedMace nextPos == wall then
                                    state
                                else
                                    listAddUnique state nextPos

                            Err OutOfBound ->
                                state

                    )

            fillOutsideWithWall updatedMace newNextPositions

listAddUnique = \list, e ->
    if List.contains list e then
        list
    else
        List.append list e

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "unreachable"
