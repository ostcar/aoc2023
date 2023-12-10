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

expect
    got = part1 exampleInput
    got == "8"

part1 = \strInput ->
    mace = parseMace strInput
    start = List.findFirstIndex mace.data (\c -> c == 'S') |> unwrap "findStart"
    startDirection = findStartDirections mace start
    loopLength = walkLoop mace start startDirection 0
    Num.toStr (loopLength // 2)

parseMace = \strInput ->
    input = Str.toUtf8 strInput
    width = getWidth input
    { data: List.dropIf input (\c -> c == '\n'), width: width }

getWidth = \input ->
    List.findFirstIndex input (\c -> c == '\n') |> unwrap "getWidth"

findStartDirections = \mace, pos ->
    if inThree (walkDirection mace pos Top) '7' '|' 'F' then
        Top
    else if inThree (walkDirection mace pos Right) '7' '-' 'J' then
        Right
    else
        Bottom

walkDirection = \mace, pos, direction ->
    getPos mace (nextPos mace pos direction)

nextPos = \{ width }, pos, direction ->
    when direction is
        Top -> pos - width
        Bottom -> pos + width
        Left -> pos - 1
        Right -> pos + 1

getPos = \mace, pos ->
    List.get mace.data pos |> unwrap "getPos"

inThree = \elem, one, two, three ->
    elem == one || elem == two || elem == three

oppositeDirection = \direction ->
    when direction is
        Top -> Bottom
        Bottom -> Top
        Left -> Right
        Right -> Left

walkLoop = \mace, pos, direction, steps ->
    nextP = nextPos mace pos direction
    nextE = getPos mace nextP

    if nextE == 'S' then
        steps + 1
    else
        nextDirection = followPipe nextE (oppositeDirection direction)
        walkLoop mace nextP nextDirection (steps + 1)

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
            v = [pipe] |> Str.fromUtf8 |> unwrap "crash pipeDirection"
            crash "pipe without direction \(v)"

expect
    got = part2
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
    got == "4"

expect
    got = part2
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
    got == "8"

expect
    got = part2
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
    got == "10"

part2 = \strInput ->
    # Make a mace that is twice as big by going each way twice.
    # Then change the loops to w 'wall'.
    # Then fill all elements starting from the outer ring also to walls until every
    # neighbor is also a wall.
    # Then let the mace shrink again by remove each second line and column
    # The result are all non-wall elements.
    mace =
        strInput
        |> Str.trim
        |> addRingAroundInput "."
        |> parseMace

    start = List.findFirstIndex mace.data (\c -> c == 'S') |> unwrap "findstart part2"
    startDirection = findStartDirections mace start

    path =
        findPath mace start startDirection []
        |> upscaleDirections

    scalledMace = { data: List.repeat '.' (List.len mace.data * 4), width: mace.width * 2 }

    startX = (start % mace.width) * 2
    startY = (start // mace.width) * 2
    scalledStart = startX + startY * scalledMace.width

    List.walk
        path
        (scalledStart, scalledMace)
        (\(pos, state), direction ->
            (
                nextPos state pos direction,
                buildWall state pos,
            )

        )
    |> .1
    |> addWallOutside [0]
    |> downscaleMace
    |> .data
    |> List.countIf (\c -> c != wall)
    |> Num.toStr

addRingAroundInput = \input, c ->
    input
    |> Str.split "\n"
    |> List.map \line -> "\(c)\(line)\(c)"
    |> (\lines ->
        length =
            List.first lines
            |> unwrap "addRingAroundInput"
            |> Str.countUtf8Bytes

        lines
        |> List.prepend (Str.repeat c length)
        |> List.append (Str.repeat c length)
    )
    |> Str.joinWith "\n"

expect
    input =
        """
        AB
        CD
        """
    got = addRingAroundInput input "."
    shouldBe =
        """
        ....
        .AB.
        .CD.
        ....
        """
    got == shouldBe

findPath = \mace, pos, direction, result ->
    nextP = nextPos mace pos direction
    nextE = getPos mace nextP
    res = List.append result direction

    if nextE == 'S' then
        res
    else
        nextDirection = followPipe nextE (oppositeDirection direction)
        findPath mace nextP nextDirection res

upscaleDirections = \directions ->
    List.walk directions [] (\state, elem -> List.concat state [elem, elem])

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

expect
    mace =
        """
        S7
        LJ
        """
        |> addRingAroundInput "."
        |> parseMace

    start = List.findFirstIndex mace.data (\c -> c == 'S') |> unwrap "findstart2"
    startDirection = findStartDirections mace start

    directions =
        (findPath mace start startDirection [] |> upscaleDirections)

    directions == [Right, Right, Bottom, Bottom, Left, Left, Top, Top]

wall = '#'

Mace : { data : List U8, width : Nat }

buildWall : Mace, Nat -> Mace
buildWall = \mace, pos ->
    { mace & data: List.set mace.data pos wall }

addWallOutside = \mace, nextElements ->
    when nextElements is
        [] -> mace
        [pos, .. as next] ->
            maceWithWall = buildWall mace pos
            newNext =
                [Top, Bottom, Left, Right]
                |> List.walk
                    next
                    (\state, direction ->
                        when nextPosSafe maceWithWall pos direction is
                            Ok nextP ->
                                if getPos maceWithWall nextP == wall then
                                    state
                                else
                                    listAddUnique state nextP

                            Err OutOfBound ->
                                state

                    )
            addWallOutside maceWithWall newNext

listAddUnique = \list, e ->
    if List.contains list e then
        list
    else
        List.append list e

nextPosSafe = \{ data, width }, pos, direction ->
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
    mace = parseMace
        """
        AB
        CD
        """
    got = nextPosSafe mace 2 Bottom
    got == Err OutOfBound

expect
    mace = parseMace
        """
        AB
        CD
        """
    got = nextPosSafe mace 2 Left
    got == Err OutOfBound

expect
    mace = parseMace
        """
        AB
        CD
        """
    got = nextPosSafe mace 1 Right
    got == Err OutOfBound

unwrap = \r, msg ->
    when r is
        Ok v -> v
        _ -> crash "unreachable \(msg)"
