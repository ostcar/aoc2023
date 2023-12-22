app "day"
    packages {
        pf: "../platform/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        "day21.input" as puzzleInput : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput 64
        Part2 -> part2 puzzleInput 26501365

exampleInput =
    """
    ...........
    .....###.#.
    .###.##..#.
    ..#.#...#..
    ....#.#....
    .##..S####.
    .##..#...#.
    .......##..
    .##.#.####.
    .##..##.##.
    ...........
    """

expect
    got = part1 exampleInput 6
    got == "16"

part1 = \input, steps ->
    input
    |> parseInput
    |> walk nextIndex steps
    |> Num.toStr

Map : Array2D [Empty, Wall]
Index : { x : I64, y : I64 }

parseInput : Str -> (Map, Index)
parseInput = \input ->
    lines = Str.split input "\n"
    dimY = List.first lines |> unwrap |> Str.countUtf8Bytes
    dimX = List.len lines

    initArray = Array2D.init { dimY, dimX } \_ -> Empty
    List.walkWithIndex
        lines
        (initArray, { x: 0, y: 0 })
        \state, line, x ->
            List.walkWithIndex
                (Str.toUtf8 line)
                state
                \(array, index), elem, y ->
                    when elem is
                        'S' ->
                            (
                                array,
                                {
                                    x: x |> Num.toI64,
                                    y: y |> Num.toI64,
                                },
                            )

                        '#' ->
                            (
                                Array2D.set array { x, y } Wall,
                                index,
                            )

                        _ ->
                            (array, index)

walk : (Map, Index), (Map, { dimX : I64, dimY : I64 }, Index -> Set Index), U64 -> Nat
walk = \(array, start), nextFn, steps ->
    shape =
        { dimX, dimY } = Array2D.shape array
        { dimX: dimX |> Num.toI64, dimY: dimY |> Num.toI64 }
    walkHelper array shape nextFn (Set.single start) steps

walkHelper : Map, { dimX : I64, dimY : I64 }, (Map, { dimX : I64, dimY : I64 }, Index -> Set Index), Set Index, U64 -> Nat
walkHelper = \array, shape, nextFn, cur, steps ->
    if steps == 0 then
        Set.len cur
    else
        next = Set.walk
            cur
            (Set.empty {})
            \result, index ->
                Set.union result (nextFn array shape index)

        walkHelper
            array
            shape
            nextFn
            next
            (steps - 1)

nextIndex = \array, shape, index ->
    list = List.withCapacity 4

    # Up
    list1 =
        when toIndexPart1 shape { index & x: index.x - 1 } is
            Ok idx ->
                if Array2D.get array idx |> unwrap == Empty then
                    List.append list { index & x: index.x - 1 }
                else
                    list

            Err OutOfShape ->
                list

    # Down
    list2 =
        when toIndexPart1 shape { index & x: index.x + 1 } is
            Ok idx ->
                if Array2D.get array idx |> unwrap == Empty then
                    List.append list1 { index & x: index.x + 1 }
                else
                    list1

            Err OutOfShape ->
                list1

    # Left
    list3 =
        when toIndexPart1 shape { index & y: index.y - 1 } is
            Ok idx ->
                if Array2D.get array idx |> unwrap == Empty then
                    List.append list2 { index & y: index.y - 1 }
                else
                    list2

            Err OutOfShape ->
                list2

    # Right
    list4 =
        when toIndexPart1 shape { index & y: index.y + 1 } is
            Ok idx ->
                if Array2D.get array idx |> unwrap == Empty then
                    List.append list3 { index & y: index.y + 1 }
                else
                    list3

            Err OutOfShape ->
                list3

    Set.fromList list4

toIndexPart1 : { dimX : I64, dimY : I64 }, { x : I64, y : I64 } -> Result { x : Nat, y : Nat } [OutOfShape]
toIndexPart1 = \{ dimX, dimY }, { x, y } ->
    if x >= 0 && x < dimX && y >= 0 && y < dimY then
        Ok {
            x: x % dimX |> Num.toNat,
            y: y % dimY |> Num.toNat,
        }
    else
        Err OutOfShape

expect
    got = part2 exampleInput 6
    got == "16"

expect
    got = part2 exampleInput 10
    got == "50"

expect
    got = part2 exampleInput 5000
    got == "16733044"

part2 = \input, steps ->
    input
    |> parseInput
    |> walk nextIndexPart2 steps
    |> Num.toStr

nextIndexPart2 = \array, shape, index ->
    list = List.withCapacity 4

    # Up
    list1 =
        idx = toIndex shape { index & x: index.x - 1 }
        if Array2D.get array idx |> unwrap == Empty then
            List.append list { index & x: index.x - 1 }
        else
            list

    # Down
    list2 =
        idx = toIndex shape { index & x: index.x + 1 }
        if Array2D.get array idx |> unwrap == Empty then
            List.append list1 { index & x: index.x + 1 }
        else
            list1

    # Left
    list3 =
        idx = toIndex shape { index & y: index.y - 1 }
        if Array2D.get array idx |> unwrap == Empty then
            List.append list2 { index & y: index.y - 1 }
        else
            list2

    # Right
    list4 =
        idx = toIndex shape { index & y: index.y + 1 }
        if Array2D.get array idx |> unwrap == Empty then
            List.append list3 { index & y: index.y + 1 }
        else
            list3

    Set.fromList list4

toIndex : { dimX : I64, dimY : I64 }, { x : I64, y : I64 } -> { x : Nat, y : Nat }
toIndex = \{ dimX, dimY }, { x, y } -> {
    x: mod x dimX |> Num.toNat,
    y: mod y dimY |> Num.toNat,
}

mod = \a, b ->
    ((a % b) + b) % b

unwrap = \r ->
    when r is
        Ok v -> v
        Err err ->
            dbg err

            crash "impossible"
