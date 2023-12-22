app "day"
    packages {
        pf: "../platform/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        "day22.input" as puzzleInput : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput
        Part2 -> part2 puzzleInput

exampleInput =
    """
    1,0,1~1,2,1
    0,0,2~2,0,2
    0,2,3~2,2,3
    0,0,4~0,2,4
    2,0,5~2,2,5
    0,1,6~2,1,6
    1,1,8~1,1,9
    """

expect
    got = part1 exampleInput
    got == "5"

part1 = \input ->
    input
    |> parseInput
    |> sortBricks
    |> dropBricks
    |> countDisintegratedBricks
    |> Num.toStr

Point : { x : U64, y : U64, z : U64 }
Brick : { start : Point, end : Point }

parseInput : Str -> List Brick
parseInput = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        line
        |> Str.split "~"
        |> List.map \point ->
            point
            |> Str.split ","
            |> List.map \n ->
                n
                |> Str.toU64
                |> unwrap
            |> \numbers ->
                when numbers is
                    [x, y, z] -> { x, y, z }
                    _ -> crash "point has not 3 dimensions"
        |> \points ->
            when points is
                [start, end] -> { start, end }
                _ -> crash "line has not two points"

dropBricks : List Brick -> List Brick
dropBricks = \brickList ->
    brickList
    |> findGrounded
    |> dropBricksHelper

sortBricks : List Brick -> List Brick
sortBricks = \brickList ->
    brickList
    |> List.sortWith
        \b1, b2 ->
            Num.compare b1.start.z b2.start.z

findGrounded : List Brick -> (List Brick, Array2D U64, List Brick)
findGrounded = \brickList ->
    shape = findShape brickList
    initZMap = Array2D.init shape (\_ -> 0)

    List.walk
        brickList
        ([], initZMap, [], Bool.false)
        \(grounded, zMap, inAir, foundOnAir), brick ->
            if foundOnAir then
                (
                    grounded,
                    zMap,
                    List.append inAir brick,
                    Bool.true,
                )
            else if isOnGround zMap brick then
                (
                    List.append grounded brick,
                    addToZMap zMap brick,
                    inAir,
                    Bool.false,
                )
            else
                (
                    grounded,
                    zMap,
                    List.append inAir brick,
                    Bool.true,
                )
    |> \(a, b, c, _) ->
        (a, b, c)

findShape = \brickList ->
    List.walk
        brickList
        { dimX: 0, dimY: 0 }
        \{ dimX, dimY }, brick -> {
            dimX: Num.max dimX (Num.max brick.start.x brick.end.x),
            dimY: Num.max dimY (Num.max brick.start.y brick.end.y),
        }
    |> \{ dimX, dimY } -> {
        dimX: dimX + 1 |> Num.toNat,
        dimY: dimY + 1 |> Num.toNat,
    }

addToZMap : Array2D U64, Brick -> Array2D U64
addToZMap = \zMap, { start: { x: startX, y: startY }, end: { x: endX, y: endY, z } } ->
    List.walk
        (List.range { start: At startX, end: At endX })
        zMap
        \zMap1, x ->
            List.walk
                (List.range { start: At startY, end: At endY })
                zMap1
                \zMap2, y ->
                    Array2D.set
                        zMap2
                        { x: x |> Num.toNat, y: y |> Num.toNat }
                        z

isOnGround : Array2D U64, Brick -> Bool
isOnGround = \zMap, { start: { x: startX, y: startY, z }, end: { x: endX, y: endY } } ->
    List.findFirst
        (List.range { start: At startX, end: At endX })
        \x ->
            List.findFirst
                (List.range { start: At startY, end: At endY })
                \y ->
                    v = Array2D.get
                        zMap
                        { x: x |> Num.toNat, y: y |> Num.toNat }
                    when v is
                        Ok n ->
                            z == n + 1

                        Err _ ->
                            dbg (Array2D.shape zMap, x, y)

                            crash "index not in zMap"
            |> Result.isOk
    |> Result.isOk

dropBricksHelper : (List Brick, Array2D U64, List Brick) -> List Brick
dropBricksHelper = \(grounded, zMap, inAir) ->
    when inAir is
        [] -> grounded
        [brick, .. as rest] ->
            (newZMap, dropped) = dropBrick zMap brick

            dropBricksHelper (
                List.append grounded dropped,
                newZMap,
                rest,
            )

dropBrick = \zMap, brick ->
    if isOnGround zMap brick then
        (
            addToZMap zMap brick,
            brick,
        )
    else
        dropBrick zMap (oneDown brick)

oneDown : Brick -> Brick
oneDown = \{ start, end } ->
    { start: { start & z: start.z - 1 }, end: { end & z: end.z - 1 } }

countDisintegratedBricks : List Brick -> U64
countDisintegratedBricks = \bricks ->
    List.walkWithIndex
        bricks
        0
        \count, _, index ->
            removedBrick = List.dropAt bricks index
            dropedRemoved = dropBricks removedBrick

            if dropedRemoved == removedBrick then
                count + 1
            else
                count

expect
    got = part2 exampleInput
    got == "7"

part2 = \input ->
    input
    |> parseInput
    |> sortBricks
    |> dropBricks
    |> countFallingBricks
    |> Num.toStr

countFallingBricks = \bricks ->
    List.walkWithIndex
        bricks
        0
        \count, _, index ->
            removedBrick = List.dropAt bricks index
            dropedRemoved = dropBricks removedBrick

            count + countFallen removedBrick dropedRemoved

countFallen = \bricks1, bricks2 ->
    List.map2 bricks1 bricks2 \a, b ->
        if a == b then
            0
        else
            1
    |> List.sum

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"
