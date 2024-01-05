app "day"
    packages {
        pf: "../platform/main.roc",
    }
    imports [
        "day24.input" as puzzleInput : Str,
        Inspect.{ Inspect, Inspector, InspectFormatter },
    ]
    provides [solution] to pf

solution = \part ->
    when part is
        Part1 -> part1 puzzleInput 200000000000000 400000000000000
        Part2 -> part2 puzzleInput

exampleInput =
    """
    19, 13, 30 @ -2,  1, -2
    18, 19, 22 @ -1, -1, -2
    20, 25, 34 @ -2, -2, -4
    12, 31, 28 @ -1, -2, -1
    20, 19, 15 @  1, -5, -3
    """

expect
    got = part1 exampleInput 7 27
    got == "2"

part1 = \input, fromC, toC ->
    input
    |> parseInput
    |> removeZ
    |> countIntercept fromC toC
    |> Num.toStr

parseInput = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        when Str.split line "@" is
            [before, after] ->
                point =
                    before
                    |> Str.trim
                    |> Str.split ","
                    |> List.map \c ->
                        c
                        |> Str.trim
                        |> Str.toI128
                        |> unwrap

                direction =
                    after
                    |> Str.trim
                    |> Str.split ","
                    |> List.map \c ->
                        c
                        |> Str.trim
                        |> Str.toI128
                        |> unwrap

                when (point, direction) is
                    ([p1, p2, p3], [d1, d2, d3]) -> (vFromInts p1 p2 p3, vFromInts d1 d2 d3)
                    _ -> crash "invalid line"

            _ -> crash "line has more then two parts: \(line)"

removeZ = \lines ->
    fn = \v -> vUpdate v (\x, y, _ -> (x, y, qFromInt 0))

    lines
    |> List.map \(point, direction) ->
        (fn point, fn direction)

countIntercept : List (Line a), Int a, Int a -> Nat
countIntercept = \lines, fromC, toC ->
    countInterceptHelper lines (qFromInt fromC) (qFromInt toC) 0

countInterceptHelper : List (Line a), Rational a, Rational a, Nat -> Nat
countInterceptHelper = \lines, fromC, toC, found ->
    when lines is
        [] -> found
        [line1, .. as rest] ->
            newFound =
                List.walk
                    rest
                    found
                    \state, line2 ->
                        when twoLines line1 line2 is
                            Intercept (t1, t2) ->
                                { x, y } = vToCoordinates (lineToVec t1 line1)
                                if qGe x fromC && qLe x toC && qGe y fromC && qLe y toC && qIsPos t1 && qIsPos t2 then
                                    state + 1
                                else
                                    state

                            _ -> state
            countInterceptHelper rest fromC toC newFound

Line a : (Vector a, Vector a)

lineToVec = \t, (p, v) ->
    vAdd p (vMul t v)

twoLines = \(p1, v1), (p2, v2) ->
    if v1 == v2 then
        Parallel
    else
        # p1 + t1 * v1 == p2 + t2 * v2
        # t1 = p2/v1 - p1/v1 + t2 * v2/v1
        # t1 = vSub ((vDiv p2 v1) (vDiv p1 v1)) + t2 * (vDiv v2 v1)
        a = vSub (vDiv p2 v1) (vDiv p1 v1) |> vToCoordinates
        b = vDiv v2 v1 |> vToCoordinates

        # t1 = a.x + t2 * b.x
        # t1 = a.y + t2 * b.y
        # t1 = a.z + t2 * b.z
        # t2 = t1 / b.y - a.y / b.y
        # t1 =  a.x + (t1 / b.y - a.y / b.y) * b.x
        # t1 =  t1 / b.y * b.x     +    a.x - a.y / b.y * b.x
        # t1 =  t1 * b.x / b.y      +    a.x    -    a.y * b.x / b.y
        # (1 - b.x / b.y)  =  (a.x - a.y * b.x / b.y ) / t1

        # t1  = (a.x - a.y * b.x / b.y ) / (1 - b.x / b.y)
        t1 = qDiv (qSub a.x (qDiv (qMul a.y b.x) b.y)) (qSub (qFromInt 1) (qDiv b.x b.y))

        if t1 == qDiv (qSub a.z (qDiv (qMul a.y b.z) b.y)) (qSub (qFromInt 1) (qDiv b.z b.y)) then
            t2 = qSub (qDiv t1 b.y) (qDiv a.y b.y)
            Intercept (t1, t2)
        else
            Skew

expect
    line1 = (vFromInts 20 25 0, vFromInts -2 -2 0)
    line2 = (vFromInts 18 19 0, vFromInts -1 -1 0)
    got = twoLines line1 line2
    got == Parallel

expect
    got = part2 exampleInput
    got == "47"

part2 = \input ->
    when input |> parseInput is
        # First line from test makes problems
        [_, line1, line2, line3, ..] ->
            solutionWithTreyLinesWithFrac line1 line2 line3
            |> Num.toStr

        _ -> crash "need trey lines"

solutionWithTreyLinesWithFrac = \(a0, av), (b0, bv), (c0, cv) ->
    # This is a translation of: https://github.com/DeadlyRedCube/AdventOfCode/blob/1f9d0a3e3b7e7821592244ee51bce5c18cf899ff/2023/AOC2023/D24.h#L66-L294
    { x: a0x, y: a0y, z: a0z } = vToFraq a0
    { x: avx, y: avy, z: avz } = vToFraq av
    { x: b0x, y: b0y, z: b0z } = vToFraq b0
    { x: bvx, y: bvy, z: bvz } = vToFraq bv
    { x: c0x, y: c0y, z: c0z } = vToFraq c0
    { x: cvx, y: cvy, z: cvz } = vToFraq cv

    abvx = avx - bvx
    abvy = avy - bvy
    abvz = avz - bvz

    acvx = avx - cvx
    acvy = avy - cvy
    acvz = avz - cvz

    ab0x = a0x - b0x
    ab0y = a0y - b0y
    ab0z = a0z - b0z

    ac0x = a0x - c0x
    ac0y = a0y - c0y
    ac0z = a0z - c0z

    h0 = (b0y * bvx - b0x * bvy) - (a0y * avx - a0x * avy)
    h1 = (c0y * cvx - c0x * cvy) - (a0y * avx - a0x * avy)
    h2 = (b0x * bvz - b0z * bvx) - (a0x * avz - a0z * avx)
    h3 = (c0x * cvz - c0z * cvx) - (a0x * avz - a0z * avx)
    h4 = (b0z * bvy - b0y * bvz) - (a0z * avy - a0y * avz)
    h5 = (c0z * cvy - c0y * cvz) - (a0z * avy - a0y * avz)

    pxx = acvx * ab0z - abvx * ac0z
    pyy = acvy * ab0x - abvy * ac0x
    pzz = acvz * ab0y - abvz * ac0y

    pxz = abvx * ac0x - acvx * ab0x
    pzy = abvz * ac0z - acvz * ab0z
    pyx = abvy * ac0y - acvy * ab0y

    pxc = abvx * h3 - acvx * h2
    pyc = abvy * h1 - acvy * h0
    pzc = abvz * h5 - acvz * h4

    pxd = acvx * abvz - abvx * acvz
    pyd = acvy * abvx - abvy * acvx
    pzd = acvz * abvy - abvz * acvy

    qz0 = (abvy / pxd) * pxz
    qx0 = (abvy / pxd) * pxx - (abvx / pyd) * pyx - ab0y
    qy0 = ab0x - (abvx / pyd) * pyy
    r0 = h0 - (abvy / pxd) * pxc + (abvx / pyd) * pyc

    qy1 = (abvx / pzd) * pzy
    qz1 = (abvx / pzd) * pzz - (abvz / pxd) * pxz - ab0x
    qx1 = ab0z - (abvz / pxd) * pxx
    r1 = h2 - (abvx / pzd) * pzc + (abvz / pxd) * pxc

    qx2 = (abvz / pyd) * pyx
    qy2 = (abvz / pyd) * pyy - (abvy / pzd) * pzy - ab0z
    qz2 = ab0y - (abvy / pzd) * pzz
    r2 = h4 - (abvz / pyd) * pyc + (abvy / pzd) * pzc

    qz =
        ((qx1 * qy0 - qx0 * qy1) * (qx2 * r0 - qx0 * r2) - (qx2 * qy0 - qx0 * qy2) * (qx1 * r0 - qx0 * r1))
        / ((qx2 * qy0 - qx0 * qy2) * (qx0 * qz1 - qx1 * qz0) - (qx1 * qy0 - qx0 * qy1) * (qx0 * qz2 - qx2 * qz0))

    qy = ((qx0 * qz1 - qx1 * qz0) * qz + (qx1 * r0 - qx0 * r1)) / (qx1 * qy0 - qx0 * qy1)

    qx = (r0 - qy0 * qy - qz0 * qz) / qx0

    px = (pxz * qz + pxx * qx + pxc) / pxd
    py = (pyx * qx + pyy * qy + pyc) / pyd
    pz = (pzy * qy + pzz * qz + pzc) / pzd

    Num.round px + Num.round py + Num.round pz

solutionWithTreyLines = \(a0, av), (b0, bv), (c0, cv) ->
    { x: a0x, y: a0y, z: a0z } = vToCoordinates a0
    { x: avx, y: avy, z: avz } = vToCoordinates av
    { x: b0x, y: b0y, z: b0z } = vToCoordinates b0
    { x: bvx, y: bvy, z: bvz } = vToCoordinates bv
    { x: c0x, y: c0y, z: c0z } = vToCoordinates c0
    { x: cvx, y: cvy, z: cvz } = vToCoordinates cv

    abvx = qSub avx bvx
    abvy = qSub avy bvy
    abvz = qSub avz bvz

    acvx = qSub avx cvx
    acvy = qSub avy cvy
    acvz = qSub avz cvz

    ab0x = qSub a0x b0x
    ab0y = qSub a0y b0y
    ab0z = qSub a0z b0z

    ac0x = qSub a0x c0x
    ac0y = qSub a0y c0y
    ac0z = qSub a0z c0z

    h0 = ((qMul b0y bvx) |> qSub (qMul b0x bvy)) |> qSub ((qMul a0y avx) |> qSub (qMul a0x avy))
    h1 = ((qMul c0y cvx) |> qSub (qMul c0x cvy)) |> qSub ((qMul a0y avx) |> qSub (qMul a0x avy))
    h2 = ((qMul b0x bvz) |> qSub (qMul b0z bvx)) |> qSub ((qMul a0x avz) |> qSub (qMul a0z avx))
    h3 = ((qMul c0x cvz) |> qSub (qMul c0z cvx)) |> qSub ((qMul a0x avz) |> qSub (qMul a0z avx))
    h4 = ((qMul b0z bvy) |> qSub (qMul b0y bvz)) |> qSub ((qMul a0z avy) |> qSub (qMul a0y avz))
    h5 = ((qMul c0z cvy) |> qSub (qMul c0y cvz)) |> qSub ((qMul a0z avy) |> qSub (qMul a0y avz))

    pxx = (qMul acvx ab0z) |> qSub (qMul abvx ac0z)
    pyy = (qMul acvy ab0x) |> qSub (qMul abvy ac0x)
    pzz = (qMul acvz ab0y) |> qSub (qMul abvz ac0y)

    pxz = (qMul abvx ac0x) |> qSub (qMul acvx ab0x)
    pzy = (qMul abvz ac0z) |> qSub (qMul acvz ab0z)
    pyx = (qMul abvy ac0y) |> qSub (qMul acvy ab0y)

    pxc = (qMul abvx h3) |> qSub (qMul acvx h2)
    pyc = (qMul abvy h1) |> qSub (qMul acvy h0)
    pzc = (qMul abvz h5) |> qSub (qMul acvz h4)

    pxd = (qMul acvx abvz) |> qSub (qMul abvx acvz)
    pyd = (qMul acvy abvx) |> qSub (qMul abvy acvx)
    pzd = (qMul acvz abvy) |> qSub (qMul abvz acvy)

    qz0 = (qDiv abvy pxd) |> qMul pxz
    qx0 = ((qDiv abvy pxd) |> qMul pxx) |> qSub ((qDiv abvx pyd) |> qMul pyx) |> qSub ab0y
    qy0 = ab0x |> qSub ((qDiv abvx pyd) |> qMul pyy)
    r0 = h0 |> qSub ((qDiv abvy pxd) |> qMul pxc) |> qAdd ((qDiv abvx pyd) |> qMul pyc)

    qy1 = (qDiv abvx pzd) |> qMul pzy
    qz1 = ((qDiv abvx pzd) |> qMul pzz) |> qSub ((qDiv abvz pxd) |> qMul pxz) |> qSub ab0x
    qx1 = ab0z |> qSub ((qDiv abvz pxd) |> qMul pxx)
    r1 = h2 |> qSub ((qDiv abvx pzd) |> qMul pzc) |> qAdd ((qDiv abvz pxd) |> qMul pxc)

    qx2 = (qDiv abvz pyd) |> qMul pyx
    qy2 = ((qDiv abvz pyd) |> qMul pyy) |> qSub ((qDiv abvy pzd) |> qMul pzy) |> qSub ab0z
    qz2 = ab0y |> qSub ((qDiv abvy pzd) |> qMul pzz)
    r2 = h4 |> qSub ((qDiv abvz pyd) |> qMul pyc) |> qAdd ((qDiv abvy pzd) |> qMul pzc)

    qzp11 = (qMul qx1 qy0) |> qSub (qMul qx0 qy1)

    qzp12 = (qMul qx2 r0) |> qSub (qMul qx0 r2)

    qzp13 = (qMul qx2 qy0) |> qSub (qMul qx0 qy2)

    qzp14 = (qMul qx1 r0) |> qSub (qMul qx0 r1)

    qzpart1 = (qMul qzp11 qzp12) |> qSub (qMul qzp13 qzp14)

    qzp21 = (qMul qx2 qy0) |> qSub (qMul qx0 qy2)
    qzp22 = (qMul qx0 qz1) |> qSub (qMul qx1 qz0)
    qzp23 = (qMul qx1 qy0) |> qSub (qMul qx0 qy1)
    qzp24 = (qMul qx0 qz2) |> qSub (qMul qx2 qz0)

    qzpart2 = (qMul qzp21 qzp22) |> qSub (qMul qzp23 qzp24)

    qz = qDiv qzpart1 qzpart2

    qy = ((qMul ((qMul qx0 qz1) |> qSub (qMul qx1 qz0)) qz) |> qAdd ((qMul qx1 r0) |> qSub (qMul qx0 r1))) |> qSub ((qMul qx1 qy0) |> qSub (qMul qx0 qy1))

    qx = (r0 |> qSub (qMul qy0 qy) |> qSub (qMul qz0 qz)) |> qDiv qx0

    px = ((qMul pxz qz) |> qAdd (qMul pxx qx) |> qAdd pxc) |> qDiv pxd
    py = ((qMul pyx qx) |> qAdd (qMul pyy qy) |> qAdd pyc) |> qDiv pyd
    pz = ((qMul pzy qy) |> qAdd (qMul pzz qz) |> qAdd pzc) |> qDiv pzd

    v1 = qToNumDen px |> .0
    v2 = qToNumDen py |> .0
    v3 = qToNumDen pz |> .0

    v1 + v2 + v3

expect
    when exampleInput |> parseInput is
        # First line from test makes problems
        [_, line1, line2, line3, ..] ->
            got = solutionWithTreyLines line1 line2 line3
            got == 47

        _ -> crash "need trey lines"

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"

Rational a := (Int a, Int a)
    implements [
        Eq { isEq: qEq },
        Inspect {
            toInspector: toInspectorRational,
        },
    ]

toInspectorRational = \@Rational (a, b) ->
    f0 <- Inspect.custom
    "\(a |> Num.toStr) / \(b |> Num.toStr)"
    |> Inspect.str
    |> Inspect.apply f0

qFromInt : Int a -> Rational a
qFromInt = \i ->
    @Rational (i, 1)

qFromNumDen : Int a, Int a -> Rational a
qFromNumDen = \n, d ->
    @Rational (n, d)
    |> qNormalize

qToNumDen = \@Rational (n, d) ->
    (n, d)

qToFrac = \@Rational (n, d) ->
    (n |> Num.toF64) / (d |> Num.toF64)

qNormalize = \@Rational (a, b) ->
    if b == 0 then
        @Rational (a, b)
    else
        d = gcd a b
        @Rational (a // d, b // d)

gcd = \a, b ->
    if a == 0 then
        b
    else
        gcd (b % a) a

qEq = \@Rational (a, b), @Rational (c, d) ->
    a * d == c * b

qLe = \@Rational (a, b), @Rational (c, d) ->
    a * d < b * c

qGe = \@Rational (a, b), @Rational (c, d) ->
    a * d > b * c

qIsPos = \@Rational (a, b) ->
    Num.isPositive a == Num.isPositive b

expect
    qFromInt 1 != qFromInt 2

expect
    r1 = qFromInt 1
    r2 = qFromNumDen 2 2
    r1 == r2

expect
    r1 = qFromInt 0
    r2 = qFromNumDen 0 24
    r1 == r2

qAdd = \@Rational (a, b), @Rational (c, d) ->
    @Rational (a * d + b * c, b * d)
    |> qNormalize

qSub = \@Rational (a, b), @Rational (c, d) ->
    @Rational (a * d - b * c, b * d)
    |> qNormalize

qMul = \@Rational (a, b), @Rational (c, d) ->
    @Rational (a * c, b * d)
    |> qNormalize

qDiv = \@Rational (a, b), @Rational (c, d) ->
    @Rational (a * d, c * b)
    |> qNormalize

Vector a := (Rational a, Rational a, Rational a)
    implements [
        Eq { isEq: vEq },
        Inspect {
            toInspector: toInspectorVector,
        },
    ]

toInspectorVector = \@Vector (x, y, z) ->
    f0 <- Inspect.custom
    [x, y, z]
    |> List.map Inspect.toInspector
    |> Inspect.tuple
    |> Inspect.apply f0

vFromInts = \n1, n2, n3 ->
    @Vector (qFromInt n1, qFromInt n2, qFromInt n3)

vToCoordinates = \@Vector (x, y, z) ->
    { x: x, y: y, z: z }

vToFraq = \@Vector (x, y, z) ->
    { x: qToFrac x, y: qToFrac y, z: qToFrac z }

vEq = \@Vector (v1, v2, v3), @Vector (w1, w2, w3) ->
    t1 = qDiv v1 w1
    t2 = qDiv v2 w2
    t3 = qDiv v3 w3
    t1 == t2 && t2 == t3

expect
    v1 = vFromInts 1 1 1
    v2 = vFromInts 2 2 2
    v1 == v2

expect
    v1 = vFromInts 1 2 3
    v2 = vFromInts 2 4 6
    v1 == v2

expect
    zero = vFromInts 0 0 0
    zero == zero

vDiv = \@Vector (v1, v2, v3), @Vector (w1, w2, w3) ->
    @Vector (
        qDiv v1 w1,
        qDiv v2 w2,
        qDiv v3 w3,
    )

vMul = \q, @Vector (v1, v2, v3) ->
    @Vector (
        qMul q v1,
        qMul q v2,
        qMul q v3,
    )

vAdd = \@Vector (v1, v2, v3), @Vector (w1, w2, w3) ->
    @Vector (
        qAdd v1 w1,
        qAdd v2 w2,
        qAdd v3 w3,
    )

vSub = \@Vector (v1, v2, v3), @Vector (w1, w2, w3) ->
    @Vector (
        qSub v1 w1,
        qSub v2 w2,
        qSub v3 w3,
    )

vUpdate = \@Vector (v1, v2, v3), fn ->
    (n1, n2, n3) = fn v1 v2 v3
    @Vector (
        n1,
        n2,
        n3,
    )
