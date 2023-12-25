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
    lines
    |> List.map \(point, direction) ->
        (vRemoveZ point, vRemoveZ direction)

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

# twoLines : (Vector a, Vector a), (Vector a, Vector a) -> [Parallel, Intercept (Rational a, Rational a) , Skew]
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

part2 = \_ ->
    "Not implemented yet"

unwrap = \r ->
    when r is
        Ok v -> v
        _ -> crash "impossible"

Rational a := (Int a, Int a)
    implements [
        Eq { isEq: qEq },
        # Inspect {
        #     toInspector: toInspectorRational,
        # },
    ]

# qToStr = \@Rational (a, b) ->
#     "\(a |> Num.toStr) / \(b |> Num.toStr)"

qFromInt : Int a -> Rational a
qFromInt = \i ->
    @Rational (i, 1)

qFromNumDen : Int a, Int a -> Rational a
qFromNumDen = \n, d ->
    @Rational (n, d)
    |> qNormalize

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
    ]

vFromInts = \n1, n2, n3 ->
    @Vector (qFromInt n1, qFromInt n2, qFromInt n3)

vToCoordinates = \@Vector (x, y, z) ->
    { x: x, y: y, z: z }

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

vRemoveZ = \@Vector (v1, v2, _) ->
    @Vector (
        v1,
        v2,
        qFromInt 0,
    )
