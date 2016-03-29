{- Problem 75: Singular integer right triangles

It turns out that 12 cm is the smallest length of wire that can be bent to form
an integer sided right angle triangle in exactly one way, but there are many
more examples.

    12 cm: (3,4,5)
    24 cm: (6,8,10)
    30 cm: (5,12,13)
    36 cm: (9,12,15)
    40 cm: (8,15,17)
    48 cm: (12,16,20)

In contrast, some lengths of wire, like 20 cm, cannot be bent to form an
integer sided right angle triangle, and other lengths allow more than one
solution to be found; for example, using 120 cm it is possible to form exactly
three different integer sided right angle triangles.

    120 cm: (30,40,50), (20,48,52), (24,45,51)

Given that L is the length of the wire, for how many values of L ≤ 1,500,000
can exactly one integer sided right angle triangle be formed?

-}

module Problem75 where
import PythagoreanTriples (primitivesTo)
import Data.List (group, sort, sortBy)
import Data.Set (empty, insert, toAscList)
import Debug.Trace (trace, traceShow, traceShowId)

n = 1500000

primitivePerimiters = toAscList $ foldr insert empty [sum t | t<-(primitivesTo n)]

similarTriangles'' [] _ _ = []
similarTriangles'' (p:ps) left right =
  (filter (isSimilar (traceShowId p)) left'):(similarTriangles'' ps left' right')
  where
    left' = left ++ (takeWhile lte right)
    right' = dropWhile lte right
    lte p2 = p2 <= p
    isSimilar p1 p2 = lcm p1 p2 == p1
singularTriangles = filter isSingular $ similarTriangles'' [12,14..n] [] primitivePerimiters

isSingular (t:[]) = True
isSingular _ = False

solve = (fromIntegral . length) singularTriangles
