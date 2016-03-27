{- Problem 39: Integer right triangles

If p is the perimeter of a right angle triangle with integral length sides,
{a,b,c}, there are exactly three solutions for p = 120.

  {20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

-}

module Problem39 where
import Data.List (groupBy, nubBy)
import Data.Set (empty, insert, toAscList)
import PythagoreanTriples (triplesForPerimeter)

maxTriangles ts1 ts2 =
  if length ts1 > length ts2
  then ts1
  else ts2

mostTriangles = foldr1 maxTriangles [triplesForPerimeter p | p<-[12,14..1000]]

solve = sum $ head mostTriangles

