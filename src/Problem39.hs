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

-- trianglesFor :: Integral a => a -> [[a]]
-- trianglesFor p = nubBy (\ls1 ls2 -> head ls1 == head ls2) sides
--   where
--     sides = [[c, a, b] | a<-ls, b<-ls, c<-ls, a + b + c == p, a^2 + b^2 == c^2]
--     ls = [1..p]

-- triangles :: Integral a => [[a]]
-- triangles = [tri a b c | a<-ls, b<-ls, c<-ls, a + b + c <= limit, a^2 + b^2 == c^2]
--   where
--     limit = 1000
--     ls = [1..(limit-2)]
--     tri a b c
--       | a < b     = [a, b, c]
--       | otherwise = [b, a, c]

-- trianglesByPerimeter = groupBy groupByPerimeter $ toAscList $ foldr insert empty triangles
--   where
--     groupByPerimeter ts1 ts2 = sum ts1 == sum ts2

maxTriangles ts1 ts2 =
  if length ts1 > length ts2
  then ts1
  else ts2

mostTriangles = foldr1 maxTriangles [triplesForPerimeter p | p<-[12,14..1000]]

solve = sum $ head mostTriangles

