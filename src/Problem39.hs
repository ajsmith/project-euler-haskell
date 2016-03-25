{- Problem 39: Integer right triangles

If p is the perimeter of a right angle triangle with integral length sides,
{a,b,c}, there are exactly three solutions for p = 120.

  {20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

-}

module Problem39 where
import Data.List (groupBy, nub, nubBy, sort, sortBy)
import Data.Ord (compare)

trianglesFor :: Integral a => a -> [[a]]
trianglesFor p = nubBy (\ls1 ls2 -> head ls1 == head ls2) sides
  where
    sides = [[c, a, b] | a<-ls, b<-ls, c<-ls, a + b + c == p, a^2 + b^2 == c^2]
    ls = [1..p]

triangles :: Integral a => [[a]]
triangles = [c:(sort [a, b]) | a<-ls, b<-ls, c<-ls, a + b + c <= limit, a^2 + b^2 == c^2]
  where
    limit = 1000
    ls = [1..(limit-2)]

trianglesByPerimeter = groupBy groupByPerimeter $ sortBy orderByPerimiter $ map (\t -> (sum t, t)) triangles
  where
    groupByPerimeter (p1, _) (p2, _) = p1 == p2
    orderByPerimiter (p1, _) (p2, _) = compare p2 p1

maxTriangles ts1 ts2 =
  if length ts1 > length ts2
  then ts1
  else ts2

mostTriangles = foldr1 maxTriangles trianglesByPerimeter

solve = fst $ head mostTriangles
