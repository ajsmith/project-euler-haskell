{- Problem 5: Smallest multiple

2520 is the smallest number that can be divided by each of the numbers from 1
to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?
-}

module Problem5 where

n = 20

divisibleByRangeN x = [1..n] == [y | y<-[1..n], x `mod` y == 0]

solve _ = head (filter divisibleByRangeN [1..])
