{- Problem 48: Self powers

The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

-}

module Problem48 where

solve = (sum [x^x `mod` 10^10 | x<-[1..1000]]) `mod` 10^10
