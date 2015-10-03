{- Problem 26: Reciprocal Cycles

A unit fraction contains 1 in the numerator. The decimal representation of the
unit fractions with denominators 2 to 10 are given:

    1/2  = 0.5
    1/3  = 0.(3)
    1/4  = 0.25
    1/5  = 0.2
    1/6  = 0.1(6)
    1/7  = 0.(142857)
    1/8  = 0.125
    1/9  = 0.(1)
    1/10 = 0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle
in its decimal fraction part.

-}

module Problem26 where
import Data.Number.BigFloat

-- There's probably a better way of doing this, but I'm bad at Haskell :/
preciseFloat x = fromIntegral x :: BigFloat (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 Prec50 ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

errorTolerance = (preciseFloat 1) / 10^1950

reciprocalCycles = [x | x<-[2..1000], x `gcd` 10 == 1]

check x n =
  if (abs ds) < errorTolerance
  then n
  else check x (n + 1)
  where
    y = preciseFloat x
    (z, ds) = properFraction ((10^n - 1) / y)

-- findLongestCycle = (x, n, (one / (fromIntegral x))) -- for debugging
findLongestCycle = x
  where
    (x, n) = foldr1 maxCycle [(x, check x 1) | x<-reciprocalCycles]
    one = preciseFloat 1
    maxCycle (x1, n1) (x2, n2) =
      if n1 > n2
      then (x1, n1)
      else (x2, n2)

solve = findLongestCycle
