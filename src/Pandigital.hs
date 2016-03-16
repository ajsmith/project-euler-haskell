{- Pandigital Numbers

A n-digit pandigital number is a number which contains the digits 1-n exactly once.

-}

module Pandigital where
import Data.List (sort)
import MathUtils (digits, digitCount)

isPandigital x = (sort . digits) x == [1..n]
  where
    n = digitCount x
