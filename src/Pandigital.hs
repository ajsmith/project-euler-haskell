{- Pandigital Numbers

A n-digit pandigital number is a number which contains the digits 1-n exactly once.
-}

module Pandigital where
import Data.List (sort)
import MathUtils (digits)

isPandigital x = isPandigital' n x
  where
    isPandigital' n x = (sort . digits) x == [1..n]
    log10 x = (log  x) / (log 10)
    y = fromIntegral x
    n = ceiling $ (log10 (y + 1))
