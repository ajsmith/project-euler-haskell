{- Pandigital Numbers

A pandigital number is a number which contains the digits 1-9 exactly once.
-}

module Pandigital where
import qualified Data.Set as Set
import MathUtils (digits)

pandigits = Set.fromList [1..9]

isPandigital x =
  x >= 123456789 &&
  x <= 987654321 &&
  (Set.fromList . digits) x == pandigits
