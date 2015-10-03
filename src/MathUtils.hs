module MathUtils where

factorial x = foldl (*) 1 [2..x]

nPr n r = (factorial n) `div` (factorial (n - r))

nCr n r = (nPr n r) `div` (factorial r)
