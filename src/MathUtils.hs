module MathUtils where

factorial x = foldl (*) 1 [2..x]

nPr n r = (factorial n) `div` (factorial (n - r))

nCr n r = (nPr n r) `div` (factorial r)

digits x = digits' x []
  where
    digits' 0 ds = ds
    digits' x ds = digits' (x `div` 10) ((x `mod` 10):ds)

joinDigits ds = join' 0 (reverse ds)
  where
    join' _ [] = 0
    join' p (d:ds) = (10^p * d) + (join' (p + 1) ds)
