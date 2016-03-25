{- List Utilities -}

module ListUtils where

splitOn d ls
  | elem d ls = next:(splitOn d $ tail remain)
  | otherwise = [ls]
  where
    (next, remain) = break (== d) ls
