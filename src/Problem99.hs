{- Problem 99: Largest exponential

Comparing two numbers written in index form like 2^11 and 3^7 is not difficult,
as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.

However, confirming that 632382^518061 > 519432^525806 would be much more
difficult, as both numbers contain over three million digits.

Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file
containing one thousand lines with a base/exponent pair on each line, determine
which line number has the greatest numerical value.

NOTE: The first two lines in the file represent the numbers in the example
given above.

-}

module Problem99 where
import ListUtils (splitOn)

maxExponentPair = maxExponentPair' (0, 0, 1) 1
  where
    maxExponentPair' result ln [] = result
    maxExponentPair' (ln, b1, e1) n ([b2,e2]:lst) =
      if b1^e1 > b2^e2
      then maxExponentPair' (ln, b1, e1) (n + 1) lst
      else maxExponentPair' (n, b2, e2) (n + 1) lst

doit = do
  source <- readFile "p099_base_exp.txt"
  putStrLn $ "Solution: " ++ show (maxExponentPair $ parse source)
  where
    parse s = [map (\t -> read t :: Integer) inputs | inputs<-(tokenize s)]
    tokenize = (map (splitOn ',')) . lines
