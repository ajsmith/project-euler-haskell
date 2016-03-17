{- Problem 92: Square digit chains

A number chain is created by continuously adding the square of the digits in a
number to form a new number until it has been seen before.

For example,

    44 → 32 → 13 → 10 → 1 → 1
    85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless
loop. What is most amazing is that EVERY starting number will eventually arrive
at 1 or 89.

How many starting numbers below ten million will arrive at 89?

-}

module Problem92 where
import MathUtils (digits)

resolveChain x
  | x == 1    = 1
  | x == 89   = 89
  | otherwise = resolveChain $ sum [a^2 | a<-(digits x)]

solve = fromIntegral . length $ filter (\x -> x == 89) [resolveChain x | x<-[2..limit]]
  where
    limit = 10^7 - 1
