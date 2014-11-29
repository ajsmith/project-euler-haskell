module Problem0026 where
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
    (z, ds) = properFraction (10^n / y - 1 / y)

findLongestCycle = (x, n, (one / (fromIntegral x)))
  where
    (x, n) = foldr1 maxCycle [(x, check x 1) | x<-reciprocalCycles]
    one = preciseFloat 1
    maxCycle (x1, n1) (x2, n2) =
      if n1 > n2
      then (x1, n1)
      else (x2, n2)

divisors :: Integer -> [Integer]
divisors n
  | n == 1 = [1]
  | n > 0 = 1:(xs ++ ys)
  | otherwise = error "Value must be greater than zero."
  where
    isqrt = floor . sqrt . fromIntegral
    xs = [x | x<-[2 .. isqrt n], n `mod` x == 0]
    ys = [n `div` x | x<-xs, x /= n `div` x]

isPrime 1 = False
isPrime n = divisors n == [1]

primeDivisors n = filter isPrime (divisors n)
