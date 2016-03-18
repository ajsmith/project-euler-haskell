{- Problem 35: Circular primes

The number, 197, is called a circular prime because all rotations of the
digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
73, 79, and 97.

How many circular primes are there below one million?

-}

module Problem35 where
import MathUtils (digits, joinDigits)
import Primes (isPrime)

isCircularPrime x = isCircularPrime' (digits x) []
  where
    isCircularPrime' [] ys = isPrime $ joinDigits ys
    isCircularPrime' (x:xs) ys =
      if isPrime $ joinDigits (xs ++ (ys ++ [x]))
      then isCircularPrime' xs (ys ++ [x])
      else False

circularPrimes n = filter isCircularPrime [2..n]

solve = fromIntegral . length $ circularPrimes n
  where
    n = 10^6 - 1
