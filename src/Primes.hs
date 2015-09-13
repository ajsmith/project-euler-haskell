module Primes where

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

primes = [x | x<-[2..], isPrime x]
