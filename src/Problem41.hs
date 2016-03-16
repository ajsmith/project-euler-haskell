{- Problem 41: Pandigital prime

We shall say that an n-digit number is pandigital if it makes use of all the
digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
also prime.

What is the largest n-digit pandigital prime that exists?

-}

module Problem41 where
import Pandigital (isPandigital)
import Primes (isPrime)

pandigitalPrimes = [-x | x<-[(-987654321)..(-2)], isPandigital (-x) && isPrime (-x)]

solve = head pandigitalPrimes
