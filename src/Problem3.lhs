Problem 3: Largest prime factor

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?

> module Problem3 where
> import Primes

> x = 600851475143

> solve = foldl max 0 (primeDivisors x)
