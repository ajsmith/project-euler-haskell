Problem 4: Largest palindrome product

A palindromic number reads the same both ways. The largest palindrome made from
the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

> module Problem4 where
> import Primes

> isPalindrome x = show x == (reverse $ show x)

> factors = [100..999]

> products = [x * y | x<-factors, y<-factors]

> palindromes = filter isPalindrome products

> solve = foldl max 0 palindromes
