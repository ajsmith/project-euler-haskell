module Fibonacci where

{- Calculate Fibonacci numbers -}

fibonacci n = head $ reverse $ take n fibonacciSeries

fibonacciSeries = [1, 1] ++ (generateFibonacci 1 1)
  where
    generateFibonacci b a = (b + a):(generateFibonacci (b + a) b)
