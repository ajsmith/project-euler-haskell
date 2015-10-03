{- Problem 37: Truncatable primes

The number 3797 has an interesting property. Being prime itself, it is possible
to continuously remove digits from left to right, and remain prime at each
stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797,
379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to
right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

-}

module Problem37 where
import Primes

digits x = digits' x []
  where
    digits' 0 ds = ds
    digits' x ds = digits' (x `div` 10) ((x `mod` 10):ds)

joinDigits ds = join' 0 (reverse ds)
  where
    join' _ [] = 0
    join' p (d:ds) = (10^p * d) + (join' (p + 1) ds)

leftTruncated x = leftTruncated' [] (digits x)
  where
    leftTruncated' xs [] = xs
    leftTruncated' xs ds = leftTruncated' ((joinDigits ds):xs) (tail ds)

rightTruncated x = rightTruncated' [] (reverse $ digits x)
  where
    rightTruncated' xs [] = xs
    rightTruncated' xs ds = rightTruncated' ((joinDigits $ reverse ds):xs) (tail ds)

isLeftTruncatablePrime x = (filter isPrime (leftTruncated x)) == (leftTruncated x)

isRightTruncatablePrime x = (filter isPrime (rightTruncated x)) == (rightTruncated x)

isTruncatablePrime x = isLeftTruncatablePrime x && isRightTruncatablePrime x

solve = sum $ take 11 [x | x<-primes, x > 10 && isTruncatablePrime x]
