module Problem0026 where
import Data.Number.BigFloat

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

digitCount :: Integer -> Int
digitCount = length . show

digits n =
  if n > 9
  then (digits (n `div` 10)) ++ [n `mod` 10]
  else [n]

findLongestCycle = findLongestCycle' reciprocalCycles
  where
    findLongestCycle' pairs = foldl1 maxCycle pairs
    maxCycle (d1, x1) (d2, x2) =
      if cycleLength x1 > cycleLength x2
      then (d1, x1)
      else (d2, x2)

reciprocalCycles = [(x, (1 / fromIntegral x):: BigFloat Prec50) | x<-[2..1000], x `gcd` 10 == 1]

-- cycle :: (RealFrac a, Integral b) => a -> b
cycle x = round (x * 10^p - x)
  where
    p = cycleLength x

-- cycleLength :: (RealFrac a, Integral b) => a -> b
cycleLength x = cycleLength' x 1
  where
    -- cycleLength' :: (RealFrac a, Integral b) => a -> b -> b
    cycleLength' x p =
      if checkCycle x p
      then p
      else cycleLength' x (p + 1)

-- checkCycle :: (RealFrac a, Integral b) => a -> b -> Bool
checkCycle x p = checkError y
  where
    y = x * 10^p - x

-- checkError :: RealFrac a => a -> Bool
checkError x = errorMargin x < errorTolerance

-- errorMargin :: RealFrac a => a -> a
errorMargin x = abs (fromIntegral (round x) - x)

-- errorTolerance :: RealFrac a => a
errorTolerance = (1 / 10^45) :: BigFloat Prec50
