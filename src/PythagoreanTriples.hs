{- Generate Pythagorean Triples using Euclid's formula. -}

module PythagoreanTriples where

triple m n =
  if a < b
  then [a, b, c]
  else [b, a, c]
  where
    a = m^2 - n^2
    b = 2 * m * n
    c = m^2 + n^2

triplesForPerimeter p = [scale t | t<-primitives]
  where
    scale t = map (*(p `div` (sum t))) t
    primitives = [triple m n | m<-[1..(p `div` 2)], n<-(ns m), lcm p (2 * m * (m + n)) == p]
    ns m = [n | n<-[1..(m - 1)], odd (m - n), gcd m n == 1]

-- primitivesTo :: Integral a => a -> [[a]]
primitivesTo p = [triple m n | m<-[2..maxM], n<-(ns m), p >= 2 * m * (m + n)]
  where
    maxM = (ceiling . sqrt) ((fromIntegral p) / 2)
    ns m =
      if odd m
      then [n | n<-[2,4..(m - 1)], gcd m n == 1]
      else [n | n<-[1,3..(m - 1)], gcd m n == 1]

primitiveTriples = [triple m n | m<-[1..], n<-(ns m)]
  where
    ns m = [n | n<-[1..(m - 1)], odd (m - n), gcd m n == 1]
