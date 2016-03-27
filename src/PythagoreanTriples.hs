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
    fits sides = sum sides <= p
    similar p t = lcm (sum t) p == p
    primitives = filter (similar p) $ takeWhile fits primitiveTriples
    scale t = map (*(p `div` (sum t))) t

primitiveTriples = [triple m n | m<-[1..], n<-(ns m)]
  where
    ns m = [n | n<-[1..(m -1)], odd (m - n), gcd m n == 1]
