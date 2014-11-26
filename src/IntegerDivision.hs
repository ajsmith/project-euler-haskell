module IntegerDivision where


-- Convert an integer into a list of digits
digits :: Integer -> [Integer]
digits n = take (digitCount $ fromIntegral n) (digitStream n)

-- Convert an integer into a stream of digits
digitStream :: Integer -> [Integer]
digitStream n = digitStream' (show n)
  where
    digitStream' "" = 0:(digitStream' "")
    digitStream' (d:ds) = (read [d] :: (Read a, Integral a) => a):(digitStream' ds)

-- Count the digits in an integer
digitCount :: Integral a => a -> Int
digitCount n =
  if (n == 10^(p - 1))
  then p
  else (p - 1)
  where
    p = ceiling (log10 (n * 10))
    log10 x = (log (fromIntegral x)) / (log 10)

-- (still buggy) implementation of long division as an infinite stream of integers
longDivide (x:[]) y
  | x `mod` y == 0 = [x `div` y]
  | x > y = (x `quot` y):(longDivide [x `rem` y] y)
  | x < y = longDivide [x * 10] y
longDivide (x:xs) y
  | x `mod` y == 0 = (x `div` y):(longDivide xs y)
  | x > y = (x `quot` y):(longDivide ((x `rem` y):xs) y)
  | x < y = longDivide ((x * 10 + (head xs)):(tail xs)) y

-- infinite stream of zeroes
zeroes = 0:zeroes

-- borrowing
borrow y (x:[]) =
  if y <= x
  then (x:[])
  else borrow y (x:[0])
borrow y (x1:x2:xs) =
  if y <= x1
  then (x1:x2:xs)
  else borrow y ((10 * x1 + x2):xs)
