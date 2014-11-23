module Main where
import System.Environment (getArgs)
import Problem0026

main = do
  args <- getArgs
  let d = read (head args) :: Integer
    in putStrLn $ show $ cycleLength (1 / d)

-- putStrLn $ show $ primeDivisors (read n :: Integer)
