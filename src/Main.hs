module Main where
import System.Environment (getArgs)
import Problem0026

main = do
  args <- getArgs
  putStrLn $ show $ findLongestCycle

-- putStrLn $ show $ primeDivisors (read n :: Integer)
