module Main where
import System.Environment (getArgs)
import Problem26

main = do
  args <- getArgs
  putStrLn $ show $ findLongestCycle
