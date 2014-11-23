module Main where
import System.Environment (getArgs)
import Problem0026

main = do
  (n:[]) <- getArgs
  putStrLn $ show $ primeDivisors (read n :: Integer)
