module Main where
import System.Environment (getArgs)
import qualified Problem1
import qualified Problem2
import qualified Problem3
import qualified Problem11
import qualified Problem15
import qualified Problem26

main = do
  args <- getArgs
  solution (read (head args) :: Int) (tail args)
  where
    solution problem args = putStrLn $ "Solution: " ++ (show $ solve problem args)

    solve 1  = Problem1.solve
    solve 2  = Problem2.solve
    solve 3  = Problem3.solve
    solve 11 = Problem11.solve
    solve 15 = Problem15.solve
    solve 26 = Problem26.solve
    solve _  = error "Solution not found."
