module Main where
import System.Environment (getArgs)
import qualified Problem1
import qualified Problem2
import qualified Problem3
import qualified Problem4
import qualified Problem5
import qualified Problem11
import qualified Problem15
import qualified Problem18
import qualified Problem19
import qualified Problem23
import qualified Problem26
import qualified Problem28
import qualified Problem29
import qualified Problem35
import qualified Problem37
import qualified Problem38
import qualified Problem39
import qualified Problem40
import qualified Problem41
import qualified Problem48
import qualified Problem55
import qualified Problem75
import qualified Problem92
import qualified Problem97

main = do
  args <- getArgs
  solution (read (head args) :: Int)
  where
    solution problem = putStrLn $ "Solution: " ++ (show $ solve problem)

    solve 1  = Problem1.solve
    solve 2  = Problem2.solve
    solve 3  = Problem3.solve
    solve 4  = Problem4.solve
    solve 5  = Problem5.solve
    solve 11 = Problem11.solve
    solve 15 = Problem15.solve
    solve 18 = Problem18.solve
    solve 19 = Problem19.solve
    solve 23 = Problem23.solve
    solve 26 = Problem26.solve
    solve 28 = Problem28.solve
    solve 29 = Problem29.solve
    solve 35 = Problem35.solve
    solve 37 = Problem37.solve
    solve 38 = Problem38.solve
    solve 39 = Problem39.solve
    solve 40 = Problem40.solve
    solve 41 = Problem41.solve
    solve 48 = Problem48.solve
    solve 55 = Problem55.solve
    solve 75 = Problem75.solve
    solve 92 = Problem92.solve
    solve 97 = Problem97.solve
    solve _  = error "Solution not found."
