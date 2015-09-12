module Main where
import System.Environment (getArgs)
import qualified Problem15
import qualified Problem26

main = do
  args <- getArgs
  solution (read (head args) :: Int) (tail args)
  where
    solution problem args = putStrLn $ "Solution: " ++ (show $ solve problem args)

    solve 15 = Problem15.solve
    solve 26 = Problem26.solve
    solve _  = error "Solution not found."
