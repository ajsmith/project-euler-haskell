module Main where
import System.Environment (getArgs)
import qualified Problem26

main = do
  args <- getArgs
  solve (read (head args) :: Int) (tail args)
  where
    solve 26 = Problem26.solve
    solve _  = error "Solution not found."
