Lattice Paths

Starting in the top left corner of a 2×2 grid, and only being able to move to
the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?

> module Problem15 where
> import NumberTheory (nCr)

> solve _ = putStrLn $ show $ nCr 40 20
