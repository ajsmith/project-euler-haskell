{- Problem 18: Maximum path sum I

By starting at the top of the triangle below and moving to adjacent numbers on
the row below, the maximum total from top to bottom is 23.

       3
      7 4
     2 4 6
    8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

                  75
                 95 64
                17 47 82
               18 35 87 10
              20 04 82 47 65
             19 01 23 75 03 34
            88 02 77 73 07 63 67
           99 65 04 28 06 16 70 92
          41 41 26 56 83 40 80 70 33
         41 48 72 33 47 32 37 16 94 29
        53 71 44 65 25 43 91 52 97 51 14
       70 11 33 28 77 73 17 78 39 68 17 57
      91 71 52 38 17 14 91 43 58 50 27 29 48
     63 66 04 68 89 53 67 30 73 16 69 87 40 31
    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23


NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)

-}

module Problem18 where
import Data.Tree (Tree(..), Forest, drawTree, rootLabel, subForest, levels, unfoldTree)

input ="\
\75\n\
\95 64\n\
\17 47 82\n\
\18 35 87 10\n\
\20 04 82 47 65\n\
\19 01 23 75 03 34\n\
\88 02 77 73 07 63 67\n\
\99 65 04 28 06 16 70 92\n\
\41 41 26 56 83 40 80 70 33\n\
\41 48 72 33 47 32 37 16 94 29\n\
\53 71 44 65 25 43 91 52 97 51 14\n\
\70 11 33 28 77 73 17 78 39 68 17 57\n\
\91 71 52 38 17 14 91 43 58 50 27 29 48\n\
\63 66 04 68 89 53 67 30 73 16 69 87 40 31\n\
\04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

input' = "\
\ 3 \n\
\ 7 4 \n\
\ 2 4 6 \n\
\ 8 5 9 3"

parse source = map parseLine $ lines source
  where
    parseToken = read :: (Integral a, Read a) => String -> a
    parseLine = (map parseToken) . words

generateNodeTuples [] = []
generateNodeTuples (lst:[]) = []
generateNodeTuples (lst1:lst2:lsts) =
  (generateNodeTuples' lst1 lst2):(generateNodeTuples (lst2:lsts))
  where
    generateNodeTuples' [] _ = []
    generateNodeTuples' (a:as) (b1:b2:bs) = (a, b1, b2):(generateNodeTuples' as (b2:bs))

nodeTuples = reverse $ generateNodeTuples $ parse input

nodes = map (map buildTreeNode) nodeTuples

linkTreeLevel _ [] = []
linkTreeLevel (c1:c2:cs) (p:ps) = (joinTrees p c1 c2):(linkTreeLevel (c2:cs) ps)

linkTree (l1:[]) = l1
linkTree (l1:l2:ls) = linkTree ((linkTreeLevel l1 l2):ls)

buildTreeNode (r, a, b) = Node r [Node a [], Node b []]

joinTrees' (Node r _) (Node c1 ts1) (Node c2 ts2)
  | r == c1 && r == c2 = Node r [Node c1 ts1, Node c2 ts2]
  | otherwise = error "Mismatched trees"

joinTrees (Node r _) (Node c1 ts1) (Node c2 ts2) = Node r [Node c1 ts1, Node c2 ts2]

height (Node r []) = 1
height (Node r [t1, t2]) = 1 + (max (height t1) (height t2))

maxTreeSum (Node r []) = r
maxTreeSum (Node r [c1]) = r + (maxTreeSum c1)
maxTreeSum (Node r [c1, c2]) = r + (max (maxTreeSum c1) (maxTreeSum c2))

solve = maxTreeSum $ head $ linkTree nodes
