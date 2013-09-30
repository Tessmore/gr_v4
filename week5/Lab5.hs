module Lab5

where

import Data.List
import Week5

{- 
  Assignment 1.

  Find a suitable assertion of mergeSrt, and write an assertive version of this.  
-}

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)


{- 
  Assignment 2.

  Implementing merge sort by splitting the list to be sorted in equal parts.

  Also find a suitable assertion of this version + implement it
-}

split :: [a] -> ([a],[a])
split xs = let
    n = (length xs) `div` 2
  in
    (take n xs, drop n xs)