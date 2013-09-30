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

-- Hlint suggestion :
mergeSort :: Ord a => [a] -> [a]
mergeSort = foldr (\ x -> merge [x]) []

-- TODO fix a Prop, fix an assert something
--mergeSortProp :: Ord a => [a] -> [a] -> Bool
--mergeSortProp xs 

--mergeSrtA :: Ord a => [a] -> [a]
--mergeSrtA = assert? mergeSortProp $ assert? someOtherProp mergeSrt

{- 
  Assignment 2.

  Implementing merge sort by splitting the list to be sorted in equal parts.
  Also implement a suitable assertion of this version
-}

split :: [a] -> ([a],[a])
split xs = let
    n = length xs `div` 2
  in
    (take n xs, drop n xs)
    
mergeSrtSplit []  = []
mergeSrtSplit [x] = [x]
mergeSrtSplit xs  = merge (mergeSrt ys) (mergeSrt zs)
                      where (ys, zs) = split xs