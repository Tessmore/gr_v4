module Lab5

where

import Data.List
import Week4 
import Week5_NRC

{- 
  Assignment 1.
  #Time spent: 3 hours

  Find a suitable assertion of mergeSrt, and write an assertive version of this.  
-}

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

-- Short test
mergeSrtTest1 = mergeSrt "asdfsakdfjsakdfjskdfjsdf" 
mergeSrtTest2 = mergeSrt [9,4,7,2,9,4,2,4,8,6,3,1,0,7,2,3,3,2] 

{- 
  Assignment 1: Assertive function
    We want mergeSrt to create a sorted list as a result. This means the output
    condition should be a sorted list. mergeSrt has one parameter as input, 
    one for output. Thus we can use "post1" with "sorted" condition to assert
    mergeSrt into returning a sorted list (or giving a post1 error condition if
    inpossible).
-}
mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = post1 sorted mergeSrt
-- VVZ: very good, very lazy solution!

{- 
  Assignment 2.
  #Time spent: 1 hour
  
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
mergeSrtSplit xs  = merge (mergeSrtSplit ys) (mergeSrtSplit zs)
                      where (ys, zs) = split xs
                      
{-
  Assignment 2: Assertive function
    Basically, this is exactly the same as Assignment 1. We want a post condition
    of a sorted list, but this time using mergeSrtSplit
-}

mergeSrtSplitA :: Ord a => [a] -> [a]
mergeSrtSplitA = post1 sorted mergeSrtSplit
