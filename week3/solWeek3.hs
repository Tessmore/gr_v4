{-
Week 3 assignment

Group GR_V_4: 

  Fabien Tesselaar, 
  Tim Gosen, 
  Lulu Zhang, 
  Tina Churlinovska

-}

module SolWeek3 where

import Data.List
import Week2
import Week3
import Techniques
import System.Random
import Control.Monad

-- Assignment 3
-- IO is a side-effect
-- IO list -> list
-- Generate random integer lists
genIntList :: IO [Int]
genIntList = liftM (randomRs (0, 10000)) newStdGen

{- Assignment 4
  Time spent 2 minutes

  + Permutation should be of same length
  + Lists sorted should be equal for them to be permutation of each other
-}
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation a b = length a == length b &&
                    sort a == sort b
                    


{- Assignment 4b
  Alternative version, conform to the assignment request input type, but a little
  cheating

  Time spent 1 minute
-}      
isPermutationCheat :: Eq a => [a] -> [a] -> Bool
isPermutationCheat a b = elem a (permutations b)

{- Assignment 4c
  Alternative version, conform to the assignment request input type, but
  slow / recursive

  Time spent 8 minutes
-}
isPermutationSlow :: Eq a => [a] -> [a] -> Bool
isPermutationSLow [] [] = True
isPermutationSLow _  [] = False
isPermutationSlow (h:t) b
  | (elem h) b = isPermutationSlow t (delete h b)
  | otherwise  = False
  
  
  
{- Exercise 7 -}
cnfTest i = testForms i (\form -> equiv form (cnf form))
