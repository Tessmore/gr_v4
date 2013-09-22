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
import System.Random
import Control.Monad
import SolWeek2
import Week2
import Week3
import Techniques

{-
  Assignment 3
  
  Indication time spent: 2 hours
  - 1 hour trying to understand IO
  - 20 minutes looking at alternatives
  - 40 minutes getting some IO to work

  Our first approach was to use the IO for generating a random number. The idea 
  was to create a list of [1, 1, .. , 1] etc. and using map to make every item 
  in the list a random number. However, the following: 

    genIntList n = map (* randomIO) (replicate n 1)
    
  This will not work as randomIO doesn't actually return a number, but an IO number.
  So it remains in some computation stage.

  The alternative to build an infinite list is using the built in "randomRs".
  which generates an infinite list of random numbers between two boundaries. This 
  function internally resolves the IO part, so it does not return IO [Int], but
  just an [Int]

  It uses a fixed seed (1337), so it can be used for testing. I.e if you want 
  to check why some test failed it can only be done if you know which numbers
  actually failed the test.
-}
genIntListNonIO :: [Int]
genIntListNonIO = randomRs(0, maxBound) (mkStdGen 1337) :: [Int]

-- Helper function to return a Finite list
genIntListFinite :: Int -> [Int]
genIntListFinite n = take n genIntListNonIO

{-
  What comes closest to an IO [Int] function, is a list of random numbers of known
  length. We can imperatively add one at a time, untill the list is of requested
  length. So it is basically more a work-around to not use IO.
-}
genIntListIO :: Int -> IO [Int]
genIntListIO n
  | n > 0 = do 
              head <- getRandomInt maxBound
              tail <- genIntListIO (n-1)
              return (head : tail)
  | otherwise = return []

  
{- 
  Assignment 4 : Permuations
  Time spent 2 minutes

  + Permutation should be of same length
  + Lists sorted should be equal for them to be permutation of each other
-}
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation a b = length a == length b &&
                    sort a == sort b

{- 
  Assignment 4b
  Time spent 1 minute

  Alternative version, conform to the assignment request input type, but a little
  cheating
-}
isPermutationCheat :: Eq a => [a] -> [a] -> Bool
isPermutationCheat a b = a `elem` permutations b

{- 
  Assignment 4c
  Time spent 8 minutes

  Alternative version, conform to the assignment request input type, but
  slow / recursive
-}
isPermutationSlow :: Eq a => [a] -> [a] -> Bool
isPermutationSLow [] [] = True
isPermutationSLow _  [] = False
isPermutationSlow (h:t) b
  | h `elem` b = isPermutationSlow t (delete h b)
  | otherwise  = False

{- 
  Assignment 5
  Time spent 15 minutes

  Define some testable properties for this function, and use your random
  generator for integer lists from Exercise 3 to test isPermutation.

  Properties: 
    Transitivity,
    Reflictivity,
    Symmetry
  
  Facts for lists to be a permutation of eachother:
    - A list is always permutation of itself
    - The length must be equal
    - Must contain all the same items (i.e if an item of set A is not in set B
      it cannot be a permutation)
    
-}

{- 
  Helper function for testing the permutations
  It generates a list of random length between 0 and 6
-}
testPermutationGen = do
                      length <- getRandomInt 6
                      genIntListIO length

-- A list should always be a permutation of itself
testPermutation1 = do
                    m <- testPermutationGen
                    return (isPermutation m m)

-- Different lengths cannot be a permutation
testPermutation2 = do
                    m <- genIntListIO 4
                    n <- genIntListIO 5
                    return (isPermutation m n == False)

-- Check for random lists
testPermutation3 :: IO ()
testPermutation3 =  do 
                     m <- testPermutationGen
                     n <- testPermutationGen
                     if isPermutation m n
                        then print (show m ++ " is a permutation of " ++ show n)
                     else error (show m ++ " is not a permutation of " ++ show n)

{- 
  Assignment 6

  Use the random formula generator from the Techniques slides to test your 
  CNF program of last week.

  * report_assignment6.pdf
-}

cnfTest i = testForms i (\form -> equiv form (cnf form))