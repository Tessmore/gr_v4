module SolWeek4

where

import Data.List
import Techniques
import Week4
import SetOrd

{-
  Assignment 2
  
   Implement a random data generator for the datatype Set Int
  
  Indication time spent: 45min
-}
getSetInt :: IO (Set Int)
getSetInt = do 
				d <- getRandomInt 10
				n <- getRandomInt 10
				getSetI d n

getSetI :: Int -> Int -> IO (Set Int)
getSetI _ 0 = return (Set [])
getSetI d n = do 
			f <- getRandomInt d
			Set fs <- getSetI d (n-1)
			return (Set (f:fs))
			
			
{-
  Assignment 3
  
  Implement Intersection, union and difference on Sets
  
  Indication time spent: 3 hours
-}

-- Intersection : All elements of A that are also in B
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set []) _  = (Set [])
setIntersection  _ (Set []) = (Set [])
setIntersection (Set (x:xs)) (Set y)
  | inSet x (Set y) = insertSet x (setIntersection (Set xs) (Set y))
  | otherwise       = setIntersection (Set xs) (Set y)

-- Union : All distinct elements of A and B
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set []) (Set x)  = (Set x)
setUnion (Set  y) (Set []) = (Set y)
setUnion (Set (x:xs)) (Set y)
  | inSet x (Set y) = setUnion (Set xs) (Set y)
  | otherwise       = insertSet x (setUnion (Set xs) (Set y))
  
-- Difference : Everything that is in A, but not in B
setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set []) (Set x)  = (Set [])
setDifference (Set  y) (Set []) = (Set y)
setDifference (Set (x:xs)) (Set y)
  | inSet x (Set y) = deleteSet x (setDifference (Set xs) (Set y))
  | otherwise       = insertSet x (setDifference (Set xs) (Set y))

{- 
  Assignment 4
  
  Create transitive closusre of a list of pairs
-}

type Rel a = [(a,a)]

infixr 5 @@

-- Finds the transitive pair needed
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

{-
  Take the first item, find the required closure for all other elements in the
  list. Continue with the next element in list to find combinations with 
  next nodes (no need to look at the previous node, as it is already done by
  looking forward with the earlier check)
  
  "nub" is not actually needed, but just in case duplicate pairs are provided
  
  Reference: 
    http://en.wikipedia.org/wiki/File:Transitive-closure.svg
-}
trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos (x:xs) = nub (x : ([x] @@ xs) ++ (trClos xs))

{- 
  Assignment 5
  
  Properties to test
  
  Need an isClosure function (i.e transivity check or someth)

  * Test with some known sets (just to start)
  
  * Result set should be transitive
  * If already transitive, shouldn't be expanded
  * All S = sub-elements/lists, if S is transitive, then Result set should 
    be smallest possible set
  
-}

testTrClos1 = trClos [(1,1), (1,2), (1,3), (2,3), (3,1)] == [(1,1), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2)]
