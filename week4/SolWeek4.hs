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
  return (Set (map head (group (sort(f:fs)))))

-- Changed this to (Set (map head (group(sort(f:fs))))) 
-- instead of (Set (f:fs)) because sets need to be sorted and without duplicates


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
  
  Time spent 2 hours
-}

type Rel a = [(a,a)]

infixr 5 @@

-- Finds the transitive pair needed
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

{-
  One time to go through all the elements in the list.
  Second time to include the additional closures that are not directly linked
  
  Take the first item, find the required closure for all other elements in the
  list. Continue with the next element in list to find combinations with 
  next nodes (no need to look at the previous node, as it is already done by
  looking forward with the earlier check)
  
  Then it needs to do it one more time to get all the 
  
  Reference: 
    http://en.wikipedia.org/wiki/File:Transitive-closure.svg
-}

trClos :: Ord a => Rel a -> Rel a
trClos r = nub (trClosHelper (trClosHelper r))

trClosHelper :: Ord a => Rel a -> Rel a
trClosHelper [] = []
trClosHelper (x:xs) = x : ([x] @@ xs) ++ trClos xs

{-
  Another answer:
-}

trClos':: Ord a => Rel a -> Rel a
trClos' r = let res = sort $ nub $ (r ++ (r @@ r)) 
				in
					if res == r
						then r
					else trClos' res

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

-- Haskell has built in isTransitive
-- isTransitive r = and [((x `po` y) && (y `po` z)) `implies` (x `po` z) | x <- set, y <- set, z <- set]

  
--testTrClos1 = isTransitive (trClos [(1,1), (1,2), (1,3), (2,3), (3,1)])
testTrClos2 = trClos [(1,1), (1,2), (1,3), (2,3), (3,1)] == [(1,1), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2)]
