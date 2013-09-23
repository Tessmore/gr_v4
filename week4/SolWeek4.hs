module SolWeek4

where

import Week4
import SetOrd


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
  | otherwise = setIntersection (Set xs) (Set y)

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
  
type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a

r @@ s =
  nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]


--trClos :: Ord a => Rel a -> Rel a

-}