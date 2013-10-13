module Sol6

where
import Data.List
import System.Random
import Week6
{-
Assignment 1:
 Time spent: 45 min

 Implement a function that does modular exponentiation of x^y in polynomial time
-}

-- Dividing y : because 32 -> 16
-- quot  = divide but do not care about the remainder so it works with dividing uneven numbers fine
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 1 = 0
exM' _ 0 n = 1
exM' x 1 n = rem x n
exM' x y n = let k = exM' (x*x) (quot y 2) n in
  if even y
    then k
    else rem (k * x) n



--Exercise 3
--Solution 1
{-
 First, mark all the numbers from [2..] as True, which means primes
 Second, if one number can be divided, then mark it as false, which means composites
 Then, filter all the numbers which are marked as false and get the numbers
-}
composites :: [Integer]

composites = map fst (filter (not.snd) sieveC)

sieveC :: [(Integer,Bool)] 
sieveC = sieveCom [(x, True) | x <- [2..]]

sieveCom :: [(Integer,Bool)] -> [(Integer,Bool)]
sieveCom ((x,y):ns) = (x, y): (sieveCom ns') 
					where ns' =
						if y
							then map (\ (x',y') -> 
								if rem x' x == 0
									then (x', False)
								else (x', y')) ns
						else ns


-- Solution 2
{-
 (another method but no sieve)
 check whether the first factor of one number is itself or not:
 if it is, this number is a prime, if not, it's a composite
-}
composites' :: [Integer]
composites' = checkCom [4..]

checkCom :: [Integer] -> [Integer]
checkCom (x : xs) = if head (factors x) == x
						then checkCom xs
					else (x: checkCom xs)
					
