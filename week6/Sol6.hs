module Sol6

where
import Data.List
import System.Random

{-
Assignment 1:
 Time spent: 45 min

 Implement a function that does modular exponentiation of x^y in polynomial time
-}

-- Dividing y : because 32 -> 16
-- quot  = divide but do not care about the remainder so it works with dividing uneven numbers fine
exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 1 = 0
exM _ 0 n = 1
exM x 1 n = rem x n
exM x y n = let k = exM (x*x) (quot y 2) n in
  if even y
    then k
    else rem (k * x) n
    