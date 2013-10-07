module Sol6

where
import Data.List
import System.Random

{-
Assignment 1:
 Time spent: 45 min

 Implement a function that does modular exponentiation of x^y in polynomial time
-}

exM :: Integer -> Integer -> Integer -> Integer
exM x 1 n = rem x n
exM x y n = if rem y 2 /= 0
	then (exMod x (y-1) n) * (rem x n)
	else exMod x y n

exMod :: Integer -> Integer -> Integer -> Integer
exMod x y n = exM (x^2) (quot y 2) n 
