module Sol6

where
import Data.List
import System.Random


exM :: Integer -> Integer -> Integer -> Integer
exM x y n = if rem y 2 /= 0
	then (exMod x (y-1) n) * (rem x n)
	else exMod x y n

exMod :: Integer -> Integer -> Integer -> Integer
exMod x 1 n = rem x n
exMod x y n = exMod (x^2) (quot y 2) n 
