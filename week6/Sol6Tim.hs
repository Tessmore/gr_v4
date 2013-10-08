module Sol6Tim

where
import Data.List
import System.Random
import Week6
import Lab6

{-
--ex1
exMTim :: Integer -> Integer -> Integer -> Integer
exMTim  b e m = exMHelper 1 1 m  where 
 exMHelper e' c m = 
                   if e' == e then (c * b) `mod` m
                   else exMHelper (e' + 1) ((c * b) `mod` m) m
                   

exMT2 :: Integer -> Integer -> Integer -> Integer
exMT2 x y n = let 
  squares = squareListMod x y n
  factOf2 = toFactOf2 y 
 in product (zipWith helper squares factOf2) `mod` n where 
  helper x y = if y then x else 1
 
squareListMod :: Integer -> Integer -> Integer -> [Integer]
squareListMod x 0 _ = []
squareListMod x y n = squareList' x 0 y n where
 squareList' x k y n = if (2^k) <= y then (x^(2^k) `mod` n) : (squareList' x (k+1)  y n)
                       else []

toFactOf2 :: Integer -> [Bool]
toFactOf2 x = decToBin' x
 where
decToBin' 0 = []
decToBin' y = let (a,b) = quotRem y 2 in [(b == 1)] ++ decToBin' a

exMT3 :: Integer -> Integer -> Integer -> Integer
exMT3 x y n = let
  factOf2 = toFactOf2 y
 in product (exMT3' factOf2 0 x ) `mod` n where
  exMT3' [] k x = []
  exMT3' (f:fs) k x = if f then ((x^(2^k)) `mod` n) : (exMT3' fs (k+1) x)
                      else exMT3' fs (k+1) x 
-}
--ex3
composites :: [Integer]
composites = compSieve (intsToIntBools [2..])

intsToIntBools :: [Integer] -> [(Integer, Bool)]
intsToIntBools [] = []
intsToIntBools (n : xs) = (n, True) : (intsToIntBools xs)

compSieve :: [(Integer, Bool)] -> [Integer]
--compSieve [] = []
compSieve ((x, False) : xs) = x : (compSieve xs)
compSieve ((x, True) : xs) = compSieve (compMark xs 1 x) 
 where
 compMark :: [(Integer, Bool)] -> Integer -> Integer -> [(Integer, Bool)]
-- compMark [] _ _ = []
 compMark ((y, val):ys) k m | k == m = (y, False) : (compMark ys 1 m)
                          | otherwise = (y, val) : (compMark ys (k+1) m)

--ex4
testF :: Int -> IO Integer
testF k = let
  comps = composites
 in testF' k comps where
  testF' k (c:cs) = do 
                     a <- primeF k c
                     if a then return c
                     else testF' k cs

{- if you keep running this test, eventually even the first number will slip through, so 4 is the smallest composite number that is able to slip through the test. The larger you make K, the smaller the chance that some number will fall through, hence the returned number is larger.
-}

--ex5
testCarmichael :: Int -> IO Integer
testCarmichael k = let
  carm = carmichael
 in testCarmichael' k carm where
  testCarmichael' k (c:cs) = do
                              a <- primeF k c
                              if a then return c
                              else testCarmichael' k cs

--ex6
testMR :: Int -> IO Integer
testMR k = let
  carm = carmichael
 in testMR' k carm where
  testMR' k (c:cs) = do
                      a <- primeMR k c
                      if a then return c
                      else testMR' k cs
