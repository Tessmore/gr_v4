module GS

where

-- 1.3
divides :: Int -> Int -> Bool
divides d n = rem n d == 0

-- Least divider
ld :: Int -> Int
ld = ldf 2

-- Least divider function
ldf :: Int -> Int -> Int
ldf k n | divides k n = k
        | k^2 >= n    = n
        | otherwise   = ldf (k+1) n

-- 1.5 Simple prime number checking
prime0 :: Int -> Bool
prime0 n | n < 1  = error "Not a positive integer"
         | n == 1 = False
         | otherwise = ld n == n

-- 1.8 Find minimum value in a list
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

-- 1.8b
minCustom :: Int -> Int -> Int
minCustom x y | x <= y = x
              | otherwise = y

-- 1.16 Prefix example
prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

-- 1.24
ldp :: Integer -> Integer
ldp n = ldpf primes1 n
ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n = n
              | otherwise = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]
prime :: Integer -> Bool
prime n | n < 1 = error "not a positive integer"
        | n == 1 = False
        | otherwise = ldp n == n
