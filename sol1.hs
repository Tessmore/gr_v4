module GS

where

-- 1.1

doubleMe x = x + x

doubleSmall x = if x < 10 then x*2 else x-1

-- 1.3

divides d n = rem n d == 0

-- Least divider
ld n = ldf 2 n

-- Least divider function
ldf k n | divides k n = k
        | k^2 >= n    = n
        | otherwise   = ldf (k+1) n


{- 1.4

It does not matter for the final solution, but it does sometimes mean that
another "otherwise" is called

-}

-- 1.5

prime0 n | n < 1  = error "Not a positive integer"
         | n == 1 = False
         | otherwise = ld n == n

{- 1.6

  :t divides 
  divides :: Integral a => a -> a -> Bool

  divides d n = rem n d == 0

  Divides returns a Bool answer, so the reminder check function should do everything before returning a Bool

  :t rem
  rem :: Integral a => a -> a -> a

-}

-- 1.8 Find minimum
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

minCustom :: Int -> Int -> Int
minCustom x y | x <= y = x
              | otherwise = y


-- 1.9 Find maximum from list of integers

max_from_list :: [Int] -> Int
max_from_list [] = error "empty list"
max_from_list [x] = x
max_from_list (x:xs) = max x (max_from_list xs)


-- 1.10 Remove the first element from a list

removeFirstOccurence :: Int -> [Int] -> [Int]
removeFirstOccurence n []  = []
removeFirstOccurence n (h:t) 
  | n == h    = t
  | otherwise = h:(removeFirstOccurence n t)

-- Very similar, used for sorting characters
removeFirstChar n []  = []
removeFirstChar n (h:t) 
  | n == h    = t
  | otherwise = h:(removeFirstChar n t)

-- 1.13 Count occurences of given character in a string

count :: Char -> String -> Int
count c [] = 0
count c (h:t) 
  | c == h    = 1 + count c t
  | otherwise = 0 + count c t

-- 1.14 String conversion : a1a2a3 a1a2a2a3a3a3... etc.

-- TODO : Probably not the best way to do this
blowup :: String -> String
blowup s = reverse (blowupRev (reverse s))
blowupRev [] = "" 
blowupRev (h:t) = take (length t+1)(repeat h) ++ blowupRev t

-- 1.15 Sort a string in lex. order

sortString :: String -> String
sortString []   = ""
sortString list = min:(sortString (removeFirstChar min list)) where min = minimum list

-- 1.16 Prefix example

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

-- 1.17 Find the substring by using `prefix` of 1.6

substring :: String -> String -> Bool
substring xs [] = False
substring (x:xs) (y:ys)
  | prefix (x:xs) (y:ys) = True
  | substring (x:xs) ys = True
  | otherwise = False