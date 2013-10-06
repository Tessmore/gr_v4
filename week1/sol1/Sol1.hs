{-

Group GR_V_4: 

  Fabiën Tesselaar, 
  Tim Gosen, 
  Lulu Zhang, 
  Tina Churlinovska

-}

module Sol1 where

import GS

-- 1.1 Try out a few calculations using * for multiplication, + for addition, 
-- - for subtraction, ^ for exponentiation, / for division. By playing with the system, 
-- find out what the precedence order is among these operators.
doubleMe :: Int -> Int
doubleMe x = x + x

doubleSmall :: Int -> Int
doubleSmall x = if x < 10 then x*2 else x-1

-- 1.9 Find maximum value from list of integers
maxFromList :: [Int] -> Int
maxFromList [] = error "empty list"
maxFromList [x] = x
maxFromList (x:xs) = max x (maxFromList xs)

-- 1.10 Remove the first element from a list
removeFirstOccurence :: Int -> [Int] -> [Int]
removeFirstOccurence n []  = []
removeFirstOccurence n (h:t) 
  | n == h    = t
  | otherwise = h:removeFirstOccurence n t

-- 1.10b Remove first character (used for sorting strings)
removeFirstChar :: Char -> String -> String
removeFirstChar n []  = []
removeFirstChar n (h:t) 
  | n == h    = t
  | otherwise = h:removeFirstChar n t

-- 1.13 Count occurences of given character in a string
count :: Char -> String -> Int
count c [] = 0
count c (h:t) 
  | c == h    = 1 + count c t
  | otherwise = 0 + count c t

-- 1.14 String conversion : a1a2a3 -> a1a2a2a3a3a3... etc.
blowup :: String -> String
blowup [] = []
blowup (x:xs) = blowupp 1 (x:xs)
blowupp n [] = []
blowupp n (x:xs) = replicate n x ++ blowupp (n + 1) xs

-- 1.14 Alternative version
blowup2 :: String -> String
blowup2 s = reverse (blowup2Rev (reverse s))

-- Helper of the alternative version: Blows up a string of reversed order
blowup2Rev [] = "" 
blowup2Rev (h:t) = replicate (length t+1) h ++ blowup2Rev t

-- 1.15 Sort a string in lex. order
sortString :: String -> String
sortString []   = ""
sortString list = min:sortString (removeFirstChar min list)
                    where min = minimum list

-- 1.17 Find the substring by using `prefix` of 1.6
substring :: String -> String -> Bool
substring xs [] = False
substring (x:xs) (y:ys)
  | prefix (x:xs) (y:ys) = True
  | substring (x:xs) ys = True
  | otherwise = False
-- VVZ: could the above be written with simple logic instead of a multiple dispatch construct? yes, it could!
-- VVZ: substring (x:xs) (y:ys) = prefix (x:xs) (y:ys) || substring (x:xs) ys
  
-- 1.20 Returns a list of the corresponding inner list lengths.
lengths :: [[a]] -> [Int]
lengths = map length

-- 1.21 Sum the list of lists length
sumLengths :: [[a]] -> Int
sumLengths list = sum (lengths list)
-- VVZ: or simply 'sumLengths = sum . lengths'
