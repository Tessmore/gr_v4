module GS

where

-- 1.1
doubleMe :: Int -> Int
doubleMe x = x + x

doubleSmall :: Int -> Int
doubleSmall x = if x < 10 then x*2 else x-1

-- 1.3
divides :: Int -> Int -> Bool
divides d n = rem n d == 0

-- Least divider
ld :: Int -> Int
ld n = ldf 2 n

-- Least divider function
ldf :: Int -> Int -> Int
ldf k n | divides k n = k
        | k^2 >= n    = n
        | otherwise   = ldf (k+1) n


{- 1.4

It does not matter for the final solution, but it does sometimes mean that
another "otherwise" is called

-}

-- 1.5 Simple prime number checking
prime0 :: Int -> Bool
prime0 n | n < 1  = error "Not a positive integer"
         | n == 1 = False
         | otherwise = ld n == n

{- 1.6

  divides :: Int -> Int -> Bool

  Divides returns a Bool answer, so the reminder check function should do 
  everything before returning a Bool, which is the Int -> Int part

  rem :: Int -> Int

-}

-- 1.8 Find minimum value in a list
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

-- 1.8b
minCustom :: Int -> Int -> Int
minCustom x y | x <= y = x
              | otherwise = y


-- 1.9 Find maximum value from list of integers
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

-- 1.10b Remove first character (used for sorting strings)
removeFirstChar :: Char -> String -> String
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
blowup :: String -> String
blowup s = reverse (blowupRev (reverse s))

-- 1.14b Blows up a string of reversed order
blowupRev [] = "" 
blowupRev (h:t) = take (length t+1)(repeat h) ++ blowupRev t

-- 1.15 Sort a string in lex. order
sortString :: String -> String
sortString []   = ""
sortString list = min:(sortString (removeFirstChar min list)) 
                    where min = minimum list

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
  
-- 1.20 Returns a list of the corresponding inner list lengths.
lengths :: [[a]] -> [Int]
lengths list = map (length) list

-- 1.21 Sum the list of lists length
sumLengths :: [[a]] -> Int
sumLengths list = sum (lengths list)

-- 1.24
-- You re-reference ldf to use the new created primes1?