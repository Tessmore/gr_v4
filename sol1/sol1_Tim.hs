module Sol1

where
import GS

{-Ex 1.1
Precedence
Exponentiation

Prelude> 2^2*2
 8
 So exponentiation precedes multiplication otherwise the answer would be 16

 Prelude> 2^4/4
 16.0
 So exponentiation precedes division otherwise the answer would be 1

 Division and Multiplication

 Prelude> 3*4/6
 2.0

 Prelude> 6/2*3
 9.0
 So multiplication and division are equal otherwise 3*4/6 would be 2.0 and 6/2*3 would be 1.

 Prelude> 2+4*2
 10

 Addition and substraction

 Meaning that Division and Multiplication are equal and so are Addition and substraction. So without the use of brackets the precedence is from left to right.
-}

{-
 Ex 1.3
 It's already in GS.hs
-}


{-
 Ex 1.4
 It wouldn't make a difference since the statement k^2>=n will only return true for powers of 2 where k^2>n wouldn't. But the statement divides k n had already returned true for that case since every power of 2 is also divisible by 2.
-}

{-
 Ex 1.5 
 They are already in GS.hs therefore we don't need to add them.
-}

{-
 Ex 1.6
  the type declaration for rem would look like this
rem :: Integer -> Integer -> Integer
  though :t rem in ghci yields the following
rem :: Integral a => a -> a -> a
  which is about the same but less restrictive, most probably there are more types of integers.
-}

{-
 Ex 1.7
 Since divides has the signature Integer -> Integer -> Bool and the first Integer is already given, we can evaluate this part of the fuction Integer and we are waiting for another Integer before we can evaluate to a type Bool. Therefore the signature of divides 5 is Integer -> Bool
 The same goes if the second Integer is also given. Then the funtion can evaluate to a Bool, therefore the type of divides 5 7 is Bool.
-}

--Ex 1.9
maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

--Ex 1.10
removeFst :: Int -> [Int] -> [Int]
removeFst n [] = []
removeFst n (x:xs) | n == x = xs
                   | otherwise = (x:removeFst n xs)

--Ex 1.11 Already in GS.hs

--Ex 1.12 Already in Gs.hs

--Ex 1.13
count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | c == x    = 1 + (count c xs)
	       | otherwise = count c xs

--Ex 1.14
blowup :: String -> String
blowup [] = []
blowup (x:xs) = let
		   blowuphelper n []     = []
		   blowuphelper n (x:xs) = (replicate (n+1) x) ++ (blowuphelper (n+1) xs)
		in [x] ++ blowuphelper 1 (xs)

--Ex 1.15
removeFstStr :: String -> [String] -> [String]
removeFstStr n [] = []
removeFstStr n (x:xs) | n == x = xs
		      | otherwise = (x:removeFstStr n xs)

minStr :: String -> String -> String
minStr a b | a < b     = a
	   | otherwise = b

mnmStr :: [String] -> String
mnmStr [] = error "empty list" 
mnmStr [x] = x
mnmStr (x:xs) =  minStr x (mnmStr xs) 

srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : (srtString (removeFstStr m xs)) where m = mnmStr xs

--Ex 1.16 Already in GS.hs

--Ex 1.17
substring :: String -> String -> Bool
substring [] [] = True
substring xs [] = False
substring xs ys | prefix xs ys = True
		| otherwise =let 
				substringhelper xs [] = False
				substringhelper xs (y:ys) = substring xs ys
	  		     in substringhelper xs ys 

{-
 Ex 1.18
  :t srtString ["bla", "bla"]
  :t (True, mnmStr ["bla"])
  :t [(False,mnmStr ["boe"] ),(True, mnmStr ["bla"])
  :t ([True, False], mnmStr ["bla"])
  :t not
-}


{-
 Ex 1.19
  head :: [a] -> a
  last :: [a] -> a
  init :: [a] -> [a]
  fst :: (a, b) -> a
  (++) :: [a] -> [a] -> [a]
  flip :: (a -> b -> c) -> b -> a -> c
  flip (++) :: [a] -> [a] -> [a]

  head take the first element from an array just as last takes the last
  head [1,2,3] = 1
  last [1,2,3] = 3

  init take every element but the last
  init [1,2,3] = [1,2]

  fst takes the first element from a tuple
  fst (1,2) = 1
 
  (++) is the concatenation operator made prefix
  (++) [1,2] [3,4] = [1,2,3,4] 

  flip take a function that takes two arguments and flips the order in which the arguments are supplied and then return the result of the function with the arguments in the original order.
  flip take [1,2,3,4,5] 3 = [1,2,3]

  flip (++) therefore concatenates the second argument with the first
  flip (++) [3,4] [1,2] = [1,2,3,4]
-} 

--Ex 1.20
lengths :: [[a]] -> [Int]
lengths [] = error "empty list"
lengths xs = map length xs

--Ex 1.21
sumLengths :: [[a]] -> Int
sumLengths [] = 0
sumLengths xs = sum (map length xs)

--Ex 1.22 Already in GS.hs

--Ex 1.23 Already in GS.hs

{-
 Ex 1.24
 The are multiple declaration of the function ldp therefore it will return an error. There can be no ambiguity.
-}
