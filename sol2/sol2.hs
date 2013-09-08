module Sol2 where

import TAMO

-- Exercise 2.13  Implement checks for the principles from Theorem 2.12.
   
logEquiv11 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv11 bf1 bf2 =
            and [(bf1 p ) <=> (bf2 p ) | p <- [True,False]]
		  
test1 = logEquiv11 (\ p -> not True) (\ p -> False)
test11= logEquiv11 (\ p -> not False) (\ p -> True)
test2 = logEquiv11 (\ p -> p ==> False) (\ p -> not p)
test3 = logEquiv11 (\ p -> p || True ) (\ p -> True)
test33 = logEquiv11 (\ p -> p && False) (\ p -> False)
test4 = logEquiv11 (\ p -> p || False) (\ p -> p)
test44 = logEquiv11 (\ p -> p && True) (\ p -> p)
test5 = logEquiv11 (\ p -> p || (not p)) (\ p -> True)
test6 = logEquiv11(\ p -> p && (not p)) (\ p -> False)

--Alternative solution
form11 =not p <=> q
form12=not q <=> p
form13=(p==>q)<=> not p
form14=(p||p)<=> p
form15= p && q <=> q
form16=p||q <=> p
form17=p && q<=> q


-- Exercise 2.15 A propositional contradiction is a formula that yields false for every combination of truth values for its proposition letters. Write Haskell definitions of contradiction tests for propositional functions with one, two and three variables.

contradiction1 :: (Bool -> Bool) -> Bool 
contradiction1 bf = and [not (bf p) | p <- [True, False] ]

contradiction2 :: (Bool -> Bool -> Bool) -> Bool
contradiction2 bf = and [not (bf p q) | p <- [True, False], q <- [True, False]]

contradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool 
contradiction3 bf = and [not (bf p q r) | p <- [True, False], q <- [True, False], r <-[True, False]]

contradict_test1a = contradiction1 (\ p -> not (not p))
contradict_test2a = contradiction2 (\ p q -> p ==> q)
contradict_test3a = contradiction3 (\ p q r -> p && (q && r))

formTest1 p = p && not p
formTest2 p = p || not p
formTest3 p q = (p && not p) && (q || not q)
formTest4 p q = (p || not p) || (q || not q)
formTest5 p q r = (p && not p) && (q || not q) && (r && not r)
formTest6 p q r = (p || not p) || (q || not q) && (r || not r)

-- 2.20

formula1_1 p q = not p ==> q 
formula1_2 p q = p ==> not q
formula2_1 p q = not p ==> q
formula2_2 p q = q ==> not p
formula3_1 p q = not p ==> q
formula3_2 p q = not q ==> p
formula4_1 p q r = p ==> (q ==> r)
formula4_2 p q r = q ==> (p ==> r)
formula5_1 p q r = p ==> (q ==> r)
formula5_2 p q r = (p ==> q) ==> r
formula6_1 p q = (p ==> q) ==> p
formula6_2 p q = p
formula7_1 p q r = p || q ==> r
formula7_2 p q r = (p ==> r) && (q ==> r)

test_formula1 = logEquiv2 formula1_1 formula1_2
test_formula2 = logEquiv2 formula2_1 formula2_2
test_formula3 = logEquiv2 formula3_1 formula3_2
test_formula4 = logEquiv3 formula4_1 formula4_2
test_formula5 = logEquiv3 formula5_1 formula5_2
test_formula6 = logEquiv2 formula6_1 formula6_2
test_formula7 = logEquiv3 formula7_1 formula7_2

--Exercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

-- Exercise 2.52 Define a function parity :: [Bool] -> Bool that gives True for parity xs just in case an even number of the xss equals True.
parity :: [Bool] -> Bool
parity xs = length (filter (\b -> b) xs) `rem` 2 == 0

-- Exercise 2.53 Define a function evenNR :: (a -> Bool) -> [a] -> Bool that gives True for evenNR p xs just in case an even number of the xss have property p. (Use the parity function from the previous exercise.)

evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)
