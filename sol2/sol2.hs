module Sol2 where

import TAMO

-- Exercise 2.13  Implement checks for the principles from Theorem 2.12.

(==>) :: Bool -> Bool -> Bool 
True ==> x = x
False ==> x = True 
   
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


-- Exercise 2.52 Define a function parity :: [Bool] -> Bool that gives True for parity xs just in case an even number of the xss equals True.

parity :: [Bool] -> Bool
parity xs = length (filter (\b -> b) xs) `rem` 2 == 0

-- Exercise 2.53 Define a function evenNR :: (a -> Bool) -> [a] -> Bool that gives True for evenNR p xs just in case an even number of the xss have property p. (Use the parity function from the previous exercise.)

evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)
