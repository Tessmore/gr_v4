module Sol2 where

import TAMO

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