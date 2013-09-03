module TAMO

where


{- 2.2

P Q  P excl. or Q 

t t       f
t f       t
f t       t
f f       f

-}

{- 2.4

P Q  not (P <=> Q) 

t t      not t = f
t f      not f = t
f t      not f = t
f f      not t = f

-}

infix 1 ==>
infix 1 <=>
infixr 2 <+>

-- Implication
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

-- Equivalence
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

-- Exclusive OR
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

p = True
q = False
formula1 = (not p) && (p ==> q) <=> not (q && (not p))

-- 2.5 Example
valid1 :: (Bool -> Bool) -> Bool
valid1 bf = (bf True) && (bf False)

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf =    (bf True True)
            && (bf True False)
            && (bf False True)
            && (bf False False)

excluded_middle :: Bool -> Bool
excluded_middle p = p || not p

form1 p q = p ==> (q ==> p)
form2 p q = (p ==> q) ==> p


{- 2.9

P Q  P excl. or Q   (P excl. or Q) exlc. or Q

t t       f              t f  = f
t f       t              f t  = t
f t       t              t t  = t
f f       f              f f  = f

-}


logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 = (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and [(bf1 p q) <=> (bf2 p q) | p <- [True,False], q <- [True,False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = and [(bf1 p q r) <=> (bf2 p q r) | p <- [True,False], q <- [True,False], r <- [True,False]]

formula3 p q = p
formula4 p q = (p <+> q) <+> q
formula5 p q = p <=> ((p <+> q) <+> q)



-- 2.12

test1 = True == not False || not False == True

-- test2 = logEquiv1 id (\ (not p) -> (not p))

--test2 = logEquiv1 id (\ p -> False (==>) not p)
