{-

Group GR_V_4: 

  Fabiën Tesselaar, 
  Tim Gosen, 
  Lulu Zhang, 
  Tina Churlinovska

-}

module SolWeek2 where

import Week2

{-
 - Exercise 2.1
 -}


data Shape = NoTriangle | Equilateral
            | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | not ((x + y > z) && (x + z > y) && (y + z > x)) 		= NoTriangle
	       | x == y && x == z 						= Equilateral
	       | (x == y) || (y == z) || (z == x) 				= Isosceles
	       | (x^2 + y^2 == z^2) || (x^2 + z^2 == y^2) || (y^2 + z^2 == x^2) = Rectangular
	       | otherwise							= Other

{-
 - Time spend on implementation 15 minutes
 -}

{-
 - Exercise 2.2
 -}

contradiction :: Form -> Bool
contradiction f = not (all (\ v -> eval v f) (allVals f)) 

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)
-- -- logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

{-
 - Time spend on implementation now 45 minutes 
 -}

{-
 - Exercise 2.3
 -}
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (f:fs)) = dist (cnf f) (Cnj (map cnf fs) )

dist :: Form -> Form -> Form
dist (Cnj x) y = Cnj [(dist (head x) y), (dist (Cnj (tail x)) y)]
dist x (Cnj (y)) = Cnj [dist x (head y), dist x (Cnj (tail y))]
dist x y = Dsj [x, y]

--dist x y | Dsj (f:fs) f2	= Cnj [dist (f, f2), dist (fs, f2)]
--	 | f2 Dsj (f:fs) 	= Cnj [dist (f2, f), dist (f2, fs)]
--	 | otherwise		= Dsj [x,y]

 
