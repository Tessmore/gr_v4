{-

Group GR_V_4: 

  Fabien Tesselaar, 
  Tim Gosen, 
  Lulu Zhang, 
  Tina Churlinovska

-}

module SolWeek2 where

import Week2

{- Exercise 2.1

Time spent
  15 min.

* OR is the fastest (finding a single True statement, the rest can be skipped), 
  so we want to write as much with OR as possible.

* Rectangular checks are most costly, the next costly is NoTriangle. However,
  if it is of NoTriangle all additional tests are useles.

Testing:

* No way of knowing it works for every triangle, as an Integer has infinite numbers and
we would have to check an infinite amount of triangles

* Positive and negative tests: values expected to produce the desired output and values that aren't adequate
for constructing a triangle

-}

data Shape = NoTriangle | Equilateral
            | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | a+b < c || b+c < a || a+c < b                            = NoTriangle
  | a == b && b == c                                         = Equilateral
  | a == b || a == c || b == c                               = Isosceles
  | a*a + b*b == c*c || a*a + c*c == b*b || b*b + c*c == a*a = Rectangular
  | otherwise                                                = Other

-- Very simple tests
testTriangle1 = triangle 1 2 2 == Isosceles && 
                triangle 1 2 1 == Isosceles &&
                triangle 1 2 2 == triangle 4 2 4

testTriangle2 = triangle 3 4 5    == Rectangular &&
                triangle 18 24 30 == Rectangular &&
                triangle 6 8 10 == triangle 5 3 4

{-
 - Exercise 2.2
 
 20 min
 -}

contradiction :: Form -> Bool
contradiction f = not (all (\ v -> eval v f) (allVals f)) 

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)


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


{-
cnf :: Form -> Form 
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj []) = Dsj []
cnf(Dsj [f]) = cnf f 
cnf (Dsj (f1:f2:fs)) = dist (cnf f1) (cnf (Dsj(f2:fs)))


dist :: Form -> Form -> Form 
dist (Cnj []) _ = Cnj []
dist (Cnj [f1]) f2 = dist f1 f2
dist (Cnj (f1:fs)) f2 = Cnj [dist f1 f2, dist (Cnj fs) f2]
dist _ (Cnj []) = Cnj []
dist f1 (Cnj [f2]) = dist f1 f2
dist f1 (Cnj (f2:fs)) = Cnj [dist f1 f2, dist f1 (Cnj fs)]
dist f1 f2 = Dsj [f1,f2]



-}
--dist x y | Dsj (f:fs) f2	= Cnj [dist (f, f2), dist (fs, f2)]
--	 | f2 Dsj (f:fs) 	= Cnj [dist (f2, f), dist (f2, fs)]
--	 | otherwise		= Dsj [x,y]