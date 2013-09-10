{-

Group GR_V_4: 

  Fabi�n Tesselaar, 
  Tim Gosen, 
  Lulu Zhang, 
  Tina Churlinovska

-}

module SolWeek2 where

import Week2

{- Exercise 2.1

Timeliness

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
 - Exercise 2.3
 -}

{-
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (f:fs)) = dist (cnf f) (Cnj (map cnf fs) )

dist :: Form -> Form -> Form
dist (Cnj x) y = Cnj [(dist (head x) y), (dist (Cnj (tail x)) y)]
dist x (Cnj (y)) = Cnj [dist x (head y), dist x (Cnj (tail y))]
dist x y = Dsj [x, y]
-}


cnf :: Form -> Form 
cnf (Prop x) = Prop x						--Simple proposition is just a simple proposition
cnf (Neg (Prop x)) = Neg (Prop x)				--Negation of prop is just a negation of prop
cnf (Cnj fs) = Cnj (map cnf fs)					--Conjuntions? Good we want conjunctions, so run cnf on all members of conjuntion
cnf (Dsj []) = Dsj []						--Exception to guarantee success of cnf (Dsj(f2:fs)) in which fs can be empty
cnf (Dsj [f]) = cnf f 						--
cnf (Dsj (f1:f2:fs)) = dist (cnf f1) (cnf (Dsj(f2:fs)))		--


dist :: Form -> Form -> Form 
dist (Cnj []) _ = Cnj []
dist (Cnj [f1]) f2 = dist f1 f2
dist (Cnj (f1:fs)) f2 = Cnj [dist f1 f2, dist (Cnj fs) f2]
dist _ (Cnj []) = Cnj []
dist f1 (Cnj [f2]) = dist f1 f2
dist f1 (Cnj (f2:fs)) = Cnj [dist f1 f2, dist f1 (Cnj fs)]
dist f1 f2 = Dsj [f1,f2]




--dist x y | Dsj (f:fs) f2	= Cnj [dist (f, f2), dist (fs, f2)]
--	 | f2 Dsj (f:fs) 	= Cnj [dist (f2, f), dist (f2, fs)]
--	 | otherwise		= Dsj [x,y]

formTest1 =  Equiv (Impl p q) (Neg q)
formTest2 =  Equiv (Cnj [p, q, r]) (Cnj [(Neg p), r])
formTest3 =  Dsj [(Cnj[(Neg q), p]), (Cnj[q,(Neg p)])]
formTest4 =  Cnj [(Dsj[p,q,r]),(Dsj[(Neg p)]),(Dsj[(Neg q), (Neg r)])]
{-
isCnf :: Form -> Bool
isCnf (Prop x) = True
isCnf (Neg (Neg x)) = False
isCnf (Neg x)  = True && isCnf x
isCnf (Impl x y) = False
isCnf (Equiv x y) = False
isCnf (Cnj fs) = and (map isCnf fs)
isCnf (Dsj fs) = let 
		  isCnfHelper (Prop x) = True
		  isCnfHelper (Neg (Neg x)) = False
		  isCnfHelper (Neg x) = True && (isCnfHelper x)
		  isCnfHelper (Impl x y) = False
		  isCnfHelper (Equiv x y) = False
		  isCnfHelper (Dsj fs) = and (map isCnfHelper fs)
		  isCnfHelper (Cnj fs) = False		  	
		 in isCnfHelper (Dsj fs)

-}
isArrowFree :: Form -> Bool
isArrowFree (Equiv x y) = False
isArrowFree (Impl x y) = False
isArrowFree (Cnj fs) = and (map isArrowFree fs)
isArrowFree (Dsj fs) = and (map isArrowFree fs)
isArrowFree (_) = True

isNegNegFree :: Form -> Bool 
isNegNegFree (Neg (Neg x)) = False
isNegNegFree (Cnj fs) = and (map isNegNegFree fs)
isNegNegFree (Dsj fs) = and (map isNegNegFree fs)
isNegNegFree (_) = True

isCnf :: Form -> Bool
isCnf x = let
           isCnfHelp1 (Cnj fs) = and (map isCnfHelp1 fs)
	   --isCnfHelp1 (Dsj fs) = l
	  in (isArrowFree x) && (isNegNegFree x) && (isCnfHelp1 x)

