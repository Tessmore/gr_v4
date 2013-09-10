{-

Group GR_V_4: 

  Fabien Tesselaar, 
  Tim Gosen, 
  Lulu Zhang, 
  Tina Churlinovska

-}

module SolWeek2 where

import Week2
import Data.List
import Control.Monad

{- Exercise 2.1

Time spent
   5 min. on reading wikipedia triangle information
  15 min. on function
  20 min reading on testing
   1 min writing simple tests
   3 min writing additional permutation / combination tests
   
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

-- Triangle test helper
triangle2 :: [Integer] -> Shape
triangle2 [] = NoTriangle
triangle2 (a:b:c:xs) = triangle a b c

-- Test to make sure the order of given numbers does not matter
-- 1. Generates all permutations
-- 2. Casts triangle on every element
-- 3. Using nub, all duplicate Shapes are removed
-- 4. Check if the list only contains one item
testTrianglePermutations :: Integer -> Integer -> Integer -> Bool
testTrianglePermutations a b c = length (nub (map triangle2 (permutations [a,b,c]))) == 1

-- Test all possible combinations of 3 given numbers
-- TODO get some use for this
testTriangleCombinations a b c = map triangle2 (replicateM 3 [a,b,c])

-- Very simple tests
-- Should all be true
testTriangle1 = triangle 1 2 5 == NoTriangle && 
                triangle 0 3 2 == NoTriangle &&
                triangle 7 3 2 == NoTriangle

testTriangle2 = triangle 5 5 5 == Equilateral && 
                triangle 9 9 9 == Equilateral

testTriangle3 = triangle 1 2 2 == Isosceles && 
                triangle 1 2 1 == Isosceles &&
                triangle 1 2 2 == triangle 4 2 4

testTriangle4 = triangle 3 4 5    == Rectangular &&
                triangle 18 24 30 == Rectangular &&
                triangle 6 8 10   == triangle 5 3 4

testTriangle5 = triangle 6 4 5 == Other &&
                triangle 2 4 3 == Other
                
testAllTriangles = testTriangle1 && 
                   testTriangle2 && 
                   testTriangle3 && 
                   testTriangle4 &&
                   testTriangle5


{-
 - Exercise 2.2
 
 20 min
 -}

-- Any possible input must return false for "f" to be a contradiction.
contradiction :: Form -> Bool
contradiction f = not (all (\ v -> eval v f) (allVals f)) -- not (tautology f)

-- Any possible input must return true for "f" to be a tautology
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

