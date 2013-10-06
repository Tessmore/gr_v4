{-
Week 2 assignment

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
-- VVZ: the code is nicely symmetrical, could we write less of it by sorting the triple?
triangle a b c
  | a+b < c || b+c < a || a+c < b                            = NoTriangle
  | a == b && b == c                                         = Equilateral
  | a == b || a == c || b == c                               = Isosceles
  | a*a + b*b == c*c || a*a + c*c == b*b || b*b + c*c == a*a = Rectangular
  | otherwise                                                = Other

-- Triangle test helpers
-- Accepts a list to be used with triangle function
triangle2 :: [Integer] -> Shape
triangle2 (a:b:c:xs) = triangle a b c

-- Returns the Shape + what made the shape for testing purposes
triangle3 :: [Integer] -> (Shape, [Integer])
triangle3 (a:b:c:xs) = (triangle a b c, [a,b,c])

-- Test all combinations of rectangles based on a list of integers
testAllTriangleShapes :: [Integer] -> [(Shape, [Integer])]
testAllTriangleShapes list = map triangle3 (replicateM 3 list)

{-

  Automated test to make sure the order of given numbers does not matter

  1. Generates all permutations
  2. Casts triangle on every element
  3. Using nub, all duplicate Shapes are removed
  4. Check if the list only contains one item
  
-}
testTrianglePermutations :: Integer -> Integer -> Integer -> Bool
testTrianglePermutations a b c = length (nub (map triangle2 (permutations [a,b,c]))) == 1


{-
  Automated test to check the function for finding Rectangular shaped triangle
  
  1. Go through all possible triangles with sides of length 1 to 5
  2. There can only be a single rectangular triangle (sides with 3 4 5)
-}

-- Check for a Rectangular shaped triangle
isRectangle :: (Shape, [Integer]) -> Bool
isRectangle (x, (h:t)) = x == Rectangular

-- Builds a list of all combinations of triangles, then filters them to get a list
-- of triangles that are Rectangular shaped. For inspection of the lengths
testRectangularTriangle :: [(Shape, [Integer])]
testRectangularTriangle = filter (isRectangle) (map triangle3 (replicateM 3 [0..5]))


{- 
  Basic tests

  For known side lengths we expect certain output
  We also want to test if it does not return a certain shape it cannot possibly be
-}
-- VVZ: no automated testing? :-(

testTriangle1a = triangle 1 2 5    == NoTriangle
testTriangle1b = triangle 5 2 5    /= NoTriangle

testTriangle2a = triangle 5 5 5    == Equilateral
testTriangle2b = triangle 4 5 5    /= Equilateral

testTriangle3a = triangle 1 2 2    == Isosceles
testTriangle3b = triangle 3 3 3    /= Isosceles

testTriangle4a = triangle 18 24 30 == Rectangular
testTriangle4b = triangle 2 3 5    /= Rectangular

testTriangle5a = triangle 6 4 5    == Other
testTriangle5b = triangle 1 2 2    /= Other

-- Should all be true                
testAllTriangles = testTriangle1a && 
                   testTriangle1b && 
                   testTriangle2a && 
                   testTriangle2b && 
                   testTriangle3a && 
                   testTriangle3b && 
                   testTriangle4a &&
                   testTriangle4b &&
                   testTriangle5a &&
                   testTriangle5b



{-
 - Exercise 2.2
 
 20 min
 -}

-- Any possible input must return false for "f" to be a contradiction.
-- So there should not be any solution
contradiction :: Form -> Bool
contradiction f = not (any (\ v -> eval v f) (allVals f))
-- VVZ: I have seen this code after "not" before. have you? be lazy and reuse your old functions!
-- VVZ: contradiction = not . satisfiable

-- Testing (must be true)
contradictionTest1 = contradiction (Cnj [(Impl p q), (Impl p (Neg q))]) == False
contradictionTest2 = contradiction (Cnj [Neg (Impl p q), Neg (Impl q p)])
contradictionTest3 = contradiction (Dsj [p, Neg p]) == False
contradictionTest4 = contradiction (Cnj [p, Neg p])
contradictionTest5 = contradiction (Dsj [p, q]) == False
contradictionTest6 = contradiction (Cnj [p, q]) == False

contradictionTestAll = contradictionTest1 &&
                       contradictionTest2 &&
                       contradictionTest3 &&
                       contradictionTest4 &&
                       contradictionTest5 &&
                       contradictionTest6

-- All possible input must return true for "f" to be a tautology
tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

tautologyTest1 = tautology (Dsj [p, Neg p])
tautologyTest2 = tautology (Cnj [p, Neg p]) == False
tautologyTest3 = tautology (Dsj [p, q]) == False
tautologyTest4 = tautology (Cnj [p, q]) == False

-- logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

entailsTest1 = entails (Impl (Impl p q) p) p
entailsTest2 = entails (Neg p) p == False
entailsTest3 = entails (Equiv p q) (Dsj[Cnj[p,q],Cnj[Neg p, Neg q]])

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

-- Testing
equivTest1 = equiv (Cnj [Dsj [p, q], Dsj [p, r]]) (Cnj [Dsj [p, q], Dsj [p, r]]) -- Identical formula
equivTest2 = equiv (Cnj [Dsj [p, q], Dsj [p, r]]) (Cnj [Dsj [p, r], Dsj [p, q]]) -- Rearranged formula

-- VVZ: very good!

{-
 - Exercise 2.3
 -
 - 2 hours implementation of function
 - 2 hours of writing tests
 - 1 hour Testing
 - 1/2 hour documentation of tests
 -}

cnf :: Form -> Form 
cnf (Prop x) = Prop x                                     --Propositions in itself are cnf
cnf (Neg (Prop x)) = Neg (Prop x)                         --Negations in itself are cnf
cnf (Cnj fs) = Cnj (map cnf fs)                           --Conjuntions? Good we want conjunctions, so run cnf on all members of conjuntion
cnf (Dsj []) = Dsj []                                     --Exception to help cnf (Dsj(f2:fs)) in which fs can be empty
cnf (Dsj [f]) = cnf f                                     --Make the members of a disjuntion cnf
cnf (Dsj (f1:f2:fs)) = dist (cnf f1) (cnf (Dsj(f2:fs)))   --Incase of a Dsj you want all its propositions to be in cnf aswell before dist so it can transform inner conjunctions to disjuncions

dist :: Form -> Form -> Form
dist (Cnj []) _ = Cnj []                                    --Conjunctions of empty lists should be passed up the recursion tree again
dist (Cnj [f1]) f2 = dist f1 f2                             --Case towards disjunction step but after important recursion
dist (Cnj (f1:fs)) f2 = Cnj [dist f1 f2, dist (Cnj fs) f2]  --The first important recusions step mentioned in the slides
dist _ (Cnj []) = Cnj []                                    --Again conjunctions of empty lists should be passed up the recussion tree again
dist f1 (Cnj [f2]) = dist f1 f2                             --Case towards disjuntion step but 
dist f1 (Cnj (f2:fs)) = Cnj [dist f1 f2, dist f1 (Cnj fs)]  --The second important recursion step mentioned in the slides
dist f1 f2 = Dsj [f1,f2]                                    --Ret

-- some predefined formulas for testing purposes.
formTest1 =  Equiv (Impl p q) (Neg q)
formTest2 =  Equiv (Cnj [p, q, r]) (Cnj [(Neg p), r])
formTest3 =  Dsj [(Cnj[(Neg q), p]), (Cnj[q,(Neg p)])]
formTest4 =  Cnj [(Dsj[p,q,r]),(Dsj[(Neg p)]),(Dsj[(Neg q), (Neg r)])]

-- Test function that guarantees that a formula is arrow free
isArrowFree :: Form -> Bool
isArrowFree (Equiv x y) = False
isArrowFree (Impl x y) = False
isArrowFree (Cnj fs) = and (map isArrowFree fs)
isArrowFree (Dsj fs) = and (map isArrowFree fs)
isArrowFree (_) = True

-- Test funtion that guarantees that a formula is double negation free
isNegNegFree :: Form -> Bool 
isNegNegFree (Neg (Neg x)) = False
isNegNegFree (Cnj fs) = and (map isNegNegFree fs)
isNegNegFree (Dsj fs) = and (map isNegNegFree fs)
isNegNegFree (_) = True

-- Test function that guarantees that a formula is in cnf by checking the rules for cnf
isCnf :: Form -> Bool
isCnf x = let
			-- VVZ: the next line is incorrect: for CNF inside the top level conjunctions are disjunctions, not CNFs
            isCnfHelp1 (Cnj fs) = and (map isCnfHelp1 fs)
            isCnfHelp1 (Dsj fs) = let
                                    isCnfHelp2 (Prop x) = True
                                    isCnfHelp2 (Neg x)  = True && (isCnfHelp2 x)
                                    -- VVZ: the next line is incorrect: there are no disjunctions allowed inside disjunctions in the CNF definition
                                    isCnfHelp2 (Dsj x)  = and (map isCnfHelp2 x)
                                    isCnfHelp2 (Cnj x)  = False
                                  in and (map isCnfHelp2 fs)
            isCnfHelp1 _ = True
          in (isArrowFree x) && (isNegNegFree x) && (isCnfHelp1 x)

-- Test function that checks if a Formula and its cnf are equivalent and that its cnf is actually cnf
-- Use with f and (cnf (nnf (arrowfree f))) as inputs
isCorrectCnf :: Form -> Form -> Bool
isCorrectCnf f cnfF = (isCnf cnfF) && (equiv f cnfF)

-- Test function to check all conversion to CNF are correct.
-- VVZ: do you like to repeat yourselves? do you like to repeat yourselves?
-- VVZ: why not write one function defined as "cnf . nnf . arrowfree" and use it everywhere?
cnfTest1 = isCnf (cnf (nnf (arrowfree formTest1)))
cnfTest2 = isCnf (cnf (nnf (arrowfree formTest2)))
cnfTest3 = isCnf (cnf (nnf (arrowfree formTest3)))
cnfTest4 = isCnf (cnf (nnf (arrowfree formTest4)))

-- VVZ: You also miss another function that would 'flatten' nested conjunctions and disjunctions. The formulae on the slides used associativity and hence assumed the flattener of x & (y & z) to x & y & z in the head of the reader, but in the implementation your rewritings could make quite a mess of the structure of conjunction/disjunction lists, not to mention that the input is 'any formula', so it can be already messed up.
-- VVZ: cnf (nnf (arrowfree (Cnj [Cnj [p,q,p], q])))
-- VVZ: I expect to see *(1 2 1 2), not *(*(1 2 1) 2)

--Test fuction that checks all.
testAllCnf = cnfTest1 && cnfTest2 && cnfTest3 && cnfTest4

--Test function to check that its the correct CNF for the test formula.
correctCnfTest1 = isCorrectCnf formTest1 (cnf (nnf (arrowfree formTest1)))
correctCnfTest2 = isCorrectCnf formTest2 (cnf (nnf (arrowfree formTest2)))
correctCnfTest3 = isCorrectCnf formTest3 (cnf (nnf (arrowfree formTest3)))
correctCnfTest4 = isCorrectCnf formTest4 (cnf (nnf (arrowfree formTest4)))

--Test function to check all all conversion are the correct cnf of the test formulas.
testAllCorrectCnf = correctCnfTest1 && correctCnfTest2 && correctCnfTest3 && correctCnfTest4
