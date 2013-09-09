{-

Group GR_V_4: 

  Fabiën Tesselaar, 
  Tim Gosen, 
  Lulu Zhang, 
  Tina Churlinovska

-}

module Fabien where

data Shape = NoTriangle | Equilateral
            | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | a+b < c || b+c < a || a+c < b                            = NoTriangle
  | a == b && b == c                                         = Equilateral
  | a == b || a == c || b == c                               = Isosceles
  | a*a + b*b == c*c || a*a + c*c == b*b || b*b + c*c == a*a = Rectangular
  | otherwise                                                = Other
  
-- Checking with OR is the fastest (as a single True, the rest can be skipped), so we want to write the condi
-- Checking for Rectangular is most costly, next up is NoTriangle. However,
-- if it is of NoTriangle all additional tests are useles

-- No way of knowing it works for every triangle, as Integer has infinite numbers

-- Testing
-- no argument ERROR
-- 1           ERROR
-- 1 2         Error

-- 0 1 2 == NoTriangle
-- 3 3 3 == Equilateral
-- 2 3 3 == 3 3 2 == 3 2 3 == 1 1 2 et. == Isosceles.
-- 3 4 5 == 5 4 3 == 5 3 4 Rectangular
-- 2 3 4 == Other