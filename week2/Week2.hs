{-

Group GR_V_4: 

  Fabiën Tesselaar, 
  Tim Gosen, 
  Lulu Zhang, 
  Tina Churlinovska

-}

module Week22 where

data Shape = NoTriangle | Equilateral
            | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
