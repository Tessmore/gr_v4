{-

Group GR_V_4: 

  Fabi�n Tesselaar, 
  Tim Gosen, 
  Lulu Zhang, 
  Tina Churlinovska

-}

module Week22 where

data Shape = NoTriangle | Equilateral
            | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape

triangle x y z 
			| x>0 && y>0 && z>0 && x+y>z && x+z>y && y+z>x && x==y && x==z && y==z = Equilateral
      			| x>0 && y>0 && z>0 && x+y>z && x+z>y && y+z>x &&(x==y && x/=z && z/=y) 
        			 || (x/=y && x/=z && z==y) 
        			 || (x/=y && z/=y && x==z) = Isosceles 
		        | x>0 && y>0 && z>0 && x+y>z && x+z>y && y+z>x && x*x+y*y==z*z = Rectangular
			| x>0 && y>0 && z>0 && x+y>=z && x+z>=y && y+z>=x && x/=y && x/=z && y/=z = Other
			| x>0 && y>0 && z>0 && x+y<z || x+z<y || y+z<x = NoTriangle
			
{- So this is one trivial solution that need to be implemented with predicate logic  suppose :D -}
