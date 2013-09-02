module GS

where

-- 1.1

doubleMe x = x + x

doubleSmall x = if x < 10 then x*2 else x-1

-- 1.3

divides d n = rem n d == 0

-- Least divider
ld n = ldf 2 n

-- Least divider function
ldf k n | divides k n = k
        | k^2 >= n    = n
        | otherwise   = ldf (k+1) n


{- 1.4

-}

