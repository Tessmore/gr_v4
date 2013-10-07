module Lab6

where
import Data.List
import System.Random
import Week6
       
{- 
  Assignment 1.
  Time spent:
  
  Source used for detailed explenation: 
    http://www.tricki.org/article/To_work_out_powers_mod_n_use_repeated_squaring
  
  Function that does modular exponentiation of x^y in polynomial time, by
  repeatedly squaring modulo N.
-}
                   
exM' :: Integer -> Integer -> Integer -> Integer 
exM' x 0 n = 1
exM' x y n = let k = exM' x (y `div` 2) n
               in if (even y)
                    then     (k * k) `mod` n
                    else (x * k * k) `mod` n


-- poep 3 4 37 53 => poep 3^4 16
poep :: Integer -> Integer -> Integer -> Integer -> [Integer]
poep x k y n
  | x `rem` n <= y = x `rem` n : poep (x^k) (2*k) y n
  | otherwise = []
                

{- 
  Assignment 2.
  Time spent:
  
  Check that your implementation is more efficient than expM by running
  relevant tests and documenting the results.

  
-}  


{-
  Assignment 3.
  
  In order to test Fermat's Primality Check (as implemented in function primeF),
  the list of prime numbers generated by Eratosthenes' sieve is useless, for Fermat's
  Primality Check correctly classify the primes as primes. Where the check can go
  wrong is on classifying composite numbers; these can slip through the Fermat test.

  Write `composites` function. It should generate an infinite list of composite 
  natural numbers. 
   
   Hint: modify Eratosthenes' sieve, so that instead of throwing away composite numbers, 
   it marks them as false. Next filter out the numbers marked as false.

-}

--composites :: [Integer]



{- 
  Assignment 4.
  Time spent:
  
  Use the list of composite numbers to test Fermat's primality check. What is the
  least composite number that you can find that fools the check, for `testF k` with   
  k = 1; 2; 3 ? What happens if you increase k?

-}



{- 
  Assignment 5.  
  Time spent: 
  
  Use the list generated by the `carmichael` function for a further test of 
  Fermat's primality check. Read the entry on Carmichael numbers on 
  Wikipedia to explain what you find.
-}

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]


{-
  Assignment 6.
  Time spent:
  
  Use the list from the previous exercise to test the Miller-Rabin primality check.
  What do you find?
  
-}


{-
  Assignment 7.
  Time spent:
  
  You can use the Miller-Rabin primality check to discover some large Mersenne
  primes. 
  
  The recipe: 
    * Take a large prime p
    * Use the Miller-Rabin algorithm to check whether 2^p - 1 is also prime
    * Check on internet the found numbers  are genuine Mersenne primes. 
    
  Report on your findings
  
-}