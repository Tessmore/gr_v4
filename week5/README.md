Week 5
=====

Group members : 

* Tim Gosen
* Lulu Zhang
* Fabiën Tesselaar

# Assignment 1.

Find a suitable assertion of mergeSrt, and write an assertive version of this.  

Time spent: 3 hours

For the assertive function, we want mergeSrt to create a sorted list as a result. 
This means the output condition should be a sorted list. mergeSrt has one parameter as input, 
one for output. Thus we can use "post1" with "sorted" condition to assert
mergeSrt into returning a sorted list (or giving a post1 error condition if
inpossible).

## Code (Lab5.hs)

    mergeSrt :: Ord a => [a] -> [a]
    mergeSrt [] = []
    mergeSrt (x:xs) = merge [x] (mergeSrt xs)

    -- Program using the assertion
    mergeSrtA :: Ord a => [a] -> [a]
    mergeSrtA = post1 sorted mergeSrt

    
# Assignment 2.

Implementing merge sort by splitting the list to be sorted in equal parts.
Also implement a suitable assertion of this version

Time spent: 1 hour

For the assertion, this is the same as in assignment 1. We want a post condition
of a sorted list, but this time using mergeSrtSplit

## Code (Lab5.hs)
  
    split :: [a] -> ([a],[a])
    split xs = let
        n = length xs `div` 2
      in
        (take n xs, drop n xs)
        
    mergeSrtSplit []  = []
    mergeSrtSplit [x] = [x]
    mergeSrtSplit xs  = merge (mergeSrtSplit ys) (mergeSrtSplit zs)
                          where (ys, zs) = split xs
                      
    -- Program using the assertion
    mergeSrtSplitA :: Ord a => [a] -> [a]
    mergeSrtSplitA = post1 sorted mergeSrtSplit

    
# Assignment 3.


# Assignment 4.
The mainfunction in RandonSudoku start with a generating a random sudoku and then one by one removing filled positions while constantly checking if it is still a unique solution. The point is that generating the random sudoku makes use of emptyN which calls the  function constraints, which calls freeAtPos, which calls the modified function freeAtSubgrid. The result is that the generated (but filled) sudoku is already conform to NRC Sudoku constraints. 

Next main will start generating a "problem" (which is removing filled positions as long as there is a unique solution). genProblem calls minimalize, which uses uniquesol, which calls solveNs, which is in the modified file. solveNs calls succNode, which calls extendNode, which call constraints again. And we've already shown that constraints now respects the NRC Sudoku constraints. Hence genProblem respects NRC constraints. 

Finally main will try to solve the generated problem using solveShowNs, which calls solveNs, of which we proved that it will call functions that respect NRC constraints so the solving of the problem also respects NRC contstraints.

Concluding, we did not need to modify anything. All randomly generated, then minimalized and solved sudokus respect the extra constraints because they are implemented in functions that are used by the functions in the main of the random generator.
