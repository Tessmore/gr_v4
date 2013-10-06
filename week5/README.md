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

Given a normal sudoku solver, we must formalize extra constraints so it can
solve special NRC sudoku's

Time spent : 10 hour

In a sense, the formalization of the constraint is simple. All the rows, columns and
3x3 blocks  must have the numbers 1..9 for a normal sudoku. Now for an NRC sudoku inside the grid, there are
four extra "3x3" blocks added. To add this as a constraint we need the following subgridInjective check

Consistent checks if possible solutions are still consistent with the constraints set by the (NRC) sudoku rules 
The first occurence of subgridInjective checks that the original constraints for the nine 3x3 (original) blocks are met
The second occurence of subgridInjective checks that the new constraints for the four 3x3 blocks are met.

    consistent :: Sudoku -> Bool
    consistent s = and $
                   [ rowInjective s r |  r <- positions ]
                    ++
                   [ colInjective s c |  c <- positions ]
                    ++
                   [ subgridInjective s (r,c) 1 | 
                        r <- [1,4,7], c <- [1,4,7]]
                    ++
                   [ subgridInjective s (r,c) 2 |
                        r <- [2,6], c <- [2,6]]
                        
To make this work, the rest of the code has to be altered slightly. Hence, the added "1" and "2".
We modified the `subgridInjective` function so that it can differentate the blockType of a subgrid (original 9 vs new 4).

    -- Block type support
    bl :: Int -> Int -> [Int]
    if blockType == 1 then concat $ filter (elem x) blocks else concat $ filter (elem x) extraBlocks

    subgridInjective :: Sudoku -> (Row,Column) -> Int -> Bool
    subgridInjective s (r,c) blockType = injective vs where 
       vs = filter (/= 0) (subGrid s (r,c) blockType)


Another adjustment is that `FreeInSubgrid` now respects both types of grids. This is done by `intersects` of the possible values of that position such that it accounts for the constraints set by both types of blocks

    freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
    freeInSubgrid s (r,c) = (freeInSeq (subGrid s (r,c) 1)) `intersect` (freeInSeq (subGrid s (r,c) 2))


## Solution

Using `solveAndShow example6` results in the following:

    +-------+-------+-------+
    | 4 7 8 | 3 9 2 | 6 1 5 |
    | 6 1 9 | 7 5 8 | 3 2 4 |
    | 2 3 5 | 4 1 6 | 9 7 8 |
    +-------+-------+-------+
    | 7 2 6 | 8 3 5 | 1 4 9 |
    | 8 9 1 | 6 2 4 | 7 5 3 |
    | 3 5 4 | 9 7 1 | 2 8 6 |
    +-------+-------+-------+
    | 5 6 7 | 2 8 9 | 4 3 1 |
    | 9 8 3 | 1 4 7 | 5 6 2 |
    | 1 4 2 | 5 6 3 | 8 9 7 |
    +-------+-------+-------+