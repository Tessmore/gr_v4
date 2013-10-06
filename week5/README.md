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


### Solution

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

# Assignment 4.
The mainfunction in RandonSudoku start with a generating a random sudoku and then one by one removing filled positions while constantly checking if it is still a unique solution. The point is that generating the random sudoku makes use of emptyN which calls the  function constraints, which calls freeAtPos, which calls the modified function freeAtSubgrid. The result is that the generated (but filled) sudoku is already conform to NRC Sudoku constraints. 

Next main will start generating a "problem" (which is removing filled positions as long as there is a unique solution). genProblem calls minimalize, which uses uniquesol, which calls solveNs, which is in the modified file. solveNs calls succNode, which calls extendNode, which call constraints again. And we've already shown that constraints now respects the NRC Sudoku constraints. Hence genProblem respects NRC constraints. 

Finally main will try to solve the generated problem using solveShowNs, which calls solveNs, of which we proved that it will call functions that respect NRC constraints so the solving of the problem also respects NRC contstraints.

Concluding, we did not need to modify anything. All randomly generated, then minimalized and solved sudokus respect the extra constraints because they are implemented in functions that are used by the functions in the main of the random generator. So calling main in RandomSudoku.hs now complies with the exercise.

# Assignment 5.
We need to check whether this problem is minimal, which means no more position can be erased otherwise, the solution will not be unique. In this test, we will judge whether the solution is still unique after we erase one filled position every time. If it is unique, the problem is not minimal; if it isn't, erase other position, and judge whether the solution is unique until every position have been erased once. If the solution to this problem will not be unique every time we erase one filled position, this problem is minimal. 

Run nr 1
*Sudoku> testSudoNRC 
+-------+-------+-------+
|       | 6     |   3   |
| 4   5 | 7   3 |       |
|     1 |     9 |       |
+-------+-------+-------+
|       |       |       |
|       |       |   8 3 |
| 3   8 |       |       |
+-------+-------+-------+
|       | 9     |     1 |
|       | 3 2 5 |       |
| 7     |       |       |
+-------+-------+-------+
+-------+-------+-------+
| 8 9 7 | 6 1 2 | 4 3 5 |
| 4 6 5 | 7 8 3 | 2 1 9 |
| 2 3 1 | 4 5 9 | 8 7 6 |
+-------+-------+-------+
| 1 2 9 | 8 3 4 | 6 5 7 |
| 6 5 4 | 2 9 7 | 1 8 3 |
| 3 7 8 | 5 6 1 | 9 2 4 |
+-------+-------+-------+
| 5 4 2 | 9 7 8 | 3 6 1 |
| 9 1 6 | 3 2 5 | 7 4 8 |
| 7 8 3 | 1 4 6 | 5 9 2 |
+-------+-------+-------+
"solver is right and generated sudoku problem is minimal"

Run 2
*Sudoku> testSudoNRC 
+-------+-------+-------+
|       | 8   2 |       |
|     7 |       |     3 |
| 4 6   |       |       |
+-------+-------+-------+
|       |       |       |
|   2   |       |       |
|       | 6 9   |   4   |
+-------+-------+-------+
|       |     5 | 7   4 |
|       | 9 7   |       |
|       |   3   |   6   |
+-------+-------+-------+
+-------+-------+-------+
| 1 5 3 | 8 4 2 | 9 7 6 |
| 2 8 7 | 5 6 9 | 4 1 3 |
| 4 6 9 | 3 1 7 | 8 2 5 |
+-------+-------+-------+
| 9 1 4 | 2 8 3 | 6 5 7 |
| 8 2 6 | 7 5 4 | 1 3 9 |
| 3 7 5 | 6 9 1 | 2 4 8 |
+-------+-------+-------+
| 6 3 8 | 1 2 5 | 7 9 4 |
| 5 4 2 | 9 7 6 | 3 8 1 |
| 7 9 1 | 4 3 8 | 5 6 2 |
+-------+-------+-------+
"solver is right and generated sudoku problem is minimal"

Run 3
*Sudoku> testSudoNRC 
+-------+-------+-------+
|       |       |       |
|       | 9 2 6 |       |
| 6 5 7 |       |       |
+-------+-------+-------+
|     2 |   1   |     9 |
|       |   8   |       |
|       |       | 3     |
+-------+-------+-------+
|       |       | 7     |
| 2   3 |       |   1   |
|     4 | 7     |   2   |
+-------+-------+-------+
+-------+-------+-------+
| 4 2 9 | 8 7 5 | 6 3 1 |
| 3 8 1 | 9 2 6 | 4 7 5 |
| 6 5 7 | 3 4 1 | 2 9 8 |
+-------+-------+-------+
| 7 6 2 | 4 1 3 | 5 8 9 |
| 9 3 5 | 6 8 7 | 1 4 2 |
| 1 4 8 | 2 5 9 | 3 6 7 |
+-------+-------+-------+
| 8 9 6 | 1 3 2 | 7 5 4 |
| 2 7 3 | 5 9 4 | 8 1 6 |
| 5 1 4 | 7 6 8 | 9 2 3 |
+-------+-------+-------+
"solver is right and generated sudoku problem is minimal"
