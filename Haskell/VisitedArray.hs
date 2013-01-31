module VisitedArray 
(VisitedArray, updateVisited, initVArray, testVisitedArrayModule) where
import Coordinate

type VisitedArray = ([Bool], Int)
-- A visited array is a tuple of a boolean array with True for every
-- coordinate index which has been visited, and an Int showing the number of 
-- spaces unvisited (i.e. number of False instances in the array)

initVArray :: Grid -> VisitedArray
initVArray grid = (visited, nRemaining) where
-- Initialized VisitedArray has one true entry at 0 index and false otherwise
	nRemaining = (gSize grid) - 1
	visited = True : (take nRemaining $ repeat False)



updateVisited :: VisitedArray -> Coordinate -> Grid -> VisitedArray
updateVisited (boolArr, nRemaining) loc grid
	-- If we have already visited the current location - return the existing 
	-- VisitedArray
	| boolArr !! idx = (boolArr, nRemaining)
	-- Otherwise we make a new visitedArray 
	| otherwise = (newArr, newRemaining) where
		idx = coord2Int loc grid
		newArr = setTrueAt idx boolArr
		newRemaining = nRemaining - 1

setTrueAt :: Int -> [Bool] -> [Bool]
setTrueAt _ [] = error "setTrueAt: Index out of bounds"
setTrueAt i (x:xs)
	| i == 0 = True : xs 
	| i < 0  = error "setTrueAt: Negative index"
	| otherwise = x : (setTrueAt (i-1) xs)

-- === Test Section ===- --

testVisitedArrayModule :: Bool
testVisitedArrayModule = and [testInitVArray, testSetTrueAt, testUpdateVisited]

testInitVArray :: Bool
testInitVArray = initVArray (squareGrid 2) == ([True, False, False, False], 3)

testSetTrueAt :: Bool
testSetTrueAt = and [t1, t2] where
	t1 = setTrueAt 0 [False,False,False] == [True,False,False]
	t2 = setTrueAt 1 [False,False,False] == [False,True,False]
	t3 = setTrueAt 1 [False,True,True]   == [False,True,True]

testUpdateVisited :: Bool
testUpdateVisited = and [t1, t2, t3, t4] where
	-- Say we move a new rover around a bit...
	grid = squareGrid 2
	blankSlate = initVArray grid
	move0 = updateVisited blankSlate (Coordinate 0 0) grid
	move1 = updateVisited move0      (Coordinate 1 0) grid
	move2 = updateVisited move1      (Coordinate 1 1) grid
	move3 = updateVisited move2      (Coordinate 1 0) grid 
	t1 = move0 == ([True, False, False, False], 3)
	t2 = move1 == ([True, True, False, False ], 2)
	t3 = move2 == ([True, True, False, True  ], 1)
	t4 = move3 == ([True, True, False, True  ], 1)