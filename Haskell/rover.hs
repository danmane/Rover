module Rover 
( Rover
, makeRover
, moveRover
, traverse
, traversals
, parallelTraversals
, testRoverModule
) where

import System.Random
import qualified Data.Set as Set
import Data.List
import Coordinate
import Direction
import VisitedArray
--V1 - took 64s on 10k/20x20 test set (o(g))

data Rover = Rover { rgrid :: Grid
				   -- the grid the rover is on
				   , location :: Coordinate
				   -- the current coordinate
				   , vArray :: VisitedArray
				   -- set of visited coordinates (by index of the coordinate)
				   } deriving (Show, Eq)


makeRover :: Grid -> Rover
-- Given the x and y bounds of the grid, make a new rover on that grid
makeRover grid = Rover grid startCoord (visited, nRemaining) where
	nRemaining = gSize grid - 1
	visited = True : (take nRemaining $ repeat False)

moveRover :: Direction -> Rover -> Rover
-- takes a direction and a rover, and constructs a new rover that's moved in 
-- the given direction
moveRover dir rover = Rover grid newCoord newVisited where
	grid = rgrid rover
	newCoord = moveCoord (location rover) grid dir
	newVisited = updateVisited (vArray rover) newCoord grid

nRemaining :: Rover -> Int
nRemaining = snd . vArray

traverse :: Grid -> StdGen -> Int
-- Takes an x and y bounds for the grid and a generator
-- Returns the number of steps it took the rover to traverse the grid
traverse grid gen = finalSteps where
	rover = makeRover grid
	directions = randomDirs gen
	roverStates = scanl (flip moveRover) rover directions 
	finalSteps = length $ takeWhile (\x -> nRemaining x > 0) roverStates


traversals :: Grid -> StdGen -> [Int]
-- given a Grid and a StdGen, return an infinite stream of traversals
-- each int is the number of steps it took a rover to reach every space
traversals grid gen = traverseStream grid (randomDirs gen) where
	traverseStream :: Grid -> [Direction] -> [Int]
	traverseStream grid dirs = nSteps : traverseStream grid newDirs where
		roverStates = scanl (flip moveRover) (makeRover grid) dirs
		(traversal, _) = span (\x -> nRemaining x > 0) roverStates
		nSteps = length traversal
		newDirs = drop nSteps dirs

stdGenStream :: StdGen -> [StdGen]
stdGenStream gen = map mkStdGen $ randoms gen

parallelTraversals :: Grid -> StdGen -> [Int]
-- not actually parallel. i was hoping that *map* would automatically parallelize it...
parallelTraversals grid gen = map (traverse grid) (stdGenStream gen)



------------------------------------
-- Test Functions Below (yay TDD) --
------------------------------------

testRoverModule :: Bool
testRoverModule = and [testMoveRover, testTraverse, testTraversals, testNumRemaining]

testMoveRover = and [t1, t2, t3] where
	t1 = location moved == startCoord  where
		moved = foldr moveRover (makeRover (squareGrid 2)) [U, R, D, L] 
	t2 = location moved == Coordinate 2 1 where
		moved = foldr moveRover (makeRover (squareGrid 3)) [R, R, L, R, U]
	t3 = location moved == Coordinate 9 0 where
		moved = moveRover L (makeRover (squareGrid 10))

testNumRemaining = and [t1, t2] where
	t1 = nRemaining moved == 0 where
		moved = foldr moveRover (makeRover (squareGrid 2)) [U, R, D, L] 
	t2 = nRemaining moved == 6 where
		moved = foldr moveRover (makeRover(squareGrid 3)) [R, R, R, L, L, R, R, L]

testTraverse = and [t1, t2, t3] where
	t1 = traverse (squareGrid 2) (mkStdGen 100) == 11
	t2 = traverse (squareGrid 2) (mkStdGen 23 ) == 3
	t3 = traverse (squareGrid 3) (mkStdGen 7  ) == 9
	-- Tested these by hand...

testTraversals = and [t1, t2, t3, t4] where
	t1 = take 4 (traversals (squareGrid 2) (mkStdGen 7)) == [3,4,9,6] 
	-- t1 was verified by hand
	-- t2-t4 just verify the head, not the sequence...
	t2 = head (traversals (squareGrid 2) (mkStdGen 100)) == 11
	t3 = head (traversals (squareGrid 2) (mkStdGen 23 )) == 3
	t4 = head (traversals (squareGrid 3) (mkStdGen 7  )) == 9