import System.Random
import qualified Data.Set as Set
import Data.List

data Direction = U | D | L | R deriving (Show, Eq)

data Grid = Grid { gridX :: Int, gridY :: Int, gSize :: Int } deriving (Show, Eq)
-- x and y bounds of the grid, and number of spaces in the grid

data Coordinate = Coordinate {x :: Int, y :: Int} deriving (Show, Eq)

data Rover = Rover { rgrid :: Grid
				   -- the grid the rover is on
				   , location :: Coordinate
				   -- the current coordinate
				   , visited :: Set.Set Int
				   -- set of visited coordinates (by index of the coordinate)
				   , nRemaining :: Int
				   -- number of unvisited spots
				   } deriving (Show, Eq)

makeGrid :: Int -> Int -> Grid
makeGrid x y = Grid x y (x*y)

squareGrid :: Int -> Grid
squareGrid x = Grid x x (x*x)

makeRover :: Grid -> Rover
-- Given the x and y bounds of the grid, make a new rover on that grid
makeRover grid = Rover grid startCoord visited nRemaining where
	startCoord = Coordinate 0 0
	nRemaining = gSize grid - 1
	visited = Set.singleton 0 

moveRover :: Direction -> Rover -> Rover
-- takes a direction and a rover, and constructs a new rover that's moved in 
-- the given direction
moveRover dir rover = Rover grid newCoord newVisited newRemaining where
	grid = rgrid rover
	newCoord = moveCoord (location rover) grid dir
	newVisited = Set.insert (coord2Int newCoord grid) (visited rover)
	newRemaining = gSize grid - Set.size newVisited

moveCoord :: Coordinate -> Grid -> Direction -> Coordinate
moveCoord startCoord theGrid dir = Coordinate newBX newBY where
	newX = case dir of 
		R -> (x startCoord) + 1
		L -> (x startCoord) - 1
		otherwise -> x startCoord
	newY = case dir of 
		U -> (y startCoord) + 1
		D -> (y startCoord) - 1
		otherwise -> y startCoord

	smartmod x y = (x + y) `mod` y
	-- smartmod handles negative numbers better
	newBX = newX `smartmod` (gridX theGrid)
	newBY = newY `smartmod` (gridY theGrid)

randomDirs :: StdGen -> [Direction]
randomDirs gen = map i2d $ randomRs (1,4) gen where
	i2d :: Int -> Direction
	i2d x = case x of
		1 -> U
		2 -> R
		3 -> D
		4 -> L

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

coord2Int :: Coordinate -> Grid -> Int
coord2Int coord grid = (x coord) + (y coord) * (gridX grid)

data2csv :: [Int] -> String
data2csv dat = concat $ intersperse "," $ map show dat

main :: IO()
main = putStrLn $ data2csv $ take 100 $ parallelTraversals (squareGrid 100) (mkStdGen 100)

------------------------------------
-- Test Functions Below (yay TDD) --
------------------------------------

allTests :: Bool
allTests = and [testMoveCoord, testCoord2Int, testMoveRover, testRandomDirs,
				testTraverse, testTraversals]

testMoveCoord = and [t1, t2, t3, t4] where
	t1 = (nc == Coordinate 0 1) where
		testc = Coordinate 0 0 
		testg = squareGrid 5
		nc = moveCoord testc testg U
	t2 = (nc == Coordinate 1 5) where
		testc = Coordinate 2 5
		testg = squareGrid 7
		nc = moveCoord testc testg L
	t3 = (nc == Coordinate 0 0) where
		testc = Coordinate 4 0
		testg = squareGrid 5
		nc = moveCoord testc testg R
	t4 = (nc == Coordinate 3 6) where
		testc = Coordinate 3 0
		testg = squareGrid 7 
		nc = moveCoord testc testg D

testCoord2Int = and [tc1, tc2, tc3, tc4] where
	tc1 = (idx == 0) where
		idx = coord2Int (Coordinate 0 0) (squareGrid 5)
	tc2 = (idx == 5) where
		idx = coord2Int (Coordinate 5 0) (squareGrid 6)
	tc3 = (idx == 8) where
		idx = coord2Int (Coordinate 2 2) (squareGrid 3)
	tc4 = (idx == 4) where
		idx = coord2Int (Coordinate 0 1) (squareGrid 4)

testMoveRover = and [t1, t2] where
	t1 = moved == Rover (makeGrid 2 2) (Coordinate 0 0) fullset 0 where
		fullset = Set.fromList [0..3]
		moved = foldr moveRover (makeRover (squareGrid 2)) [U, R, D, L] 
	t2 = (nRemaining moved == 6) && (visited moved == Set.fromList [0,1,2]) where
		moved = foldr moveRover (makeRover(squareGrid 3)) [R, R, R, L, L, R, R, L]

testRandomDirs = take 5 (randomDirs (mkStdGen 0)) == [R, L, R, U, D]

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