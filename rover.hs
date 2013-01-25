import System.Random
import qualified Data.Set as Set

data Direction = U | D | L | R deriving (Show, Eq)

data Grid = Grid { gridX :: Int, gridY :: Int, gSize :: Int } deriving (Show, Eq)
-- x and y bounds of the grid, and number of spaces in the grid
makeGrid :: Int -> Int -> Grid
makeGrid x y = Grid x y (x*y)

data Coordinate = Coordinate {x :: Int, y :: Int} deriving (Show, Eq)

data Rover = Rover { rgrid :: Grid
				   -- the grid the rover is on
				   , coord :: Coordinate
				   -- the current coordinate
				   , visited :: Set.Set Int
				   -- set of visited coordinates (by index of the coordinate)
				   , nRemaining :: Int
				   -- number of unvisited spots
				   } deriving (Show, Eq)

makeRover :: Int -> Int -> Rover
-- Given the x and y bounds of the grid, make a new rover on that grid
makeRover gx gy = Rover newGrid startCoord visited nRemaining where
	newGrid = makeGrid gx gy
	startCoord = Coordinate 0 0
	nRemaining = gSize newGrid - 1
	visited = Set.singleton 0 

moveRover :: Direction -> Rover -> Rover
moveRover dir rover = Rover roverGrid newCoord newVisited newRemaining where
	roverGrid = rgrid rover
	newCoord = moveCoord (coord rover) roverGrid dir
	newVisited = Set.insert (coord2Int newCoord roverGrid) (visited rover)
	newRemaining = gSize roverGrid - Set.size newVisited

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

traverse :: Int -> Int -> StdGen -> Int
-- Takes an x and y bounds for the grid and a generator
-- Returns the number of steps it took the rover to traverse the grid
traverse x y gen = finalSteps where
	rover = makeRover x y
	directions = randomDirs gen
	roverStates = scanl (flip moveRover) rover directions 
	finalSteps = length $ takeWhile (\x -> nRemaining x > 0) roverStates



coord2Int :: Coordinate -> Grid -> Int
coord2Int coord grid = (x coord) + (y coord) * (gridX grid)

-- Test Functions Below (yay TDD)

allTests :: Bool
allTests = and [testMoveCoord, testCoord2Int, testMoveRover, testRandomDirs]

testMoveCoord :: Bool
testMoveCoord = and [t1, t2, t3, t4] where
	t1 = (nc == Coordinate 0 1) where
		testc = Coordinate 0 0 
		testg = makeGrid 5 5
		nc = moveCoord testc testg U
	t2 = (nc == Coordinate 1 5) where
		testc = Coordinate 2 5
		testg = makeGrid 7 7
		nc = moveCoord testc testg L
	t3 = (nc == Coordinate 0 0) where
		testc = Coordinate 4 0
		testg = makeGrid 5 5
		nc = moveCoord testc testg R
	t4 = (nc == Coordinate 3 6) where
		testc = Coordinate 3 0
		testg = makeGrid 7 7 
		nc = moveCoord testc testg D

testCoord2Int :: Bool
testCoord2Int = and [tc1, tc2, tc3, tc4] where
	tc1 = (idx == 0) where
		idx = coord2Int (Coordinate 0 0) (makeGrid 5 5)
	tc2 = (idx == 5) where
		idx = coord2Int (Coordinate 5 0) (makeGrid 6 6)
	tc3 = (idx == 8) where
		idx = coord2Int (Coordinate 2 2) (makeGrid 3 3)
	tc4 = (idx == 4) where
		idx = coord2Int (Coordinate 0 1) (makeGrid 4 4)

testMoveRover :: Bool
testMoveRover = and [t1, t2] where
	t1 = moved == Rover (makeGrid 2 2) (Coordinate 0 0) fullset 0 where
		fullset = Set.fromList [0..3]
		moved = foldr moveRover (makeRover 2 2) [U, R, D, L] 
	t2 = (nRemaining moved == 6) && (visited moved == Set.fromList [0,1,2]) where
		moved = foldr moveRover (makeRover 3 3) [R, R, R, L, L, R, R, L]

testRandomDirs = take 5 (randomDirs (mkStdGen 0)) == [R, L, R, U, D]

testTraversal = and [t1, t2, t3] where
	t1 = traverse 2 2 mkStdGen 100 == 11
	t2 = traverse 2 2 mkStdGen 23  == 3
	t3 = traverse 3 3 mkStdGen 7   == 9
	-- Tested these by hand...


--move :: Grid -> Coordinate -> Move -> Coordinate
--move (Grid xmax ymax) (Coordinate x y) mv = Coordinate nx ny where
--	(mx, my) = case mv of 
--		U -> (x, y+1)
--		D -> (x, y-1)
--		L -> (x-1, y)
--		R -> (x+1, y)
--	nx = (mx + xmax) `mod` xmax
--	ny = (my + ymax) `mod` ymax