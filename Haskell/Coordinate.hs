module Coordinate
( Grid (gSize)
, Coordinate (..)
, makeGrid
, squareGrid
, startCoord
, moveCoord
, coord2Int
, testCoordinateModule
) where

import Direction

data Grid = Grid { gridX :: Int, gridY :: Int, gSize :: Int } deriving (Show, Eq)
-- x and y bounds of the grid, and number of spaces in the grid

data Coordinate = Coordinate {x :: Int, y :: Int} deriving (Show, Eq)

makeGrid :: Int -> Int -> Grid
makeGrid x y = Grid x y (x*y)

squareGrid :: Int -> Grid
squareGrid x = Grid x x (x*x)

startCoord :: Coordinate
startCoord = Coordinate 0 0

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

	newBX = newX `mod` (gridX theGrid)
	newBY = newY `mod` (gridY theGrid)

coord2Int :: Coordinate -> Grid -> Int
coord2Int coord grid = (x coord) + (y coord) * (gridX grid)


testCoordinateModule :: Bool
testCoordinateModule = and [testMoveCoord, testCoord2Int]

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