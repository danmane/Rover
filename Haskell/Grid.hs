module Grid 
( Grid
, makeGrid,
, squareGrid
) where 

makeGrid :: Int -> Int -> Grid
makeGrid x y = Grid x y (x*y)

squareGrid :: Int -> Grid
squareGrid x = Grid x x (x*x)

data Grid = Grid { gridX :: Int, gridY :: Int, gSize :: Int } deriving (Show, Eq)
-- x and y bounds of the grid, and number of spaces in the grid