module Direction 
( Direction (..)
, randomDirs
, testDirectionModule
) where

import System.Random

data Direction = U | D | L | R deriving (Show, Eq)

randomDirs :: StdGen -> [Direction]
randomDirs gen = map i2d $ randomRs (1,4) gen where
	i2d :: Int -> Direction
	i2d x = case x of
		1 -> U
		2 -> R
		3 -> D
		4 -> L

testDirectionModule :: Bool
testDirectionModule = take 5 (randomDirs (mkStdGen 0)) == [R, L, R, U, D]
