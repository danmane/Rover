import System.Random
import qualified Data.Set as S
 
randomWalk :: Int -> StdGen -> [(Int, Int)]
randomWalk w g = scanl move (0, 0) $ randomRs (0, 3) g
    where
        move :: (Int, Int) -> Int -> (Int, Int)
        move (x, y) n = case n of
            0 -> (x, (y + 1) `mod` w) -- up
            1 -> (x, (y - 1) `mod` w) -- down
            2 -> ((x - 1) `mod` w, y) -- left
            3 -> ((x + 1) `mod` w, y) -- right
 
-- length of a run with stdGen seeded with n on a grid of size w
runWalk :: Int -> Int -> Int
runWalk w n = length
            . takeWhile (not . S.null) 
            . scanl (flip S.delete) pointSet $ randomWalk w g
    where g = mkStdGen n
          pointSet = S.fromDistinctAscList [(i, j) | i <- [0..w - 1], j <- [0..w - 1]]
 
main = print . sum $ map (runWalk 20) [1..10000]