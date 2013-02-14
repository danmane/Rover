import Coordinate
import Rover
import System.Random
import Data.List

data2csv :: [Int] -> String
data2csv dat = concat $ intersperse "," $ map show dat

main :: IO()
main = putStrLn $ data2csv $ take 10000 $ parallelTraversals (squareGrid 20) (mkStdGen 100)
