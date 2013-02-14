import Rover
import Coordinate
import Direction
import Data.List

modTests :: [(Bool, String)]
modTests = [(testRoverModule, "Rover Module")
		   ,(testCoordinateModule, "Coordinate Module")
		   ,(testDirectionModule, "Direction Module")
		   ]

allTests :: [(Bool, String)]
allTests = modTests ++ [(and $ map fst modTests, "All")]

report :: (Bool, String) -> String
report (p, name)
	| p = "Yay! " ++ name ++ " tests passed!"
	| otherwise = "Oh no! " ++ name ++ " tests failed!"

main :: IO()
main = putStrLn $ intercalate "\n" $ map report allTests