import Data.Char (isNumber)
import Data.List (sortBy, sortOn)
import Data.Map as Map
import Prelude

type Coord = (Int, Int)
type Interval = (Int, Int)
type CoordPair = (Coord, Coord)
type IntervalMap = Map Int [Interval]

-- >>> main
main :: IO ()
main = do
  input <- readFile "./15.txt"
  let parsed = Prelude.map parseLine $ lines input
  -- putStr $ unlines $ Prelude.map show parsed
  let m = getMap parsed
  print "answer:\n"
  print $ getCoveredSquares $ m ! 200000 

{-
>>> parseLine  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
((2,18),(-2,15))
 -}
parseLine :: String -> CoordPair
parseLine x = readToTuples trimmedCoords
 where
  coordParts = Prelude.filter isCoord $ words x
  trimmedCoords = Prelude.map (Prelude.filter isNum) coordParts

--- >>> getMap [((12,14), (10,16)), ((8,7),(2,10))] ! 10
-- [(12,14)]
getMap :: [CoordPair] -> IntervalMap
getMap cps = unionsWith mergeLists intervals
 where
  intervals = Prelude.map getIntervals cps

isNum :: Char -> Bool
isNum x
  | isNumber x = True
  | x == '-' = True
  | otherwise = False

isCoord :: [Char] -> Bool
isCoord (x : xs)
  | x == 'x' = True
  | x == 'y' = True
  | otherwise = False

manhattan :: Num a => ((a, a), (a, a)) -> a
manhattan ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

-- >>> getIntervals ( (12,14),(10,16)) ! 10
-- [(12,12)]
-- >>> getIntervals ((8,7),(2,10)) ! 10
-- [(2,14)]
--
getIntervals :: CoordPair -> IntervalMap
getIntervals x =
  Prelude.foldl f empty [sensorY - mDistance .. sensorY + mDistance]
 where
  ((sensorX, sensorY), (beaconX, beaconY)) = x
  mDistance = manhattan x
  f m nextIndex = Map.insert nextIndex [(sensorX - (mDistance - dist), sensorX + (mDistance - dist))] m
   where
    dist = abs $ sensorY - nextIndex 

readToTuples :: [String] -> CoordPair
readToTuples strs = ((sensorX, sensorY), (beaconX, beaconY))
 where
  [sensorX, sensorY, beaconX, beaconY] = Prelude.map (\x -> read x :: Int) strs

-- >>> mergeLists [(12,12)] [(2,14)]
-- [(2,14)]
mergeLists :: [Interval] -> [Interval] -> [Interval]
mergeLists a b = mergeSorted sorted
 where
  sorted = sortOn fst $ a ++ b
  mergeSorted :: [Interval] -> [Interval]
  mergeSorted [x] = [x]
  mergeSorted ((s1, e1) : (s2, e2) : xs)
    | e1 >= e2 = mergeSorted $ (s1, e1) : xs -- interval1 contains interval2
    | e1 >= s2 = mergeSorted $ (s1, e2) : xs
    | otherwise = (s1, e1) : mergeSorted ((s2, e2) : xs)

getCoveredSquares :: [Interval] -> Int
getCoveredSquares = Prelude.foldl (\x (s, e) -> x + abs (s - e)) 0

{-
>>>line = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
>>> coordParts = Prelude.filter isCoord $ words line
>>> arr =  Prelude.map (Prelude.filter isNum) coordParts
>>> readToTuples arr
((2,18),(-2,15))
-}

