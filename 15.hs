import Data.Char (isNumber)
import Data.List (sortBy, sortOn)
import Data.Map as Map
import Prelude

type Coord = (Int, Int)
type Interval = (Int, Int)
type CoordPair = (Coord, Coord)
type IntervalMap = Map Int [Interval]

main :: IO ()
main = do
  input <-getContents
  let parsed = Prelude.map parseLine $ lines input
  print parsed
  let m = getMap parsed
  print m
  print $ m ! 10
  print $ getCoveredSquares $ m ! 10

{-
>>> parseLine  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
((2,18),(-2,15))
 -}
parseLine :: String -> CoordPair
parseLine x = readToTuples trimmedCoords
 where
  coordParts = Prelude.filter isCoord $ words x
  trimmedCoords = Prelude.map (Prelude.filter isNum) coordParts

--- >>> getMap [((2,18), (-2,15)), ((9,16),(10,16))]
-- fromList [(-5,[(34,2)]),(-4,[(33,3)]),(-3,[(32,4)]),(-2,[(31,5)]),(-1,[(30,6)]),(0,[(29,7)]),(1,[(28,8)]),(2,[(27,9)]),(3,[(26,10)]),(4,[(25,11)]),(5,[(24,12)]),(6,[(23,13)]),(7,[(22,14)]),(8,[(21,15)]),(9,[(20,16)]),(10,[(21,11)])]
getMap :: [CoordPair] -> IntervalMap
getMap cps = Prelude.foldl (unionWith mergeLists) Map.empty intervals
 where
  intervals = Prelude.map getIntervals cps

isNum x
  | isNumber x = True
  | x == '-' = True
  | otherwise = False

isCoord (x : xs)
  | x == 'x' = True
  | x == 'y' = True
  | otherwise = False

manhattan :: Num a => ((a, a), (a, a)) -> a
manhattan ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

-- >>> getIntervals ( (-2,2),(0,0)) 
-- fromList [(-2,[(-2,-2)]),(-1,[(-3,-1)]),(0,[(-4,0)]),(1,[(-5,1)]),(2,[(-6,2)]),(3,[(-5,1)]),(4,[(-4,0)]),(5,[(-3,-1)]),(6,[(-2,-2)])]
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

-- >>> mergeLists [(1,2), (1,3), (1,4), (2,5), (3,6)] [  (5, 15), (12, 17)]
-- [(1,17)]
mergeLists :: [Interval] -> [Interval] -> [Interval]
mergeLists a b = mergeSorted sorted
 where
  sorted = sortOn fst a ++ b
  mergeSorted :: [Interval] -> [Interval]
  mergeSorted [x] = [x]
  mergeSorted ((s1, e1) : (s2, e2) : xs)
    | e1 >= e2 = mergeSorted $ (s1, e1) : xs -- interval1 contains interval 2
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

