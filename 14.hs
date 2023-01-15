import Data.List as List
import Data.Set as Set
import Data.Text as T
import Data.Maybe as Maybe
import Debug.Trace (trace)

arrowSep :: Text
arrowSep = T.pack " -> "

comma :: Text
comma = T.pack ","

-- main :: IO ()
-- main = interact $ solve . parseLines

maybe2bool:: Maybe a -> Bool
maybe2bool Nothing = False
maybe2bool (Just _) = True

main :: IO ()
main = do
  input <- getContents
  let map = Maybe.fromJust $ parseLines input
  let bounds = setBounds map
  let floor = snd (snd $ setBounds map) + 2
  let ns = List.takeWhile maybe2bool $ iterate (getNextState floor) $ Just map
  let x = List.map (printVis <$>) ns
  print $ List.length ns - 1

parseLines :: String -> Maybe(Set (Int, Int))
parseLines x = Just $ Set.unions $ Prelude.map parseOneLine $ Prelude.lines x

parseOneLine :: String -> Set (Int, Int)
parseOneLine line = Set.fromList fullPointsLists
  where
    pointStrings = T.splitOn arrowSep $ T.pack line
    pointTuples = Prelude.map (readTuple . breakOnComma) pointStrings
    fullPointsLists = Prelude.concatMap tupleRange $ byTwos pointTuples

solve :: Set (Int, Int) -> String
solve x = ""

-- utils
byTwos :: [b] -> [(b, b)]
byTwos (f : s : rest) = (f, s) : byTwos (s : rest)
byTwos [f] = []
byTwos [] = []

readTuple :: (Text, Text) -> (Int, Int)
readTuple (x, y) = (read $ T.unpack x :: Int, read $ T.unpack y :: Int)

unReadTuple :: (Int, Int) -> (Text, Text)
unReadTuple (x, y) = (T.pack $ show x, T.pack $ show y)

tupleRange :: (Ord a, Enum a, Num a) => ((a, a), (a, a)) -> [(a, a)]
tupleRange ((x1, y1), (x2, y2))
  | x1 == x2 = Prelude.zip (repeat x1) $ range y1 y2
  | y1 == y2 = Prelude.zip (range x1 x2) (repeat y1)

range :: (Ord a, Enum a, Num a) => a -> a -> [a]
range s e
  | s < e = [s .. e]
  | otherwise = [s, s - 1 .. e]

breakOnComma :: Text -> (Text, Text)
breakOnComma x = (a, T.drop 1 b)
  where
    (a, b) = T.breakOn comma x

getNextState ::Int -> Maybe(Set (Int, Int)) -> Maybe(Set (Int, Int))
getNextState _ Nothing = Nothing
getNextState floor (Just x) = loop (500, 0)
  where
    ((_, _), (_, maxY)) = setBounds x
    loop :: (Int, Int) -> Maybe (Set (Int, Int) )
    loop (sX, sY)
      | Set.member (500, 0) x = Nothing
      | Set.member (sX, sY) x = Just x
      | sY + 1 == floor = Just $ Set.insert ( sX,sY ) x
      | otherwise =
          let candidates = [(sX, sY + 1), (sX - 1, sY + 1), (sX + 1, sY + 1)]
           in case List.find (not . (`Set.member` x)) candidates of
                Just nextSpace -> loop nextSpace
                _ -> Just $ Set.insert (sX, sY) x

-- testing utils (not needed for the actual solution )

makeFlatPlatform :: Int -> Int -> Int -> Set (Int, Int)
makeFlatPlatform y startX endX = Set.fromList $ tupleRange ((startX, y), (endX, y))

window :: Int -> Int -> [a] -> [a]
window start width = List.take width . List.drop start


toArray :: Set (Int, Int) -> [[Char]]
toArray grid = [[ ch (x,y) | x <- [lowX .. highX]] | y <- [lowY .. highY]]
  where
    ch x
      | Set.member x grid = '#'
      | otherwise = '.'
    ((lowX, highX),(lowY,highY)) = setBounds grid

setBounds ::Ord a => Set (a,a)->((a,a), (a,a))
setBounds x = ((lowX, highX),(lowY,highY))
  where
    lowX = Set.findMin $ Set.map fst x
    lowY = Set.findMin $ Set.map snd x
    highX = Set.findMax $ Set.map fst x
    highY = Set.findMax $ Set.map snd x


printVis :: Set (Int, Int) -> IO ()
printVis = putStr . List.unlines . toArray

{-
>>>x = parseLines "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
>>>let ns = List.takeWhile maybe2bool $ iterate getNextState x
>>>List.length ns -1
24
-}


{-
>>> mapped = fmap setBounds
>>> setBounds <$> (Just $ Set.fromList [(11,1)])
Just ((11,11),(1,1))

-}
