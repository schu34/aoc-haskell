import Data.Char (isNumber)
import Data.Text as T (Text, lines, pack, splitOn, unpack)
import Prelude

comma :: Text
comma = pack ","

dash :: Text
dash = pack "-"

main :: IO ()
main = do
  input <- readFile "./4.txt"
  let parsed = parse $ pack input
  print $ p1 parsed
  print $ p2 parsed

p1 :: [[Int]] -> Int
p1 = length . filter contains

p2 :: [[Int]] -> Int
p2 = length . filter overlaps

contains :: [Int] -> Bool
contains [a, b, c, d] = (a <= c && b >= d) || (a >= c && b <= d)

overlaps :: [Int] -> Bool
overlaps [a, b, c, d] = (h1 >= l2 && h1 <= h2) || (h2 >= low1 && h2 <= h1)
  where
    low1 = min a b
    h1 = max a b
    l2 = min c d
    h2 = max c d

parse :: Text -> [[Int]]
parse = map parseLine . T.lines

parseLine :: Text -> [Int]
parseLine = map (read . unpack) . concatMap (splitOn dash) . splitOn comma
