import Prelude
import Data.Text as T
import Data.List as L

groupDelim = T.pack "\n\n"

main :: IO ()
main = do
  input <- readFile "./1.txt"
  let elves = Prelude.map sum $ parse input 
  print $ Prelude.maximum  elves
  print $ Prelude.sum $ Prelude.take 3 $ L.reverse $ L.sort elves


parse :: String -> [[Int]]
parse x = Prelude.map (fmap (read . T.unpack) . T.lines ) groups
  where
    tt = T.pack x
    groups = T.splitOn groupDelim tt





