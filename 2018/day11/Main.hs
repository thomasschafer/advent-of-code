module Main (main) where

import Data.List (intercalate)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)

-- When you start part 2, uncomment the next line.
-- print (part2 input)

powerLevel :: Int -> Int -> Int -> Int
powerLevel x y serialNum = (((rackId * y + serialNum) * rackId `div`) 100 `mod` 10) - 5
  where
    rackId = x + 10

part1 :: String -> String
part1 s = intercalate "," . (\(x, y) -> map show [x, y]) . snd $ maximum squares
  where
    boxSize = 3
    maxCoord = 300
    serialNum = read s

    grid = HM.fromList [((x, y), powerLevel x y serialNum) | x <- [1 .. maxCoord], y <- [1 .. maxCoord]]
    squares = [(calc x y, (x, y)) | x <- [1 .. maxCoord - boxSize + 1], y <- [1 .. maxCoord - boxSize + 1]]
      where
        calc xTop yTop = sum [fromJust $ HM.lookup (x, y) grid | x <- [xTop .. xTop + boxSize - 1], y <- [yTop .. yTop + boxSize - 1]]

part2 :: String -> Int
part2 = undefined
