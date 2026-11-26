module Main (main) where

import Control.Arrow ((&&&))
import Data.HashMap.Lazy qualified as HM
import Data.List (intercalate)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

powerLevel :: Int -> Int -> Int -> Int
powerLevel serialNum x y = (((rackId * y + serialNum) * rackId `div` 100) `mod` 10) - 5
  where
    rackId = x + 10

solve :: Bool -> [Int] -> String -> String
solve includeSize boxSizes s = intercalate "," . (\(x, y, boxSize) -> map show ([x, y] ++ if includeSize then [boxSize] else [])) . snd $ maximum squares
  where
    maxCoord = 300
    serialNum = read s

    coords = [(x, y) | x <- [1 .. maxCoord], y <- [1 .. maxCoord]]
    powerLevels = HM.fromList $ map (id &&& uncurry (powerLevel serialNum)) coords
    sumsFromTopRight = HM.fromList $ map (id &&& uncurry calc) coords
      where
        calc xb yb = get (xb - 1) yb + get xb (yb - 1) - get (xb - 1) (yb - 1) + fromJust (HM.lookup (xb, yb) powerLevels)

    get = curry $ flip (HM.findWithDefault 0) sumsFromTopRight

    squares = [(calc (x, y) (x + boxSize - 1, y + boxSize - 1), (x, y, boxSize)) | boxSize <- boxSizes, x <- [1 .. maxCoord - boxSize + 1], y <- [1 .. maxCoord - boxSize + 1]]
      where
        calc (xt, yt) (xb, yb) = get xb yb - get xb (yt - 1) - get (xt - 1) yb + get (xt - 1) (yt - 1)

part1 :: String -> String
part1 = solve False [3]

part2 :: String -> String
part2 = solve True [1 .. 300]
