module Day9 (day9Main) where

import Data.Char (digitToInt)

solvePart1 :: String -> Int
solvePart1 heightData = sum $ map (+ 1) lowPoints
  where
    heightMap = map (map digitToInt) $ lines heightData :: [[Int]]
    rows = length heightMap
    cols = length (head heightMap)
    neighbors i j =
      [ heightMap !! r !! c
        | (r, c) <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)],
          r >= 0,
          r <= rows - 1,
          c >= 0,
          c <= cols - 1
      ]
    lowPoints =
      [ heightMap !! i !! j
        | i <- [0 .. rows - 1],
          j <- [0 .. cols - 1],
          (heightMap !! i !! j) < minimum (neighbors i j)
      ]

day9Main :: IO ()
day9Main = do
  testData <- readFile "data/day_9_test.txt"
  realData <- readFile "data/day_9.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData

-- print $ solvePart2 testData
-- print $ solvePart2 realData
