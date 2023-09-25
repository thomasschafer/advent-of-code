module Day7 (day7Main) where

import Utils (splitBy)

solvePart1 :: String -> Int
solvePart1 positionsData = foldl calcFuelSpent fuelUpperBound [minPos .. maxPos]
  where
    positions = map read $ splitBy ',' positionsData :: [Int]
    minPos = minimum positions
    maxPos = maximum positions
    fuelUpperBound = (maxPos - minPos) * length positions

    calcFuelSpent minFuel pos = min minFuel curFuelRequired
      where
        curFuelRequired = sum $ map (abs . (pos -)) positions

day7Main :: IO ()
day7Main = do
  testData <- readFile "data/day_7_test.txt"
  realData <- readFile "data/day_7.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
