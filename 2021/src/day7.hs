module Day7 (day7Main) where

import Utils (splitBy)

solveFuelRequired :: (Int -> Int -> Int) -> String -> Int
solveFuelRequired fuelFromDistance positionsData =
  foldl calcFuelSpent fuelUpperBound [minPos .. maxPos]
  where
    positions = map read $ splitBy ',' positionsData :: [Int]
    minPos = minimum positions
    maxPos = maximum positions
    fuelUpperBound = fuelFromDistance maxPos minPos * length positions

    calcFuelSpent minFuel pos = min minFuel curFuelRequired
      where
        curFuelRequired = sum $ map (fuelFromDistance pos) positions

distance :: (Num a) => a -> a -> a
distance = (abs .) . (-)

solvePart1 :: String -> Int
solvePart1 = solveFuelRequired distance

solvePart2 :: String -> Int
solvePart2 = solveFuelRequired $ (fuelReq .) . distance
  where
    fuelReq dist = dist * (dist + 1) `div` 2

day7Main :: IO ()
day7Main = do
  testData <- readFile "data/day_7_test.txt"
  realData <- readFile "data/day_7.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
  print $ solvePart2 testData
  print $ solvePart2 realData
