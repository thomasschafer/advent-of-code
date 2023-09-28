module Day11 (day11Main) where

import Data.Char (digitToInt)

takeStep :: Int -> Int -> [[Int]] -> [[Int]]
takeStep numRows numCols energyLevels = map (map (\x -> if x > 9 then 0 else x)) newEnergyLevels
  where
    incrementedEnergyLevels = map (map (+ 1)) energyLevels
    flashStack = [(i, j) | i <- [0 .. numRows - 1], j <- [0 .. numCols - 1], (incrementedEnergyLevels !! i !! j) > 9]
    newEnergyLevels = applyFlashes numRows numCols incrementedEnergyLevels flashStack

takeStepWithCounter :: Int -> Int -> ([[Int]], Int) -> ([[Int]], Int)
takeStepWithCounter numRows numCols (energyLevels, flashCount) = (newEnergyLevels, flashCount + newFlashes)
  where
    newEnergyLevels = takeStep numRows numCols energyLevels
    newFlashes = length $ concatMap (filter (== 0)) newEnergyLevels

applyFlashes :: Int -> Int -> [[Int]] -> [(Int, Int)] -> [[Int]]
applyFlashes _ _ prevEnergyLevels [] = prevEnergyLevels
applyFlashes numRows numCols prevEnergyLevels ((x, y) : flashStackRemainder) = applyFlashes numRows numCols newEnergyLevels (flashStackRemainder ++ newFlashes)
  where
    allNeighbors = [(x + xDelta, y + yDelta) | xDelta <- [-1 .. 1], yDelta <- [-1 .. 1], not (xDelta == 0 && yDelta == 0)]
    validNeighbors = [(i, j) | (i, j) <- allNeighbors, i >= 0, i < numRows, j >= 0, j < numRows]
    newFlashes = [(i, j) | (i, j) <- validNeighbors, prevEnergyLevels !! i !! j == 9]
    newEnergyLevels =
      [ [ (prevEnergyLevels !! i !! j) + if (i, j) `elem` validNeighbors then 1 else 0
          | j <- [0 .. numCols - 1]
        ]
        | i <- [0 .. numRows - 1]
      ]

solvePart1 :: String -> Int
solvePart1 energyLevelData = snd $ iterate (takeStepWithCounter numRows numCols) (startingEnergyLevels, 0) !! 100
  where
    startingEnergyLevels = map (map digitToInt) $ lines energyLevelData
    numRows = length startingEnergyLevels
    numCols = length $ head startingEnergyLevels

solvePart2 :: String -> Int
solvePart2 energyLevelData = findSimultaneousFlash startingEnergyLevels 1
  where
    startingEnergyLevels = map (map digitToInt) $ lines energyLevelData
    numRows = length startingEnergyLevels
    numCols = length $ head startingEnergyLevels

    findSimultaneousFlash :: [[Int]] -> Int -> Int
    findSimultaneousFlash energyLevels numSteps =
      if all (all (== 0)) newEnergyLevels then numSteps else findSimultaneousFlash newEnergyLevels (numSteps + 1)
      where
        newEnergyLevels = takeStep numRows numCols energyLevels

day11Main :: IO ()
day11Main = do
  testData <- readFile "data/day_11_test.txt"
  realData <- readFile "data/day_11.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
  print $ solvePart2 testData
  print $ solvePart2 realData
