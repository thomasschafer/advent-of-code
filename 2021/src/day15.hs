module Day15 (day15Main) where

import Data.Array
import Data.Char (digitToInt)
import Data.PQueue.Min (MinQueue)
import Data.PQueue.Min qualified as MQ

type WeightedCoord = (Float, (Int, Int))

type PathRiskLevels = Array (Int, Int) Float

infinity :: Float
infinity = 1 / 0

listToArray2d :: [[a]] -> Array (Int, Int) a
listToArray2d xs =
  array
    ((0, 0), (numRows - 1, numCols - 1))
    [((i, j), xs !! i !! j) | i <- [0 .. numRows - 1], j <- [0 .. numCols - 1]]
  where
    numRows = length xs
    numCols = if numRows == 0 then 0 else length $ head xs

-- Uses A* search algorithm
findLowestRiskPath :: Array (Int, Int) Int -> Maybe Float
findLowestRiskPath riskLevels = result
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds riskLevels
    numRows = maxRow - minRow + 1
    numCols = maxCol - minCol + 1

    initialPathRisks = listToArray2d [[initialWeight i j | i <- [0 .. numRows]] | j <- [0 .. numCols]]
      where
        initialWeight :: Int -> Int -> Float
        initialWeight i j
          | (i, j) == (0, 0) = 0
          | otherwise = infinity

    heurPathRisk :: (Int, Int) -> Float
    heurPathRisk (i, j) = sqrt ((iGoal - i') ** 2 + (jGoal - j') ** 2)
      where
        (i', j') = (fromIntegral i, fromIntegral j)
        (iGoal, jGoal) = (fromIntegral (numRows - 1), fromIntegral (numCols - 1))

    initialBestGuess :: Int -> Int -> Float
    initialBestGuess i j
      | (i, j) == (0, 0) = heurPathRisk (i, j)
      | otherwise = infinity
    initialQueue = MQ.singleton (initialBestGuess 0 0, (0, 0))
    initialBestGuesses = listToArray2d [[initialBestGuess i j | i <- [0 .. numRows]] | j <- [0 .. numCols]]

    result = updatePathRisks initialPathRisks initialBestGuesses initialQueue
      where
        updatePathRisks :: PathRiskLevels -> PathRiskLevels -> MinQueue WeightedCoord -> Maybe Float
        updatePathRisks pathRisks bestGuesses queue
          | MQ.null queue = Nothing
          | (i, j) == (numRows - 1, numCols - 1) = Just $ updatedPathRisks ! (i, j) -- todo: is this correct?
          | otherwise = updatePathRisks updatedPathRisks updatedBestGuesses updatedQueue
          where
            ((_, current@(i, j)), queueWithoutMin) = MQ.deleteFindMin queue
            neighbors =
              [ (r, c)
                | (r, c) <- [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)],
                  r >= 0,
                  r < numRows,
                  c >= 0,
                  c < numCols
              ]
            (updatedPathRisks, updatedBestGuesses, updatedQueue) = foldl updateQueue (pathRisks, bestGuesses, queueWithoutMin) neighbors
              where
                updateQueue :: (PathRiskLevels, PathRiskLevels, MinQueue WeightedCoord) -> (Int, Int) -> (PathRiskLevels, PathRiskLevels, MinQueue WeightedCoord)
                updateQueue (prevPathRisks, prevBestGuesses, prevQueue) neighbor =
                  let tentativePathRisk = (prevPathRisks ! current) + fromIntegral (riskLevels ! neighbor)
                   in if tentativePathRisk < (prevPathRisks ! neighbor)
                        then
                          let newPathRisks = prevPathRisks // [(neighbor, tentativePathRisk)]
                              newBestGuess = tentativePathRisk + heurPathRisk neighbor
                              newBestGuesses = prevBestGuesses // [(neighbor, newBestGuess)]
                              newQueue = MQ.insert (newBestGuess, neighbor) $ MQ.filter ((/= neighbor) . snd) prevQueue
                           in (newPathRisks, newBestGuesses, newQueue)
                        else (prevPathRisks, prevBestGuesses, prevQueue)

solvePart1 :: String -> Maybe Float
solvePart1 riskData = findLowestRiskPath riskLevels
  where
    riskLevelsList = map (map digitToInt) $ lines riskData
    riskLevels = listToArray2d riskLevelsList

solvePart2 :: String -> Maybe Float
solvePart2 riskData = findLowestRiskPath riskLevels
  where
    riskLevelsList = map (map digitToInt) $ lines riskData
    riskLevelsTiled =
      [ [ wrap $ riskLevelsList !! (i `mod` numRows) !! (j `mod` numCols) + (i `div` numRows) + (j `div` numCols)
          | j <- [0 .. numTiles * numCols - 1]
        ]
        | i <- [0 .. numTiles * numRows - 1]
      ]
      where
        wrap x = (x - 1) `mod` 9 + 1
        numTiles = 5
        numRows = length riskLevelsList
        numCols = length riskLevelsList
    riskLevels = listToArray2d riskLevelsTiled

day15Main :: IO ()
day15Main = do
  testData <- readFile "data/day_15_test.txt"
  realData <- readFile "data/day_15.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
  print $ solvePart2 testData
  print $ solvePart2 realData
