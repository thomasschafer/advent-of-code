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

solvePart1 :: String -> Float
solvePart1 riskData = finalPathRisks ! (numRows - 1, numCols - 1)
  where
    riskLevelsList = map (map digitToInt) $ lines riskData
    numRows = length riskLevelsList
    numCols = length $ head riskLevelsList
    riskLevels = listToArray2d riskLevelsList

    initialWeight i j = if (i, j) == (0, 0) then 0 else infinity
    initialQueue = MQ.fromList [(initialWeight i j, (i, j)) | i <- [0 .. numRows], j <- [0 .. numCols]]
    initialPathRisks = listToArray2d [[initialWeight i j | i <- [0 .. numRows]] | j <- [0 .. numCols]]

    (finalPathRisks, _) = updatePathRisks initialPathRisks initialQueue
      where
        updatePathRisks :: PathRiskLevels -> MinQueue WeightedCoord -> (PathRiskLevels, MinQueue WeightedCoord)
        updatePathRisks pathRisks queue =
          if MQ.null queue
            then (pathRisks, queue)
            else updatePathRisks updatedPathRisks updatedQueue
          where
            ((_, (i, j)), queueWithoutMin) = MQ.deleteFindMin queue
            neighbors =
              [ (r, c)
                | (r, c) <- [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)],
                  r >= 0,
                  r < numRows,
                  c >= 0,
                  c < numCols
              ]
            (neighborsQueue, remainingQueue) = MQ.partition ((`elem` neighbors) . snd) queueWithoutMin
            (updatedPathRisks, updatedQueue) = MQ.foldlU updateQueue (pathRisks, remainingQueue) neighborsQueue
              where
                updateQueue :: (PathRiskLevels, MinQueue WeightedCoord) -> WeightedCoord -> (PathRiskLevels, MinQueue WeightedCoord)
                updateQueue (prevPathRisks, prevQueue) (_, (iNeigh, jNeigh)) = (newPathRisks, newQueue)
                  where
                    newPathRisk =
                      min
                        (prevPathRisks ! (iNeigh, jNeigh))
                        ((prevPathRisks ! (i, j)) + fromIntegral (riskLevels ! (iNeigh, jNeigh)))
                    newPathRisks = prevPathRisks // [((iNeigh, jNeigh), newPathRisk)]
                    newQueue = MQ.insert (newPathRisk, (iNeigh, jNeigh)) prevQueue

day15Main :: IO ()
day15Main = do
  testData <- readFile "data/day_15_test.txt"
  realData <- readFile "data/day_15.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData

-- print $ solvePart2 testData
-- print $ solvePart2 realData
