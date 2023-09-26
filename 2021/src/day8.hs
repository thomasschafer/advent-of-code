module Day8 (day8Main) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (quickTrace, splitBy)

digitToSegmentNum :: HashMap Int Int
digitToSegmentNum = HM.fromList [(0, 6), (1, 2), (2, 5), (3, 5), (4, 4), (5, 5), (6, 6), (7, 3), (8, 7), (9, 6)]

uniqueDigitNumSegs :: Set Int
uniqueDigitNumSegs = S.fromList $ mapMaybe (`HM.lookup` digitToSegmentNum) [1, 4, 7, 8]

solvePart1 :: String -> Int
solvePart1 signalData = length valuesWithUniqueSegs
  where
    outputValues = map (words . (!! 1) . splitBy '|') $ lines signalData
    valuesWithUniqueSegs = filter ((`S.member` uniqueDigitNumSegs) . length) $ concat outputValues

day8Main :: IO ()
day8Main = do
  testData <- readFile "data/day_8_test.txt"
  realData <- readFile "data/day_8.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData

-- print $ solvePart2 testData
-- print $ solvePart2 realData
