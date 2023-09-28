module Day8 (day8Main) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (splitBy)

digitToSegmentNum :: HashMap Int Int
digitToSegmentNum = HM.fromList [(0, 6), (1, 2), (2, 5), (3, 5), (4, 4), (5, 5), (6, 6), (7, 3), (8, 7), (9, 6)]

uniqueDigitNumSegs :: Set Int
uniqueDigitNumSegs = S.fromList $ mapMaybe (`HM.lookup` digitToSegmentNum) [1, 4, 7, 8]

solvePart1 :: String -> Int
solvePart1 signalData = length valuesWithUniqueSegs
  where
    outputValues = map (words . (!! 1) . splitBy '|') $ lines signalData
    valuesWithUniqueSegs = filter ((`S.member` uniqueDigitNumSegs) . length) $ concat outputValues

listIntersection :: (Ord a) => [Set a] -> Set a
listIntersection xs = foldl S.intersection (head xs) xs

listUnion :: (Ord a) => [Set a] -> Set a
listUnion xs = foldl S.union (head xs) xs

toNum :: [Int] -> Int
toNum = toNumHelper 0
  where
    toNumHelper acc (x : xs) = toNumHelper (10 * acc + x) xs
    toNumHelper acc [] = acc

solvePart2 :: String -> Int
solvePart2 signalData = sum outputValues
  where
    scrambledData = map (map (map S.fromList . words) . splitBy '|') $ lines signalData

    determineOutputVals :: [[Set Char]] -> Maybe [Int]
    determineOutputVals [signalPatterns, outputPatterns] = Just $ mapMaybe charsToNum outputPatterns
      where
        one = head $ filter ((== 2) . length) signalPatterns
        seven = head $ filter ((== 3) . length) signalPatterns
        eight = head $ filter ((== 7) . length) signalPatterns
        twoThreeFive = filter ((== 5) . length) signalPatterns
        zeroSixNine = filter ((== 6) . length) signalPatterns

        top = S.findMin $ (seven `S.intersection` eight) `S.difference` one
        bottom = S.findMin $ S.delete top $ listIntersection (zeroSixNine ++ twoThreeFive)
        bottomRight = S.findMin $ one `S.intersection` listIntersection zeroSixNine
        topRight = S.findMin $ S.delete bottomRight one
        five = head $ filter (notElem topRight) twoThreeFive
        twoThree = filter (/= five) twoThreeFive
        topLeft = S.findMin $ five `S.difference` listUnion twoThree
        middle = S.findMin $ S.delete bottom $ S.delete top $ listIntersection twoThreeFive
        bottomLeft = S.findMin $ foldr S.delete eight [top, bottom, bottomRight, topRight, topLeft, middle]

        charsToNum :: Set Char -> Maybe Int
        charsToNum chars = case length chars of
          7 -> Just 8
          6 -> Just $ if middle `notElem` chars then 0 else if topRight `notElem` chars then 6 else 9
          5 -> Just $ if bottomLeft `elem` chars then 2 else if topRight `elem` chars then 3 else 5
          4 -> Just 4
          3 -> Just 7
          2 -> Just 1
          _ -> Nothing
    determineOutputVals _ = Nothing

    outputValues = map toNum $ mapMaybe determineOutputVals scrambledData

day8Main :: IO ()
day8Main = do
  testData <- readFile "data/day_8_test.txt"
  realData <- readFile "data/day_8.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
  singleData <- readFile "data/day_8_single.txt"
  print $ solvePart2 testData
  print $ solvePart2 singleData
  print $ solvePart2 realData
