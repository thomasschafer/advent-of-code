module Day06 (day06Main) where

import Data.Char (isSpace)
import Data.List.Split (splitOn)

type TimeDistPair = (Int, Int)

numWaysToWin :: TimeDistPair -> Int
numWaysToWin (time, distToBeat) = length . filter (> distToBeat) $ map (\t -> (time - t) * t) [0 .. time]

part1 :: String -> Int
part1 = product . map numWaysToWin . parseSeparateNums
  where
    parseSeparateNums = (\[a, b] -> zip a b) . map (map read . tail . words) . lines

part2 :: String -> Int
part2 = numWaysToWin . parseSingleNums
  where
    parseSingleNums = (\[a, b] -> (a, b)) . map (read . filter (not . isSpace) . last . splitOn ":") . lines

day06Main :: IO ()
day06Main = do
  testData <- readFile "data/day_6_test.txt"
  realData <- readFile "data/day_6.txt"
  print $ part1 testData
  print $ part1 realData
  print $ part2 testData
  print $ part2 realData
