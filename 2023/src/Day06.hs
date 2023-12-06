module Day06 (day06Main) where

import Data.List.Split (splitOn)

parseData :: String -> [(Int, Int)] -- [(time, distance)]
parseData = (\[a, b] -> zip a b) . map (map read . filter (/= "") . tail . splitOn " ") . lines

part1 :: String -> Int
part1 = product . map numWaysToWin . parseData
  where
    numWaysToWin (time, distToBeat) =
      length . filter (> distToBeat) $ map (\t -> (time - t) * t) [0 .. time]

day06Main :: IO ()
day06Main = do
  testData <- readFile "data/day_6_test.txt"
  realData <- readFile "data/day_6.txt"
  print $ part1 testData
  print $ part1 realData
