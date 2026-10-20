module Main (main) where

import Data.Char (isSpace)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

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
