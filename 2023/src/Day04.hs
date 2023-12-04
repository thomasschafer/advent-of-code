module Day04 (day04Main) where

import Data.Set (Set)
import Data.Set qualified as S
import Utils (splitBy)

data CardResult = CardResult {cardNum :: Int, numWinningCards :: Int}

parseNums :: String -> CardResult
parseNums line = CardResult {cardNum, numWinningCards}
  where
    [cardStr, nums] = splitBy ':' line
    cardNum = read $ splitBy ' ' cardStr !! 1
    [winningNums, chosenNums] =
      map (S.fromList . map read . splitBy ' ') $ splitBy '|' nums :: [Set Int]
    numWinningCards = length (S.intersection winningNums chosenNums)

part1 :: String -> Int
part1 = sum . map (cardsToPoints . numWinningCards . parseNums) . lines
  where
    cardsToPoints n = if n == 0 then 0 else 2 ^ (n - 1)

day04Main :: IO ()
day04Main = do
  testData <- readFile "data/day_4_test.txt"
  realData <- readFile "data/day_4.txt"
  print $ part1 testData
  print $ part1 realData