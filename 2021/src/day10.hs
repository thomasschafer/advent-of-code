module Day10 (day10Main) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (sort)
import Data.Maybe (fromJust, isJust, mapMaybe)

charMapping :: HashMap Char Char
charMapping = HM.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

charScores :: HashMap Char Int
charScores = HM.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

lineToIllegalCharScore :: [Char] -> Maybe Int
lineToIllegalCharScore = lineToIllegalCharScoreHelper []
  where
    lineToIllegalCharScoreHelper :: [Char] -> [Char] -> Maybe Int
    lineToIllegalCharScoreHelper _ [] = Nothing -- We can ignore incomplete lines for now
    lineToIllegalCharScoreHelper charStack (nextChar : remainder)
      | isJust (HM.lookup nextChar charMapping) = lineToIllegalCharScoreHelper (nextChar : charStack) remainder -- Opening bracket
      | fromJust (HM.lookup (head charStack) charMapping) /= nextChar = HM.lookup nextChar charScores -- Invalid closing bracket
      | otherwise = lineToIllegalCharScoreHelper (tail charStack) remainder -- Valid closing bracket

solvePart1 :: String -> Int
solvePart1 navData = sum illegalCharScores
  where
    illegalCharScores = mapMaybe lineToIllegalCharScore $ lines navData

medianOddList :: [Int] -> Int
medianOddList xs = sort xs !! (length xs `div` 2)

completionScores :: HashMap Char Int
completionScores = HM.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

lineToCompletionScore :: [Char] -> Maybe Int
lineToCompletionScore = lineToCompletionScoreHelper []
  where
    remainingStringScore :: [Char] -> Int
    remainingStringScore = remainingStringScoreHelper 0
      where
        remainingStringScoreHelper :: Int -> [Char] -> Int
        remainingStringScoreHelper acc [] = acc
        remainingStringScoreHelper acc (char : remainder) = remainingStringScoreHelper (5 * acc + fromJust (HM.lookup char completionScores)) remainder

    lineToCompletionScoreHelper :: [Char] -> [Char] -> Maybe Int
    lineToCompletionScoreHelper charStack [] = Just $ remainingStringScore $ map (fromJust . (`HM.lookup` charMapping)) charStack
    lineToCompletionScoreHelper charStack (nextChar : remainder)
      | isJust (HM.lookup nextChar charMapping) = lineToCompletionScoreHelper (nextChar : charStack) remainder -- Opening bracket
      | fromJust (HM.lookup (head charStack) charMapping) /= nextChar = Nothing
      | otherwise = lineToCompletionScoreHelper (tail charStack) remainder -- Valid closing bracket

solvePart2 :: String -> Int
solvePart2 navData = medianOddList completionStringScores
  where
    completionStringScores = mapMaybe lineToCompletionScore $ lines navData

day10Main :: IO ()
day10Main = do
  testData <- readFile "data/day_10_test.txt"
  realData <- readFile "data/day_10.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
  print $ solvePart2 testData
  print $ solvePart2 realData
