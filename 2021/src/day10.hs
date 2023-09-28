module Day10 (day10Main) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (isJust, mapMaybe)

charMapping :: HashMap Char Char
charMapping = HM.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

charScores :: HashMap Char Int
charScores = HM.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

solvePart1 :: String -> Int
solvePart1 navData = sum illegalCharScores
  where
    lineToIllegalCharScore :: [Char] -> Maybe Int
    lineToIllegalCharScore = lineToIllegalCharScoreHelper []
      where
        lineToIllegalCharScoreHelper :: [Char] -> [Char] -> Maybe Int
        lineToIllegalCharScoreHelper _ [] = Nothing -- We can ignore incomplete lines for now
        lineToIllegalCharScoreHelper charStack (nextChar : remainder)
          | isJust (HM.lookup nextChar charMapping) = lineToIllegalCharScoreHelper (nextChar : charStack) remainder -- Opening bracket
          | HM.lookup (head charStack) charMapping /= Just nextChar = HM.lookup nextChar charScores -- Invalid closing bracket
          | otherwise = lineToIllegalCharScoreHelper (tail charStack) remainder -- Valid closing bracket
    illegalCharScores = mapMaybe lineToIllegalCharScore $ lines navData

day10Main :: IO ()
day10Main = do
  testData <- readFile "data/day_10_test.txt"
  realData <- readFile "data/day_10.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData

-- print $ solvePart2 testData
-- print $ solvePart2 realData
