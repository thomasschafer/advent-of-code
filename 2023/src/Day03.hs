module Day03 (day03Main) where

import Data.Char (isDigit)
import Data.List (findIndex)
import Data.Maybe (fromJust, fromMaybe)
import Utils (quickTrace)

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && (c /= '.')

part1 :: String -> Int
part1 engineSchematicData = partNumSum 0 0 0
  where
    engineSchematic = lines engineSchematicData
    rows = length engineSchematic
    cols = length $ head engineSchematic

    hasSurroundingSymbol :: Int -> Int -> Int -> Bool
    hasSurroundingSymbol row colStart colEnd =
      colStart <= colEnd
        && any (isSymbol . (\(r, c) -> engineSchematic !! r !! c)) indicesToCheck
      where
        indicesToCheck =
          [ (r, c)
            | r <- [row - 1 .. row + 1],
              r >= 0,
              r < rows,
              c <- [colStart - 1 .. colEnd + 1],
              c >= 0,
              c < cols,
              not (r == row && c >= colStart && c <= colEnd)
          ]

    partNumSum :: Int -> Int -> Int -> Int
    partNumSum r c acc
      | r >= rows = acc
      | isDigit (engineSchematic !! r !! c) =
          let numEndCol = fromMaybe cols (findIndex (not . isDigit) (drop c (engineSchematic !! r) ++ ['.'])) + c - 1
              nextCol = (numEndCol + 1) `mod` cols
              nextRow = if nextCol == 0 then r + 1 else r
              num = read . take (numEndCol - c + 1) $ drop c (engineSchematic !! r)
              includeNum = hasSurroundingSymbol r c numEndCol
           in partNumSum nextRow nextCol (if includeNum then acc + num else acc)
      | otherwise = partNumSum (if c + 1 == cols then r + 1 else r) ((c + 1) `mod` cols) acc

day03Main :: IO ()
day03Main = do
  testData <- readFile "data/day_3_test.txt"
  realData <- readFile "data/day_3.txt"
  print $ part1 testData
  print $ part1 realData