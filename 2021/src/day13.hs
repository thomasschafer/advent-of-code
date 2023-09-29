module Day13 (day13Main) where

import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
import Utils (splitBy)

printGrid :: Set (Int, Int) -> Set (Int, Int)
printGrid coords = trace displayed coords
  where
    coordsList = S.toList coords
    numRows = maximum (map snd coordsList)
    numCols = maximum (map fst coordsList)
    displayed =
      ( concat
          [ concat
              [ if (x, y) `elem` coords then "#" else "."
                | x <- [0 .. numCols]
              ]
              ++ ['\n']
            | y <- [0 .. numRows]
          ]
      )
        ++ "\n"

applyFolds :: Maybe Int -> String -> Set (Int, Int)
applyFolds foldLimit dotData = foldl applyFold dotCoords foldsTruncated
  where
    (dots, folds) = break (== "") $ lines dotData
    parseCoords = (\l -> (head l, l !! 1)) . map read . splitBy ','
    dotCoords = S.fromList $ map parseCoords dots
    parseFolds = (\l -> (head l, read (l !! 1))) . splitBy '=' . drop 11
    foldsParsed = map parseFolds (tail folds)

    applyFold :: Set (Int, Int) -> (String, Int) -> Set (Int, Int)
    applyFold points (plane, pos) = foldl applyFoldToPoint S.empty points
      where
        applyFoldToPoint :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
        applyFoldToPoint curPoints (x, y) = S.insert foldedPoint curPoints
          where
            foldedPoint
              | plane == "x" && x > pos = (pos - (x - pos), y)
              | plane == "y" && y > pos = (x, pos - (y - pos))
              | otherwise = (x, y)

    foldsTruncated = case foldLimit of
      Just limit -> take limit foldsParsed
      _ -> foldsParsed

solvePart1 :: String -> Int
solvePart1 dotData = length $ applyFolds (Just 1) dotData

day13Main :: IO ()
day13Main = do
  testData <- readFile "data/day_13_test.txt"
  realData <- readFile "data/day_13.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
