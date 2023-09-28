module Day9 (day9Main) where

import Data.Char (digitToInt)
import Data.List (sort)
import Data.Set (Set)
import Data.Set qualified as S

parseHeightData :: String -> [[Int]]
parseHeightData = map (map digitToInt) . lines

neighbors :: [[Int]] -> Int -> Int -> [(Int, Int)]
neighbors heightMap i j =
  [ (r, c)
    | (r, c) <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)],
      r >= 0,
      r <= length heightMap - 1,
      c >= 0,
      c <= length (head heightMap) - 1
  ]

lowPointIndices :: [[Int]] -> [(Int, Int)]
lowPointIndices heightMap = lowPoints
  where
    rows = length heightMap
    cols = length (head heightMap)
    neighborHeights i j = map (\(r, c) -> heightMap !! r !! c) (neighbors heightMap i j)
    lowPoints =
      [ (i, j)
        | i <- [0 .. rows - 1],
          j <- [0 .. cols - 1],
          (heightMap !! i !! j) < minimum (neighborHeights i j)
      ]

solvePart1 :: String -> Int
solvePart1 heightData = sum $ map (+ 1) lowPoints
  where
    heightMap = parseHeightData heightData
    lowPoints = [heightMap !! i !! j | (i, j) <- lowPointIndices heightMap]

solvePart2 :: String -> Int
solvePart2 heightData = product threeLargestBasinSizes
  where
    heightMap = parseHeightData heightData

    calcBasinSize :: (Int, Int) -> Int
    calcBasinSize = fst . calcBasinSizeHelper S.empty
      where
        calcBasinSizeHelper :: Set (Int, Int) -> (Int, Int) -> (Int, Set (Int, Int))
        calcBasinSizeHelper visited (i, j) =
          if ((i, j) `elem` visited) || (heightMap !! i !! j == 9)
            then (0, updatedVisited)
            else foldl updateSumAndVisited (1, updatedVisited) higherNeighbors
          where
            higherNeighbors = [(r, c) | (r, c) <- neighbors heightMap i j, heightMap !! r !! c >= heightMap !! i !! j]
            updatedVisited = S.insert (i, j) visited
            updateSumAndVisited :: (Int, Set (Int, Int)) -> (Int, Int) -> (Int, Set (Int, Int))
            updateSumAndVisited (curSum, curVisited) (r, c) = (curSum + childSum, newVisited)
              where
                (childSum, newVisited) = calcBasinSizeHelper curVisited (r, c)

    updateLargestBasinSizes :: [Int] -> (Int, Int) -> [Int]
    updateLargestBasinSizes largestSizes lowPoint
      | length largestSizes < 3 = size : largestSizes
      | size > minimum largestSizes = size : drop 1 (sort largestSizes)
      | otherwise = largestSizes
      where
        size = calcBasinSize lowPoint

    threeLargestBasinSizes = foldl updateLargestBasinSizes [] (lowPointIndices heightMap)

day9Main :: IO ()
day9Main = do
  testData <- readFile "data/day_9_test.txt"
  realData <- readFile "data/day_9.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
  print $ solvePart2 testData
  print $ solvePart2 realData
