module Day20 (part1Test, part1Full, part2Test, part2Full) where

import Data.List (tails)
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Utils (mapTuple, positionsOf)

data Position = Wall | Empty | Start | End
  deriving (Eq)

parse :: Char -> Position
parse '.' = Empty
parse '#' = Wall
parse 'S' = Start
parse 'E' = End

findPath :: [[Position]] -> [(Int, Int)]
findPath grid = bfs S.empty start
  where
    (start, end) = mapTuple (head . positionsOf grid . (==)) (Start, End)
    (rows, cols) = (length grid, length (head grid))
    walls = S.fromList $ positionsOf grid (== Wall)

    bfs visited position
      | position == end = [position]
      | otherwise = position : bfs (S.insert position visited) nextPosition
      where
        nextPosition =
          (\[pos] -> pos)
            . filter
              (\pos -> pos `notElem` visited && pos `notElem` walls && withinBounds pos)
            $ neighbors position
        withinBounds (r', c') = r' >= 0 && r' < rows && c' >= 0 && c' < cols

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

solve :: Int -> Int -> String -> Int
solve maxJump minTimeSaved s = sum . map (uncurry shortcuts) . mapMaybe split . tails $ findPath grid
  where
    grid = map (map parse) $ lines s

    split (x : rest) = Just (x, rest)
    split _ = Nothing

    shortcuts cur =
      length
        . filter (\(timeNormalReaches, pos) -> dist cur pos <= maxJump && timeNormalReaches - dist cur pos >= minTimeSaved)
        . zip [1 ..]

dist :: (Int, Int) -> (Int, Int) -> Int
dist (r, c) (r', c') = abs (r' - r) + abs (c' - c)

part1Test, part1Full :: String -> Int
(part1Test, part1Full) = mapTuple (solve 2) (10, 100)

part2Test, part2Full :: String -> Int
(part2Test, part2Full) = mapTuple (solve 20) (72, 100)
