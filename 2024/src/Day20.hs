module Day20 (part1Test, part1Full, part2) where

import Data.List (tails)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
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

    bfs :: Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
    bfs visited position@(r, c)
      | position == end = [position]
      | otherwise = position : bfs (S.insert position visited) nextPosition
      where
        nextPosition =
          (\[pos] -> pos) $
            filter
              (\pos -> pos `notElem` visited && pos `notElem` walls && withinBounds pos)
              [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
        withinBounds (r', c') = r' >= 0 && r' < rows && c' >= 0 && c' < cols

part1 :: Int -> String -> Int
part1 jumpReq = sum . map (uncurry shortcuts) . mapMaybe split . tails . findPath . map (map parse) . lines
  where
    split (x : _ : rest) = Just (x, reverse $ drop jumpReq rest)
    split _ = Nothing

    shortcuts :: (Int, Int) -> [(Int, Int)] -> Int
    shortcuts _ [] = 0
    shortcuts (r, c) ((r', c') : rest) =
      (if abs (r' - r) + abs (c' - c) <= 2 then 1 else 0)
        + shortcuts (r, c) rest

part1Test, part1Full :: String -> Int
(part1Test, part1Full) = mapTuple part1 (64, 100)

part2 :: String -> Int
part2 = const 2
