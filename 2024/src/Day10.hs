module Day10 (part1, part2) where

import Data.Set qualified as S
import Utils (toInt)

score :: ([[Int]], Int, Int) -> (Int, Int) -> Int
score (hikingMap, rows, cols) = length . reachable9s S.empty
  where
    outOfBounds (r, c) = r < 0 || r >= rows || c < 0 || c >= cols
    reachable9s visited (r, c)
      | (r, c) `elem` visited || outOfBounds (r, c) = S.empty
      | hikingMap !! r !! c == 9 = S.singleton (r, c)
      | otherwise = foldl S.union S.empty $ map (reachable9s updatedVisited) nextSteps
      where
        updatedVisited = S.insert (r, c) visited
        nextSteps =
          filter
            (\(r', c') -> not (outOfBounds (r', c')) && hikingMap !! r' !! c' == 1 + hikingMap !! r !! c)
            [(r + 1, c), (r - 1, c), (r, c - 1), (r, c + 1)]

part1 :: String -> Int
part1 s = sum $ map (score (hikingMap, rows, cols)) trailheads
  where
    hikingMap = map (map toInt) $ lines s
    (rows, cols) = (length hikingMap, length (head hikingMap))
    trailheads = [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1], hikingMap !! r !! c == 0]

part2 :: String -> Int
part2 = const 2
