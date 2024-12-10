module Day10 (part1, part2) where

import Data.Set qualified as S
import Utils (toInt)

nextSteps :: (Int, Int) -> ([[Int]], Int, Int) -> [(Int, Int)]
nextSteps (r, c) (hikingMap, rows, cols) =
  filter
    (\(r', c') -> not (outOfBounds (r', c')) && hikingMap !! r' !! c' == 1 + hikingMap !! r !! c)
    [(r + 1, c), (r - 1, c), (r, c - 1), (r, c + 1)]
  where
    outOfBounds (r', c') = r' < 0 || r' >= rows || c' < 0 || c' >= cols

type ScoreFn = ([[Int]], Int, Int) -> (Int, Int) -> Int

score, rating :: ScoreFn
score hm@(hikingMap, _, _) = length . reachable9s
  where
    reachable9s (r, c)
      | hikingMap !! r !! c == 9 = S.singleton (r, c)
      | otherwise = foldl S.union S.empty . map reachable9s $ nextSteps (r, c) hm
rating hm@(hikingMap, _, _) = rating'
  where
    rating' (r, c)
      | hikingMap !! r !! c == 9 = 1
      | otherwise = sum . map rating' $ nextSteps (r, c) hm

solve :: ScoreFn -> String -> Int
solve scoreFn s = sum $ map (scoreFn (hikingMap, rows, cols)) trailheads
  where
    hikingMap = map (map toInt) $ lines s
    (rows, cols) = (length hikingMap, length (head hikingMap))
    trailheads = [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1], hikingMap !! r !! c == 0]

part1, part2 :: String -> Int
part1 = solve score
part2 = solve rating
