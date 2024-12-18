module Day18 (part1Test, part1Full, part2) where

import Control.Arrow (Arrow (first))
import Control.Monad (join)
import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (mapTuple, toTuple)

solve :: (Int, Int) -> Set (Int, Int) -> Int
solve (maxRow, maxCol) blocked = solve' (S.singleton (0, 0)) S.empty
  where
    solve' :: Set (Int, Int) -> Set (Int, Int) -> Int
    solve' positions visited
      | (maxRow - 1, maxCol - 1) `elem` positions = 0
      | otherwise = 1 + solve' (S.fromList $ concatMap neighbors positions) (S.union visited positions)
      where
        neighbors (r, c) =
          [ (r', c')
            | (r', c') <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)],
              r' >= 0 && r' < maxRow && c' >= 0 && c' < maxCol,
              (r', c') `notElem` blocked && (r', c') `notElem` visited
          ]

part1 :: (Int, Int) -> Int -> String -> Int
part1 dims numBytes = solve dims . S.fromList . take numBytes . map (toTuple . map read . splitOn ",") . lines

part1Test, part1Full :: String -> Int
(part1Test, part1Full) = mapTuple (uncurry part1 . first (join (,))) ((7, 12), (71, 1024))

part2 :: String -> Int
part2 = const 2
