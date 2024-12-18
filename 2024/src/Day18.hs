module Day18 (part1Test, part1Full, part2Test, part2Full) where

import Control.Arrow (Arrow (first))
import Control.Monad (join)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)
import Utils (mapTuple, toTuple, (...))

solve :: Bool -> (Int, Int) -> Set (Int, Int) -> Maybe Int
solve startAtEnd (maxRow, maxCol) blocked = solve' (S.singleton start) S.empty
  where
    (start, end) = (if startAtEnd then swap else id) ((0, 0), (maxRow - 1, maxCol - 1))

    solve' positions visited
      | null positions = Nothing
      | end `elem` positions = Just 0
      | otherwise = (1 +) <$> solve' (S.fromList $ concatMap neighbors positions) (S.union visited positions)
      where
        neighbors (r, c) =
          [ (r', c')
            | (r', c') <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)],
              r' >= 0 && r' < maxRow && c' >= 0 && c' < maxCol,
              (r', c') `notElem` blocked && (r', c') `notElem` visited
          ]

parse :: Maybe Int -> String -> [(Int, Int)]
parse numBytes = maybe id take numBytes . map (toTuple . map read . splitOn ",") . lines

part1Test, part1Full :: String -> Int
(part1Test, part1Full) = mapTuple (uncurry part1 . first (join (,))) ((7, 12), (71, 1024))
  where
    part1 :: (Int, Int) -> Int -> String -> Int
    part1 dims = fromJust . solve False dims . S.fromList ... parse . Just

part2Test, part2Full :: String -> (Int, Int)
(part2Test, part2Full) = mapTuple (part2 . join (,)) (7, 71)
  where
    part2 dims = firstToRemove . reverse . parse Nothing
      where
        firstToRemove (toRemove : rest) = case solve True dims (S.fromList rest) of
          Just _ -> toRemove
          Nothing -> firstToRemove rest
