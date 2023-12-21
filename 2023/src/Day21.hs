module Day21 (part1, part2) where

import Data.Set (Set)
import Data.Set qualified as S

plotsReachedAfter :: [String] -> Int -> Set (Int, Int)
plotsReachedAfter plots n =
  (if odd n then fst else snd) $
    plotsReachedAfter' 1 initial S.empty S.empty
  where
    initial = S.fromList [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1], plots !! r !! c == 'S']
    rows = length plots
    cols = length $ head plots

    plotsReachedAfter' k cur seenOdd seenEven
      | k > n = (seenOdd, seenEven)
      | otherwise = plotsReachedAfter' (k + 1) boundaryAfterStep updatedSeenOdd updatedSeenEven
      where
        boundaryAfterStep = foldl (\acc -> foldr S.insert acc . neighbors) S.empty cur
        neighbors (r, c) =
          filter
            (\(r', c') -> r' >= 0 && r' < rows && c' >= 0 && c' < cols && plots !! r' !! c' /= '#')
            [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
        updatedSeenOdd = if odd k then foldr S.insert seenOdd boundaryAfterStep else seenOdd
        updatedSeenEven = if even k then foldr S.insert seenEven boundaryAfterStep else seenEven

part1 :: Int -> String -> Int
part1 = flip $ (length .) . plotsReachedAfter . lines

part2 :: String -> Int
part2 = const 1
