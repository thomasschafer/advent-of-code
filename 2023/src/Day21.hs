module Day21 (part1, part2, solve) where

import Control.Applicative (liftA2)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Set (Set)
import Data.Set qualified as S

plotsReachedAfter :: [String] -> (Bool, Int) -> Set (Int, Int)
plotsReachedAfter plots (wrap, n) =
  (if odd n then fst else snd) $
    plotsReachedAfter' 1 initial S.empty S.empty
  where
    initial = S.fromList [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1], plots !! r !! c == 'S']
    rows = length plots
    cols = length $ head plots
    canMoveTo (r, c) =
      if wrap
        then plots !! (r `mod` rows) !! (c `mod` cols) /= '#'
        else r >= 0 && r < rows && c >= 0 && c < cols && plots !! r !! c /= '#'

    plotsReachedAfter' k cur seenOdd seenEven
      | k > n = (seenOdd, seenEven)
      | otherwise = plotsReachedAfter' (k + 1) boundaryAfterStep updatedSeenOdd updatedSeenEven
      where
        boundaryAfterStep = foldl (\acc -> foldr S.insert acc . neighbors) S.empty cur
        neighbors (r, c) =
          filter
            (liftA2 (&&) (`notElem` (if odd k then seenOdd else seenEven)) canMoveTo)
            [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
        updatedSeenOdd = if odd k then foldr S.insert seenOdd boundaryAfterStep else seenOdd
        updatedSeenEven = if even k then foldr S.insert seenEven boundaryAfterStep else seenEven

solve :: Bool -> Int -> String -> Int
solve = curry . flip $ (length .) . plotsReachedAfter . lines

part1 :: String -> Int
part1 = solve False 64

part2 :: String -> Int
part2 = solve True 26501365
