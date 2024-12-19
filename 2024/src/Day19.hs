module Day19 (part1, part2) where

import Control.Arrow (Arrow (first, (&&&)))
import Data.Bifunctor (Bifunctor (bimap))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Utils (toTuple)

numPossible :: HashMap String Int -> [String] -> String -> (Int, HashMap String Int)
numPossible cache _ [] = (1, cache)
numPossible cache towels design = case HM.lookup design cache of
  Just res -> (res, cache)
  Nothing -> (res, HM.insert design res updatedCache)
    where
      (res, updatedCache) = foldl update (0, cache) towels

      update (x, c) towel
        | towel `isPrefixOf` design = first (x +) $ numPossible c towels (drop (length towel) design)
        | otherwise = (x, c)

solve :: String -> [Int]
solve s = map (fst . numPossible HM.empty towels) designs
  where
    (towels, designs) = bimap (splitOn ", ") lines . toTuple $ splitOn "\n\n" s

part1, part2 :: String -> Int
(part1, part2) = ((length . filter (> 0)) .) &&& (sum .) $ solve
