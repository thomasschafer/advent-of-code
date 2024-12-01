module Day01 (part1, part2) where

import Data.HashMap.Strict qualified as HM
import Data.List (sort, transpose)
import Data.Maybe (fromMaybe)
import Utils (freqCounts, toTuple)

part1 :: String -> Int
part1 = sum . map abs . uncurry (zipWith (-)) . toTuple . map sort . transpose . map (map read . words) . lines

part2 :: String -> Int
part2 = uncurry similarityScore . toTuple . map (freqCounts . sort) . transpose . map (map read . words) . lines
  where
    similarityScore counts1 counts2 = sum . map (\(x, c) -> x * c * fromMaybe 0 (HM.lookup x counts2)) $ HM.toList counts1
