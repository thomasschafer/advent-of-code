module Day06 (part1, part2) where

import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S

solve :: (Set Char -> Set Char -> Set Char) -> String -> Int
solve f = sum . map (length . foldl1 f . map S.fromList . lines) . splitOn "\n\n"

part1 :: String -> Int
part1 = solve S.union

part2 :: String -> Int
part2 = solve S.intersection
