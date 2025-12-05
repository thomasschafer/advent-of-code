module Day05 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.List.Split (splitOn)
import Utils (mapTuple, toTuple, (...))

parse :: String -> ([(Int, Int)], [Int])
parse = bimap (map parseRange) (map read) . mapTuple lines . toTuple . splitOn "\n\n"
  where
    parseRange = toTuple . map read . splitOn "-"

part1 :: String -> Int
part1 = uncurry freshIngredients . parse
  where
    freshIngredients = length ... filter . (flip $ any . inRange)
    inRange n (lower, upper) = n >= lower && n <= upper

rangeOverlap :: [(Int, Int)] -> Int
rangeOverlap ((l1, u1) : (l2, u2) : rest)
  | u1 >= l2 = rangeOverlap ((l1, max u1 u2) : rest)
rangeOverlap ((lower, upper) : rest) = upper - lower + 1 + rangeOverlap rest
rangeOverlap [] = 0

part2 :: String -> Int
part2 = rangeOverlap . sort . fst . parse
