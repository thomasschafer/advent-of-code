module Day05 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Utils (mapTuple, toTuple, (...))

part1 :: String -> Int
part1 = uncurry freshIngredients . bimap (map parseRange) (map read) . mapTuple lines . toTuple . splitOn "\n\n"
  where
    parseRange = toTuple . map read . splitOn "-"

freshIngredients :: [(Int, Int)] -> [Int] -> Int
freshIngredients = length ... filter . (flip $ any . inRange)

inRange :: Int -> (Int, Int) -> Bool
inRange n (lower, upper) = n >= lower && n <= upper

part2 :: String -> Int
part2 = const 2
