module Day01 (part1, part2) where

import Data.Maybe (fromJust)
import Data.Set qualified as S

numsSummingTo :: Int -> [Int] -> (Int, Int)
numsSummingTo target = fromJust . numsSummingTo' S.empty
  where
    numsSummingTo' _ [] = Nothing
    numsSummingTo' seen (x : xs)
      | required `elem` seen = Just (x, required)
      | otherwise = numsSummingTo' (S.insert x seen) xs
      where
        required = target - x

part1 :: String -> Int
part1 = uncurry (*) . numsSummingTo 2020 . map read . lines

part2 :: String -> Int
part2 = const 3