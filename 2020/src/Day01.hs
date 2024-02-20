module Day01 (part1, part2) where

import Data.Maybe (fromJust)
import Data.Set qualified as S

twoSum :: (Ord a, Num a) => S.Set a -> a -> [a] -> Maybe (a, a)
twoSum _ _ [] = Nothing
twoSum seen target (x : xs)
  | required `elem` seen = Just (x, required)
  | otherwise = twoSum (S.insert x seen) target xs
  where
    required = target - x

part1 :: String -> Int
part1 = uncurry (*) . fromJust . twoSum S.empty 2020 . map read . lines

threeSum :: (Ord a, Num a) => a -> [a] -> Maybe (a, a, a)
threeSum target (n : nums) =
  case twoSum S.empty (target - n) nums of
    Just (x, y) -> Just (n, x, y)
    Nothing -> threeSum target nums
threeSum _ _ = Nothing

part2 :: String -> Int
part2 = tupleProduct . fromJust . threeSum 2020 . map read . lines
  where
    tupleProduct (x, y, z) = x * y * z
