module Day07 (part1, part2) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List.Split (splitOn)
import Utils (toTuple)

hasSolution :: (Int, [Int]) -> Bool
hasSolution (result, num : nums) = hasSolution' num nums
  where
    hasSolution' :: Int -> [Int] -> Bool
    hasSolution' acc [] = result == acc
    hasSolution' acc (n : ns)
      | acc > result = False
      | otherwise = or [hasSolution' (acc `op` n) ns | op <- [(*), (+)]]

part1 :: String -> Int
part1 = sum . map fst . filter hasSolution . map (bimap read (map read . words) . toTuple . splitOn ": ") . lines

part2 :: String -> Int
part2 = const 2
