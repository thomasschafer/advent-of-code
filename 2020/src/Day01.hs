module Day01 (part1, part2) where

import Data.Maybe (fromJust)
import Data.Set qualified as S

twoSum :: Int -> [Int] -> (Int, Int)
twoSum target = fromJust . twoSum' S.empty
  where
    twoSum' _ [] = Nothing
    twoSum' seen (x : xs)
      | required `elem` seen = Just (x, required)
      | otherwise = twoSum' (S.insert x seen) xs
      where
        required = target - x

part1 :: String -> Int
part1 = uncurry (*) . twoSum 2020 . map read . lines

-- This solution assumes that there are no duplicates, which works for my input data.
-- If duplicates might be present we can use a hashmap with a count rather than a set.
threeSum :: Int -> [Int] -> (Int, Int, Int)
threeSum target nums =
  head
    [ (nums !! i, nums !! j, required)
      | i <- [1 .. length nums - 1],
        let a = nums !! i,
        j <- [0 .. i - 1],
        let b = nums !! j,
        let required = target - (a + b),
        required `elem` numsSet
    ]
  where
    numsSet = S.fromList nums

part2 :: String -> Int
part2 = tupleProduct . threeSum 2020 . map read . lines
  where
    tupleProduct (x, y, z) = x * y * z
