module Day10 (part1, part2) where

import Data.List (sort)

countOf :: (Eq a) => a -> [a] -> Int
countOf = (length .) . filter . (==)

countProduct :: (Num a, Ord a) => [a] -> [a] -> Int
countProduct xs toCount = product . countsOfList toCount $ diffs extendedNums
  where
    extendedNums = 0 : (maximum xs + 3) : xs
    diffs = (\ys -> zipWith (-) (tail ys) ys) . sort
    countsOfList = flip $ map . flip countOf

part1 :: String -> Int
part1 = flip countProduct [1, 3] . map read . lines

part2 :: String -> Int
part2 = const 2
