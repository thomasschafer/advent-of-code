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

data ArrangementRes = ArrangementRes {num :: Int, arrangements :: Int}
  deriving (Show)

distinctArrangements :: [Int] -> Int
distinctArrangements =
  arrangements
    . head -- result is in reverse order
    . foldl calcNextArrangement [ArrangementRes {num = 0, arrangements = 1}]
    . sort
  where
    calcNextArrangement arrs next = nextResult : take 3 arrs -- only three elements can be within 3 of the current
      where
        nextResult =
          ArrangementRes
            { num = next,
              arrangements = sum . map arrangements $ filter ((>= next - 3) . num) arrs
            }

part2 :: String -> Int
part2 = distinctArrangements . map read . lines
