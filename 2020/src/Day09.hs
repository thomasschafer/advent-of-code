module Day09 (part1, part2) where

import Data.Foldable (find)
import Data.Maybe (fromJust, isNothing)
import Day01 (twoSum)

firstInvalidNum :: [Int] -> Int -> Maybe Int
firstInvalidNum xs windowLength = (xs !!) <$> find isInvalid [windowLength .. length xs - 1]
  where
    isInvalid i =
      isNothing $
        twoSum (xs !! i) (take windowLength $ drop (i - windowLength) xs)

part1 :: Int -> String -> Int
part1 = flip $ (fromJust .) . firstInvalidNum . map read . lines

part2 :: String -> Int
part2 = const 1
