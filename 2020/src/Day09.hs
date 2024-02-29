module Day09 (part1, part2) where

import Control.Arrow ((***))
import Control.Monad (join)
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

contiguousRangeSum :: Int -> [Int] -> Maybe [Int]
contiguousRangeSum target = contiguousRangeSum' 0 []
  where
    contiguousRangeSum' acc curWindow rest = case compare acc target of
      EQ -> Just curWindow
      GT -> contiguousRangeSum' (acc - head curWindow) (tail curWindow) rest
      LT ->
        if null rest
          then Nothing
          else contiguousRangeSum' (acc + head rest) (curWindow ++ [head rest]) (tail rest)

part2 :: Int -> String -> Int
part2 windowLength inputStr = minPlusMax . fromJust $ contiguousRangeSum firstInvalid nums
  where
    nums = map read $ lines inputStr
    firstInvalid = fromJust $ firstInvalidNum nums windowLength
    minPlusMax = uncurry (+) . flip (join (***)) (minimum, maximum) . flip ($)
