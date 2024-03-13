module Day05 (part1, part2) where

import Control.Arrow (Arrow (first), (***))
import Control.Monad (join)
import Data.Foldable (find)
import Data.List (sort)
import Data.Maybe (fromJust)

toNum :: String -> Int
toNum = foldl update 0
  where
    update acc next = 2 * acc + if next `elem` ['R', 'B'] then 1 else 0

seatId :: String -> Int
seatId = uncurry (+) . first (* 8) . join (***) toNum . splitAt 7

part1 :: String -> Int
part1 = maximum . map seatId . lines

missingSeatId :: [Int] -> Int
missingSeatId =
  (+ 1)
    . fst
    . fromJust
    . find (\(a, b) -> a + 1 /= b)
    . (\d -> zip d (tail d))
    . sort

part2 :: String -> Int
part2 = missingSeatId . map seatId . lines