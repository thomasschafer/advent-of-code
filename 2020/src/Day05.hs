module Day05 (part1, part2) where

import Control.Arrow ((***))
import Control.Monad (join)

toNum :: String -> Int
toNum = foldl update 0
  where
    update acc next = 2 * acc + if next `elem` ['R', 'B'] then 1 else 0

parse :: String -> (Int, Int)
parse = join (***) toNum . splitAt 7

part1 :: String -> Int
part1 = maximum . map (seatId . parse) . lines
  where
    seatId (row, col) = row * 8 + col

part2 :: String -> Int
part2 = const 2