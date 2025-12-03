module Day03 (part1, part2) where

import Utils (toInt)

solve :: Int -> String -> Int
solve n = sum . map (maxJoltage n 0 . map toInt) . lines

maxJoltage :: Int -> Int -> [Int] -> Int
maxJoltage 0 acc _ = acc
maxJoltage n acc xs = maxJoltage (n - 1) (10 * acc + firstDigit) rest
  where
    firstDigit = maximum $ take (length xs - (n - 1)) xs
    rest = tail $ dropWhile (< firstDigit) xs

part1 :: String -> Int
part1 = solve 2

part2 :: String -> Int
part2 = solve 12
