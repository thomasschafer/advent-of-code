module Day01 (part1, part2) where

part1 :: String -> Int
part1 = sum . map (fuelRequired . read) . lines
 where
  fuelRequired mass = mass `div` 3 - 2

part2 :: String -> Int
part2 = const 1
