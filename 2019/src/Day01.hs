module Day01 (part1, part2) where

fuelRequired :: Bool -> Int -> Int
fuelRequired recursiveFuel mass
  | fuel > 0 = sum (fuel : [fuelRequired recursiveFuel fuel | recursiveFuel])
  | otherwise = 0
 where
  fuel = mass `div` 3 - 2

solve :: Bool -> String -> Int
solve recursiveFuel = sum . map (fuelRequired recursiveFuel . read) . lines

part1 :: String -> Int
part1 = solve False

part2 :: String -> Int
part2 = solve True
