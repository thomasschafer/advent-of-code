module Day02 (part1, part2) where

perms :: Int -> [Int] -> [[Int]]
perms _ [] = [[]]
perms 0 xs = [xs]
perms 1 (x : xs) = xs : map (x :) (perms 1 xs)

solve :: Int -> String -> Int
solve badLevelsAccepted = length . filter (any isSafe) . map (perms badLevelsAccepted) . parse
  where
    parse = map (map read . words) . lines
    isIncreasing xs = all (\(x, y) -> y > x && (abs (x - y) <= 3)) . zip xs $ tail xs
    isDecreasing = isIncreasing . reverse
    isSafe xs = isIncreasing xs || isDecreasing xs

part1 :: String -> Int
part1 = solve 0

part2 :: String -> Int
part2 = solve 1
