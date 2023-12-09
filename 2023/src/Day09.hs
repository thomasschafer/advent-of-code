module Day09 (part1, part2) where

nextValue :: [Int] -> Int
nextValue xs = last xs + if all (== 0) differences then 0 else nextValue differences
  where
    differences = map (\idx -> xs !! (idx + 1) - xs !! idx) [0 .. length xs - 2]

solve :: Bool -> String -> Int
solve rev = sum . map (nextValue . (if rev then reverse else id) . map read . words) . lines

part1 :: String -> Int
part1 = solve False

part2 :: String -> Int
part2 = solve True
