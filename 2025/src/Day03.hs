module Day03 (part1, part2) where

import Utils (toInt)

part1 :: String -> Int
part1 = sum . map (maxJoltage . map toInt) . lines

maxJoltage :: [Int] -> Int
maxJoltage xs
  | length xs < 2 = 0
  | otherwise =
      maximum
        [ (head xs) * 10 + last xs,
          maxShiftLeft,
          maxShiftRight
        ]
  where
    maxShiftLeft = maxJoltage . dropWhile (<= head xs) $ tail xs
    maxShiftRight = maxJoltage . reverse . dropWhile (<= last xs) . reverse $ init xs

part2 :: String -> Int
part2 = const 2
