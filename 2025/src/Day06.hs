module Day06 (part1, part2) where

import Data.List (transpose)
import Data.List.Split (splitWhen)

parseOp :: Char -> ([Int] -> Int)
parseOp '+' = sum
parseOp '*' = product

part1 :: String -> Int
part1 = sum . map (uncurry ($) . parse) . transpose . map words . lines
  where
    parse xs = (parseOp . head $ last xs, map read $ init xs)

part2 :: String -> Int
part2 = sum . map solve . splitWhen (all (== ' ')) . transpose . lines
  where
    solve (n : ns) = op $ map read (numHead : ns)
      where
        (numHead, op) = (init n, parseOp $ last n)
