module Main (main) where

import Data.List (transpose)
import Data.List.Split (splitWhen)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

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
