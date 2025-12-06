module Day06 (part1, part2) where

import Utils (rpad, quickTrace)
import Data.List (transpose)
import Data.Bifunctor (bimap)

data Op = Add | Multiply deriving (Show)

parseOp :: String -> Op
parseOp "+" = Add
parseOp "*" = Multiply

fromOp :: Op -> [Int] -> Int
fromOp Add = sum
fromOp Multiply = product

part1 :: String -> Int
part1  = sum . map (uncurry ($) . parse) . transpose . map words . lines
  where
    parse xs = (fromOp . parseOp $ last xs, map read $ init xs)

part2 :: String -> Int
part2 = const 2
