module Day22 (part1, part2) where

import Data.Bits (Bits (xor))

secretNumber :: Int -> Int
secretNumber x0 = x3
  where
    mix = xor
    prune = (`mod` 2 ^ 24)

    x1 = prune $ (x0 * 2 ^ 6) `mix` x0
    x2 = prune $ (x1 `div` 2 ^ 5) `mix` x1
    x3 = prune $ (x2 * 2 ^ 11) `mix` x2

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (`mod` 16777216)

part1 :: String -> Int
part1 = sum . map ((!! 2000) . iterate secretNumber . read) . lines

part2 :: String -> Int
part2 = const 2
