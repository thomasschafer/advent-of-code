module Day22 (part1, part2) where

import Data.List.Split (splitOn)
import Utils (toTuple)

parse :: String -> ([Int], [Int])
parse = toTuple . map (map read . tail . lines) . splitOn "\n\n"

score :: [Int] -> Int
score = sum . zipWith (*) [1 ..] . reverse

playToCompletion :: ([Int], [Int]) -> [Int]
playToCompletion ([], cards) = cards
playToCompletion (cards, []) = cards
playToCompletion (c1 : c1s, c2 : c2s)
  | c1 > c2 = playToCompletion (c1s ++ [c1, c2], c2s)
  | c2 > c1 = playToCompletion (c1s, c2s ++ [c2, c1])
  | otherwise = error $ "Found cards of equal value: " ++ show c1

part1 :: String -> Int
part1 = score . playToCompletion . parse

part2 :: String -> Int
part2 = const 2
