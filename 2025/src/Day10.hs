module Day10 (part1, part2) where

import Data.Bits (Bits (xor))
import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S

parse :: String -> (Int, [Int], [Int])
parse s = (lights, wiring, joltage)
  where
    [l, w, j] = concatMap (splitOn ") {") $ splitOn "] (" s
    lights = sum . zipWith (*) [2 ^ i | i <- [0 ..]] . map (fromEnum . (== '#')) $ tail l
    wiring = map (sum . map ((2 ^) . read) . splitOn ",") $ splitOn ") (" w
    joltage = map read . splitOn "," $ init j

fewestPresses :: (Int, [Int], [Int]) -> Int
fewestPresses (desired, switches, _) = go (S.singleton 0) 0
  where
    go :: Set Int -> Int -> Int
    go cur count
      | desired `S.member` cur = count
      | otherwise = go new (count + 1)
      where
        new = foldl (flip S.insert) cur $ [s `xor` c | s <- switches, c <- S.toList cur]

part1 :: String -> Int
part1 = sum . map (fewestPresses . parse) . lines

part2 :: String -> Int
part2 = const 2
