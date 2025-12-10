module Day10 (part1, part2) where

import Data.Bits (Bits (xor))
import Data.List.Split (splitOn)
import Data.Set qualified as S

parse :: String -> ([Bool], [[Int]], [Int])
parse s = (lights, wiring, joltage)
  where
    [l, w, j] = concatMap (splitOn ") {") $ splitOn "] (" s
    lights = map (== '#') $ tail l
    wiring = map (map (read) . splitOn ",") $ splitOn ") (" w
    joltage = map read . splitOn "," $ init j

fewestPressesIndicators :: [Bool] -> [[Int]] -> Int
fewestPressesIndicators desiredBools switchesList = go (S.singleton 0) 0
  where
    desired = sum . zipWith (*) [2 ^ i | i <- [0 ..]] $ map fromEnum desiredBools
    switches = map (sum . map (2 ^)) switchesList

    go cur count
      | desired `S.member` cur = count
      | otherwise = go new (count + 1)
      where
        new = foldl (flip S.insert) cur $ [s `xor` c | s <- switches, c <- S.toList cur]

part1 :: String -> Int
part1 = sum . map (uncurry fewestPressesIndicators . (\(l, w, _) -> (l, w)) . parse) . lines

fewestPressesJoltage :: [[Int]] -> [Int] -> Int
fewestPressesJoltage switchesLists desired = go (S.singleton $ map (const 0) desired) 0
  where
    switches =
      map (\s -> map fromEnum [i `elem` s | i <- [0 .. length desired - 1]]) $
        map S.fromList switchesLists

    go cur count
      | desired `S.member` cur = count
      | otherwise = go new (count + 1)
      where
        new = foldl (flip S.insert) cur $ [res | s <- switches, c <- S.toList cur, let res = zipWith (+) s c, and $ zipWith (<=) res desired]

part2 :: String -> Int
part2 = sum . map (uncurry fewestPressesJoltage . (\(_, w, j) -> (w, j)) . parse) . lines
