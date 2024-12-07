module Day07 (part1, part2) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List.Split (splitOn)
import Utils (toTuple)

concatOp :: Int -> Int -> Int
concatOp x y = read $ show x ++ show y

hasSolution :: Bool -> (Int, [Int]) -> Bool
hasSolution allowConcat (result, num : nums) = hasSolution' num nums
  where
    ops = (if allowConcat then (concatOp :) else id) [(*), (+)]

    hasSolution' acc [] = result == acc
    hasSolution' acc (n : ns)
      | acc > result = False
      | otherwise = any (\op -> hasSolution' (acc `op` n) ns) ops

solve :: Bool -> String -> Int
solve allowConcat = sum . map fst . filter (hasSolution allowConcat) . parse
  where
    parse = map (bimap read (map read . words) . toTuple . splitOn ": ") . lines

part1 :: String -> Int
part1 = solve False

part2 :: String -> Int
part2 = solve True
