module Day02 (part1, part2) where

import Data.List.Split (splitOn)
import Utils (quickTrace, toTuple)

part1 :: String -> Int
part1 = sum . quickTrace "sum" . concat . map (filter isInvalid . toRange . toTuple . map read . splitOn "-") . splitOn ","
  where
    toRange (lower, upper) = [lower .. upper]

isInvalid :: Int -> Bool
isInvalid x = even len && (take (len `div` 2) s == drop (len `div` 2) s)
  where
    s = show x
    len = length s

part2 :: String -> Int
part2 = const 2
