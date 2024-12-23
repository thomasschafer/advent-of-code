module Day22 (part1, part2) where

import Data.Bits (Bits (xor))
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Tuple (swap)

secretNumber :: Int -> Int
secretNumber x0 = x3
  where
    mix = xor
    prune = (`mod` 2 ^ 24)

    x1 = prune $ (x0 * 2 ^ 6) `mix` x0
    x2 = prune $ (x1 `div` 2 ^ 5) `mix` x1
    x3 = prune $ (x2 * 2 ^ 11) `mix` x2

part1 :: String -> Int
part1 = sum . map ((!! 2000) . iterate secretNumber . read) . lines

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs
  | length xs < n = []
  | otherwise = take n xs : groupsOf n (tail xs)

withDiffs :: [Int] -> [(Int, [Int])]
withDiffs = map addDiff . groupsOf 5
  where
    addDiff xs = (last xs, zipWith (-) (tail xs) xs)

bestSequence :: [[(Int, [Int])]] -> Int
bestSequence xs = maximum . map countSum $ S.toList keys
  where
    counts = map (HM.fromList . reverse . map swap) xs
    keys = S.fromList $ concatMap HM.keys counts
    countSum k = sum $ map (fromMaybe 0 . HM.lookup k) counts

part2 :: String -> Int
part2 = bestSequence . map (withDiffs . map lastDigit . take 2000 . iterate secretNumber . read) . lines
  where
    lastDigit = read . (: []) . last . show
