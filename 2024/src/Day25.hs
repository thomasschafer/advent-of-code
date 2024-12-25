module Day25 (part1) where

import Control.Arrow (Arrow (first), second)
import Data.List.Split (splitOn)
import Utils (mapTuple)

type Key = [[Bool]]

type Lock = [[Bool]]

parse :: String -> ([Key], [Lock])
parse = foldr (update . map (map (== '#')) . lines) ([], []) . splitOn "\n\n"
  where
    update cur = (if and (head cur) then second else first) (cur :)

matchingPairs :: [Key] -> [Lock] -> [(Key, Lock)]
matchingPairs keys locks = concatMap (filter (not . hasOverlap) . flip map locks . (,)) keys
  where
    hasOverlap = or . uncurry (zipWith (&&)) . mapTuple concat

part1 :: String -> Int
part1 = length . uncurry matchingPairs . parse
