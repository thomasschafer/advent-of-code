module Day12 (part1, part2) where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Utils (toTuple)

type Present = [[Bool]]

type Region = ((Int, Int), [Int])

parse :: String -> ([Present], [Region])
parse s = bimap (map parsePresent) (map parseRegion) . (init &&& last) . map lines $ splitOn "\n\n" s
  where
    parseRegion = bimap (toTuple . map read . splitOn "x") (map read . words) . toTuple . splitOn ": "
    parsePresent = map (map (== '#')) . tail

canFitPresents :: [Present] -> Region -> Bool
canFitPresents initialPs ((width, height), initialReq)
  | length (filter id . concat . concat $ initialPs) > width * height = False
  | otherwise = go initialPs initialReq $ replicate height (replicate width False)
  where
    go :: [Present] -> [Int] -> [[Bool]] -> Bool
    go presents required grid = undefined

part1 :: String -> Int
part1 = length . uncurry (filter . canFitPresents) . parse

part2 :: String -> Int
part2 = const 2
