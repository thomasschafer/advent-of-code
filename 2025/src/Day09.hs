module Day09 (part1, part2) where

import Data.List.Split (splitOn)
import Utils (toTuple)

part1 :: String -> Int
part1 = maximum . toAreas . map (toTuple . map read . splitOn ",") . lines
  where
    toAreas xs =
      [ (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)
      | i <- [0 .. length xs - 2],
        j <- [(i + 1) .. length xs - 1],
        let (x1, y1) = xs !! i,
        let (x2, y2) = xs !! j
      ]

part2 :: String -> Int
part2 = const 2
