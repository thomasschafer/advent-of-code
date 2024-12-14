module Day14 (part1Test, part1Full, part2) where

import Data.List.Split (splitOn)
import Utils (mapTuple, toTuple)

parse :: String -> ((Int, Int), (Int, Int))
parse = toTuple . map (toTuple . map read . splitOn "," . last . splitOn "=") . words

move :: (Int, Int) -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
move (width, height) ((px, py), (vx, vy)) = (((px + vx) `mod` width, (py + vy) `mod` height), (vx, vy))

safetyFactor :: (Int, Int) -> [(Int, Int)] -> Int
safetyFactor (width, height) robotPositions = product $ map length quadrants
  where
    (quadHeight, quadWidth) = mapTuple (`div` 2) (height, width)
    quadrants =
      [ filter (\(x, y) -> x `f` quadWidth && y `g` quadHeight) robotPositions
        | let comps = [(<), (>)],
          f <- comps,
          g <- comps
      ]

part1 :: (Int, Int) -> String -> Int
part1 dims = safetyFactor dims . map fst . (!! 100) . iterate (map $ move dims) . map parse . lines

part1Test, part1Full :: String -> Int
(part1Test, part1Full) = mapTuple part1 ((11, 7), (101, 103))

part2 :: String -> Int
part2 = const 2
