module Day08 (part1, part2) where

import Data.List (partition, sort)
import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (mapTuple)

type Pos = (Int, Int, Int)

dist :: Pos -> Pos -> Float
dist (x1, y1, z1) (x2, y2, z2) = sqrt . fromIntegral $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2

combineCircuits :: [Set Pos] -> (Pos, Pos) -> [Set Pos]
combineCircuits circuits (pos1, pos2) = newContains : notContains
  where
    (contains, notContains) = partition (\c -> any (flip S.member c) [pos1, pos2]) circuits
    newContains = foldl1 S.union ((S.fromList [pos1, pos2]) : contains)

withDistances :: [Pos] -> [(Float, Pos, Pos)]
withDistances positions =
  [ (dist p1 p2, p1, p2)
  | i <- [0 .. length positions - 2],
    j <- [i + 1 .. length positions - 1],
    let p1 = positions !! i,
    let p2 = positions !! j
  ]

combineClosestN :: Int -> [Pos] -> [Set Pos]
combineClosestN n =
  foldl combineCircuits []
    . map (\(_, p1, p2) -> (p1, p2))
    . take n
    . sort
    . withDistances

parse :: String -> [Pos]
parse = map (toPos . map read . splitOn ",") . lines
  where
    toPos [x, y, z] = (x, y, z)

part1 :: Int -> String -> Int
part1 n = product . take 3 . reverse . sort . map S.size . combineClosestN n . parse

combineUntilFullyConnected :: [Pos] -> [(Float, Pos, Pos)] -> (Pos, Pos)
combineUntilFullyConnected = go . map S.singleton
  where
    go circuits ((_, p1, p2) : rest)
      | length newCircuits == 1 = (p1, p2)
      | otherwise = go newCircuits rest
      where
        newCircuits = combineCircuits circuits (p1, p2)

part2 :: String -> Int
part2 s = uncurry (*) . mapTuple (\(x, _, _) -> x) . combineUntilFullyConnected positions . sort $ withDistances positions
  where
    positions = parse s
