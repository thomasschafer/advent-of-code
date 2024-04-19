module Day20 (part1, part2) where

import Data.List.Split (splitOn)
import Data.Set qualified as S

type Tile = (Int, [String])

parse :: String -> [Tile]
parse =
  map ((\t -> (parseId $ head t, tail t)) . splitOn "\n")
    . splitOn "\n\n"
 where
  parseId = read . init . last . words

sides :: [String] -> [String]
sides tile = [head tile, last tile, map head tile, map last tile]

hasMatchingSide :: Tile -> Tile -> Bool
hasMatchingSide t1 t2 =
  not . null $
    S.fromList (sides $ snd t1)
      `S.intersection` (S.fromList (sides $ snd t2) `S.union` S.fromList (map reverse . sides $ snd t2))

findCorners :: [Tile] -> [Tile]
findCorners tiles = filter ((== 2) . length . matchingTiles) tiles
 where
  matchingTiles t1 = filter (\t2 -> (fst t2 /= fst t1) && hasMatchingSide t1 t2) tiles

part1 :: String -> Int
part1 = product . map fst . findCorners . parse

part2 :: String -> Int
part2 = const 2
