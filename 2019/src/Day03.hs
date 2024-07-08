module Day03 (part1, part2) where

import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (toTuple)
import Prelude hiding (Left, Right)

data Move = Left Int | Right Int | Up Int | Down Int

parse :: String -> [Move]
parse = map toMove . splitOn ","
  where
    toMove (c : num) =
      case c of
        'L' -> Left
        'R' -> Right
        'U' -> Up
        'D' -> Down
        $ read num

start :: (Int, Int)
start = (0, 0)

toPathSet :: [Move] -> Set (Int, Int)
toPathSet = fst . foldl update (S.singleton start, start)
  where
    update (path, pos) move = (foldr S.insert path [f pos k | k <- [1 .. n]], f pos n)
      where
        (n, f) = case move of
          Left k -> (k, \(x, y) k' -> (x - k', y))
          Right k -> (k, \(x, y) k' -> (x + k', y))
          Up k -> (k, \(x, y) k' -> (x, y + k'))
          Down k -> (k, \(x, y) k' -> (x, y - k'))

dist :: (Int, Int) -> Int
dist (x, y) = abs x + abs y

part1 :: String -> Int
part1 =
  minimum
    . S.map dist
    . S.filter (/= (0, 0))
    . uncurry S.intersection
    . toTuple
    . map (toPathSet . parse)
    . lines

part2 :: String -> Int
part2 = const 2
