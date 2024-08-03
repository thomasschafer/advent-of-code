module Day17 (part1, part2) where

import Data.Set (Set)
import Data.Set qualified as S
import Utils ((...))

type Coords = [Int]

parse :: Int -> String -> Set Coords
parse numDims = S.fromList . activeCoords . lines
 where
  activeCoords d =
    [ [x, y] ++ replicate (numDims - 2) 0
    | x <- [0 .. length (head d) - 1]
    , y <- [0 .. length d - 1]
    , d !! y !! x == '#'
    ]

neighbors :: Coords -> Set Coords
neighbors coords = S.delete coords . S.fromList $ neighbors' coords
 where
  neighbors' [] = [[]]
  neighbors' (x : xs) =
    [ (x + delta) : rest
    | rest <- neighbors' xs
    , delta <- [-1 .. 1]
    ]

runCycle :: Set Coords -> Set Coords
runCycle active = stillActive `S.union` newlyActive
 where
  numActiveNeighbors = length . S.filter (`elem` active) . neighbors

  remainActive = (`elem` [2, 3]) . numActiveNeighbors
  stillActive = S.filter remainActive active

  becomeActive = (== 3) . numActiveNeighbors
  newlyActive =
    S.filter
      (\c -> (c `notElem` active) && becomeActive c)
      (foldr1 S.union $ S.map neighbors active)

solve :: Int -> String -> Int
solve = length . (!! 6) . iterate runCycle ... parse

part1 :: String -> Int
part1 = solve 3

part2 :: String -> Int
part2 = solve 4
