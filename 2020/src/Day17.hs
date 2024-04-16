module Day17 (part1, part2) where

import Data.Set (Set)
import Data.Set qualified as S

type Coords = (Int, Int, Int)

parse :: String -> Set Coords
parse = S.fromList . activeCoords . lines
 where
  activeCoords d =
    [ (x, y, 0)
    | x <- [0 .. length (head d) - 1]
    , y <- [0 .. length d - 1]
    , d !! y !! x == '#'
    ]

neighbors :: Coords -> Set Coords
neighbors (x, y, z) =
  S.fromList
    [ (x + dx, y + dy, z + dz)
    | dx <- [-1 .. 1]
    , dy <- [-1 .. 1]
    , dz <- [-1 .. 1]
    , (dx, dy, dz) /= (0, 0, 0)
    ]
runCycle :: Set Coords -> Set Coords
runCycle active = stillActive `S.union` newlyActive
 where
  stillActive = S.filter remainActive active
  newlyActive =
    S.filter
      (\c -> not (isActive c) && becomeActive c)
      (foldr (S.union . neighbors) S.empty active)

  isActive = (`elem` active)

  remainActive c =
    if not (isActive c)
      then error $ "remainActive was passed inactive element " ++ show c
      else length (S.filter isActive $ neighbors c) `elem` [2, 3]

  becomeActive c =
    if isActive c
      then error $ "becomeActive was passed active element " ++ show c
      else length (S.filter isActive $ neighbors c) == 3

part1 :: String -> Int
part1 = length . (!! 6) . iterate runCycle . parse

part2 :: String -> Int
part2 = const 1
