module Day17 (part1, part2) where

import Data.Set (Set)
import Data.Set qualified as S
import Utils ((...))

parse :: Int -> String -> Set [Int]
parse numDims = S.fromList . activeCoords . lines
 where
  activeCoords d =
    [ [x, y] ++ replicate (numDims - 2) 0
    | x <- [0 .. length (head d) - 1]
    , y <- [0 .. length d - 1]
    , d !! y !! x == '#'
    ]

neighbors :: [Int] -> Set [Int]
neighbors coords = S.delete coords . S.fromList $ neighbors' coords
 where
  neighbors' [] = [[]]
  neighbors' (x : xs) =
    [ (x + delta) : rest
    | rest <- neighbors' xs
    , delta <- [-1 .. 1]
    ]

runCycle :: Set [Int] -> Set [Int]
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

solve :: Int -> String -> Int
solve = length . (!! 6) . iterate runCycle ... parse

part1 :: String -> Int
part1 = solve 3

part2 :: String -> Int
part2 = solve 4
