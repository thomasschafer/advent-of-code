module Day24 (part1Test, part1Real, part2) where

import Data.List.Split (splitOn)

type Coords = (Int, Int, Int)

type Velocity = (Int, Int, Int)

parse :: String -> (Coords, Velocity)
parse s = ((px, py, pz), (vx, vy, vz))
  where
    [[px, py, pz], [vx, vy, vz]] = map (map read . splitOn ", ") $ splitOn " @ " s

pathsCross :: Int -> Int -> (Coords, Velocity) -> (Coords, Velocity) -> Bool
pathsCross lower' upper' ((px1', py1', _), (vx1', vy1', _)) ((px2', py2', _), (vx2', vy2', _))
  | vx1 == vx2 && vy1 == vy2 || vx1 * vy2 == vx2 * vy1 =
      (px1, py1) == (px2, py2)
  | otherwise =
      (k1 >= 0 && k2 >= 0)
        && all (\c -> c >= lower && c <= upper) [xCrossing, yCrossing]
  where
    [px1, py1, vx1, vy1, px2, py2, vx2, vy2, lower, upper] =
      map toRational [px1', py1', vx1', vy1', px2', py2', vx2', vy2', lower', upper']
    k2 = ((py2 - py1) * vx1 - (px2 - px1) * vy1) / (vx2 * vy1 - vy2 * vx1)
    k1 = if vy1 == 0 then (px2 - px1 + k2 * vx2) / vx1 else (py2 - py1 + k2 * vy2) / vy1
    xCrossing = px1 + k1 * vx1
    yCrossing = py1 + k1 * vy1

numCrossingPaths :: Int -> Int -> [(Coords, Velocity)] -> Int
numCrossingPaths lower upper hailstones =
  length . filter (uncurry $ pathsCross lower upper) $
    [ (hailstones !! idx2, hailstones !! idx1)
      | idx1 <- [1 .. length hailstones - 1],
        idx2 <- [0 .. idx1 - 1]
    ]

part1Solve :: Int -> Int -> String -> Int
part1Solve lower upper = numCrossingPaths lower upper . map parse . lines

part1Test :: String -> Int
part1Test = part1Solve 7 27

part1Real :: String -> Int
part1Real = part1Solve 200000000000000 400000000000000

collisionPosition :: [(Coords, Velocity)] -> Coords
collisionPosition hailstones = (1, 2, 3)

part2 :: String -> Int
part2 = addCoords . collisionPosition . map parse . lines
  where
    addCoords (x, y, z) = x + y + z

-- co, vo, (k1, ..., kn) such that:
-- co + k1 * vo == c1 + k1 * v1
-- co + k2 * vo == c2 + k2 * v2
-- ...
-- co + kn * vo == c2 + k2 * v2
