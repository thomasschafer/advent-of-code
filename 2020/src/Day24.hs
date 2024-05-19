module Day24 (part1, part2) where

import Data.Set qualified as S

-- Distance (north, east)
-- e or w moves two units in given direction, ne/nw/se/sw moves one unit in both given directions
type Coords = (Int, Int)

parsePosition :: String -> Coords
parsePosition = parsePosition' (0, 0)
 where
  parsePosition' coords@(distNorth, distEast) = \case
    ('e' : rest) -> parsePosition' (distNorth, distEast + 2) rest
    ('w' : rest) -> parsePosition' (distNorth, distEast - 2) rest
    ('s' : 'e' : rest) -> parsePosition' (distNorth - 1, distEast + 1) rest
    ('s' : 'w' : rest) -> parsePosition' (distNorth - 1, distEast - 1) rest
    ('n' : 'e' : rest) -> parsePosition' (distNorth + 1, distEast + 1) rest
    ('n' : 'w' : rest) -> parsePosition' (distNorth + 1, distEast - 1) rest
    [] -> coords

part1 :: String -> Int
part1 = length . foldl updateCounts S.empty . map parsePosition . lines
 where
  updateCounts seen coords
    | coords `elem` seen = S.delete coords seen
    | otherwise = S.insert coords seen

part2 :: String -> Int
part2 = const 2
