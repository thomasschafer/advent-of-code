module Day24 (part1, part2) where

import Data.List (intercalate)
import Data.Set (Set)
import Data.Set qualified as S

-- Distance (north, east)
-- e or w moves two units in given direction, ne/nw/se/sw moves one unit in both given directions
type Coords = (Int, Int)

moveEast, moveWest, moveSouthEast, moveSouthWest, moveNorthEast, moveNorthWest :: Coords -> Coords
moveEast (n, e) = (n, e + 2)
moveWest (n, e) = (n, e - 2)
moveSouthEast (n, e) = (n - 1, e + 1)
moveSouthWest (n, e) = (n - 1, e - 1)
moveNorthEast (n, e) = (n + 1, e + 1)
moveNorthWest (n, e) = (n + 1, e - 1)

parsePosition :: String -> Coords
parsePosition = parsePosition' (0, 0)
 where
  parsePosition' coords = \case
    ('e' : rest) -> parsePosition' (moveEast coords) rest
    ('w' : rest) -> parsePosition' (moveWest coords) rest
    ('s' : 'e' : rest) -> parsePosition' (moveSouthEast coords) rest
    ('s' : 'w' : rest) -> parsePosition' (moveSouthWest coords) rest
    ('n' : 'e' : rest) -> parsePosition' (moveNorthEast coords) rest
    ('n' : 'w' : rest) -> parsePosition' (moveNorthWest coords) rest
    [] -> coords

neighbors :: Coords -> [Coords]
neighbors = (`map` [moveEast, moveWest, moveSouthEast, moveSouthWest, moveNorthEast, moveNorthWest]) . flip ($)

flipTiles :: Set Coords -> Set Coords
flipTiles blackTiles = remainingBlackTiles `S.union` newBlackTiles
 where
  blackNeighbors = length . filter (`elem` blackTiles) . neighbors

  remainingBlackTiles = S.filter ((`elem` [1, 2]) . blackNeighbors) blackTiles

  newBlackTiles =
    S.filter (\tile -> tile `notElem` blackTiles && blackNeighbors tile == 2) $
      foldl (flip $ S.union . S.fromList . neighbors) S.empty blackTiles

solve :: String -> [Int]
solve =
  map length
    . iterate flipTiles
    . foldl updateBlackTiles S.empty
    . map parsePosition
    . lines
 where
  updateBlackTiles seen coords
    | coords `elem` seen = S.delete coords seen
    | otherwise = S.insert coords seen

part1 :: String -> Int
part1 = head . solve

part2 :: String -> Int
part2 = (!! 100) . solve
