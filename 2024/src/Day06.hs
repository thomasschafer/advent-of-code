module Day06 (part1, part2) where

import Data.Set (Set)
import Data.Set qualified as S
import Utils (setAt2d)
import Prelude hiding (Left, Right)

data Direction = Up | Down | Left | Right
  deriving (Eq, Ord)

parse :: [String] -> (((Int, Int), Direction), [[Bool]])
parse s = ((startPos, Up), positions)
  where
    positions = map (map (== '#')) s
    startPos =
      head
        [ (r, c)
          | r <- [0 .. length s - 1],
            c <- [0 .. length (head s) - 1],
            s !! r !! c == '^'
        ]

positionsVisited :: ((Int, Int), Direction) -> [[Bool]] -> (Set ((Int, Int), Direction), Bool)
positionsVisited (startPos, startDir) blockedMap = positionsVisited' S.empty startPos startDir
  where
    (rows, cols) = (length blockedMap, length (head blockedMap))

    positionsVisited' visited pos@(r, c) dir
      | outOfBounds nextPos = (updatedVisited, False)
      | (nextPos, nextDir) `elem` visited = (updatedVisited, True)
      | otherwise = positionsVisited' updatedVisited nextPos nextDir
      where
        updatedVisited = S.insert (pos, dir) visited
        (nextPos, nextDir) = case dir of
          Up -> ((r - 1, c), Up) `orIfBlocked` ((r, c), Right)
          Right -> ((r, c + 1), Right) `orIfBlocked` ((r, c), Down)
          Down -> ((r + 1, c), Down) `orIfBlocked` ((r, c), Left)
          Left -> ((r, c - 1), Left) `orIfBlocked` ((r, c), Up)

    orIfBlocked pos1@((r1, c1), _) pos2
      | outOfBounds (r1, c1) || not (blockedMap !! r1 !! c1) = pos1
      | otherwise = pos2

    outOfBounds (r, c) = r < 0 || r >= rows || c < 0 || c >= cols

part1 :: String -> Int
part1 = length . fst . uncurry positionsVisited . parse . lines

part2 :: String -> Int
part2 s = length $ S.filter isValidBlocker initialPath
  where
    (start@(startPos, _), blockedMap) = parse $ lines s
    initialPath = S.map fst . fst $ positionsVisited start blockedMap
    isValidBlocker pos = pos /= startPos && (snd . positionsVisited start) (setAt2d pos True blockedMap)
