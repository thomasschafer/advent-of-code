module Day06 (part1, part2) where

import Data.Set (Set)
import Data.Set qualified as S
import Prelude hiding (Left, Right)

data Direction = Up | Down | Left | Right

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

positionsVisited :: ((Int, Int), Direction) -> [[Bool]] -> Set (Int, Int)
positionsVisited (startPos, startDir) blockedMap = positionsVisited' S.empty startPos startDir
  where
    (rows, cols) = (length blockedMap, length (head blockedMap))

    positionsVisited' visited pos@(r, c) dir
      | outOfBounds nextPos = updatedVisited
      | otherwise = positionsVisited' updatedVisited nextPos nextDir
      where
        updatedVisited = S.insert pos visited
        (nextPos, nextDir) = case dir of
          Up -> up `orIfBlocked` right
          Right -> right `orIfBlocked` down
          Down -> down `orIfBlocked` left
          Left -> left `orIfBlocked` up
          where
            up = ((r - 1, c), Up)
            right = ((r, c + 1), Right)
            down = ((r + 1, c), Down)
            left = ((r, c - 1), Left)

    orIfBlocked pos1@((r1, c1), _) pos2
      | outOfBounds (r1, c1) || not (blockedMap !! r1 !! c1) = pos1
      | otherwise = pos2

    outOfBounds (r, c) = r < 0 || r >= rows || c < 0 || c >= cols

part1 :: String -> Int
part1 = length . uncurry positionsVisited . parse . lines

part2 :: String -> Int
part2 = const 2
