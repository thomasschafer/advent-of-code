module Day16 (part1, part2) where

import Data.Set (Set)
import Data.Set qualified as S
import Prelude hiding (Left, Right)

data Direction = Up | Left | Down | Right
  deriving (Eq, Ord, Show)

energisedTiles :: [String] -> ((Int, Int), Direction) -> Set (Int, Int)
energisedTiles contraption start = S.map fst $ energisedTiles' S.empty start
  where
    rows = length contraption
    cols = length $ head contraption

    energisedTiles' :: Set ((Int, Int), Direction) -> ((Int, Int), Direction) -> Set ((Int, Int), Direction)
    energisedTiles' seen coordsDir@((r, c), direction)
      | coordsDir `elem` seen || r < 0 || r >= rows || c < 0 || c >= cols = seen
      | otherwise = case contraption !! r !! c of
          '/' -> energisedTiles' seenWithCur $ case direction of
            Up -> rightCoords
            Left -> downCoords
            Right -> upCoords
            Down -> leftCoords
          '\\' -> energisedTiles' seenWithCur $ case direction of
            Up -> leftCoords
            Right -> downCoords
            Down -> rightCoords
            Left -> upCoords
          '|' ->
            if direction `elem` [Up, Down]
              then energisedTiles' seenWithCur ((if direction == Up then r - 1 else r + 1, c), direction)
              else foldl energisedTiles' seenWithCur [upCoords, downCoords]
          '-' ->
            if direction `elem` [Left, Right]
              then energisedTiles' seenWithCur ((r, if direction == Left then c - 1 else c + 1), direction)
              else foldl energisedTiles' seenWithCur [rightCoords, leftCoords]
          '.' -> energisedTiles' seenWithCur $ case direction of
            Up -> upCoords
            Left -> leftCoords
            Right -> rightCoords
            Down -> downCoords
          tile -> error ("Unexpected tile " ++ show tile)
      where
        seenWithCur = S.insert coordsDir seen
        upCoords = ((r - 1, c), Up)
        rightCoords = ((r, c + 1), Right)
        leftCoords = ((r, c - 1), Left)
        downCoords = ((r + 1, c), Down)

solve :: [String] -> [((Int, Int), Direction)] -> Int
solve s = maximum . map (length . energisedTiles s)

part1 :: String -> Int
part1 = flip solve [((0, 0), Right)] . lines

part2 :: String -> Int
part2 = (\s -> solve s (allStartPositions s)) . lines
  where
    allStartPositions s =
      [((r, 0), Right) | r <- [0 .. length s - 1]]
        ++ [((r, length (head s) - 1), Left) | r <- [0 .. length s - 1]]
        ++ [((0, c), Down) | c <- [0 .. length (head s) - 1]]
        ++ [((length s - 1, c), Up) | c <- [0 .. length (head s) - 1]]
