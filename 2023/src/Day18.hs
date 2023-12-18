module Day18 (part1, part2) where

import Data.List (find, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
import Prelude hiding (Left, Right)

data Direction = Up | Down | Left | Right
  deriving (Show)

parse :: String -> (Direction, Int, String)
parse line = (dir, read steps, tail (init colour))
  where
    [dirStr, steps, colour] = splitOn " " line
    dir = case dirStr of
      "U" -> Up
      "D" -> Down
      "L" -> Left
      "R" -> Right
      c -> error c

type Coords = (Int, Int)

findTrench :: [(Direction, Int, String)] -> Set Coords
findTrench = fst . foldl updateTrench (S.singleton (0, 0), (0, 0))
  where
    updateTrench (trench, (r, c)) (dir, steps, _) = (foldr S.insert trench path, last path)
      where
        path =
          [ case dir of
              Up -> (r - s, c)
              Down -> (r + s, c)
              Left -> (r, c - s)
              Right -> (r, c + s)
            | s <- [1 .. steps]
          ]

neighbors :: Coords -> [Coords]
neighbors (r, c) = [(r, c + 1), (r, c - 1), (r + 1, c), (r - 1, c)]

interiorArea :: Set Coords -> Int
interiorArea trench = length $ display $ S.union trench (interiorPoints S.empty start startNext)
  where
    start@(startRow, startCol) = minimum trench
    startNext = (startRow, startCol + 1)

    dfs points cur =
      if cur `elem` S.union points trench
        then points
        else foldl dfs (S.insert cur points) (neighbors cur)

    interiorPoints points prev@(prevRow, prevCol) cur@(curRow, curCol)
      | cur == start = points
      | otherwise = interiorPoints (S.union (dfs points right) points) cur next
      where
        (rowMove, colMove) = (curRow - prevRow, curCol - prevCol)
        next = fromJust $ find (\n -> n /= prev && n `elem` trench) (neighbors cur)
        right = (curRow + colMove, curCol - rowMove)

display :: Set Coords -> Set Coords
display seen = trace (intercalate "\n" [[if (r, c) `elem` seen then '#' else '.' | c <- [minimum (map snd seenList) .. maximum (map snd seenList)]] | r <- [minimum (map fst seenList) .. maximum (map fst seenList)]]) seen
  where
    seenList = S.toList seen

part1 :: String -> Int
part1 = interiorArea . display . findTrench . map parse . lines

part2 :: String -> Int
part2 = const 1
