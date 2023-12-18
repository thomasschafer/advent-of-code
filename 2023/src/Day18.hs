module Day18 (part1, part2) where

import Data.Char (digitToInt)
import Data.List (sort)
import Data.List.Split (splitOn)
import Utils (hexStringToInt)
import Prelude hiding (Left, Right)

data Direction = Up | Down | Left | Right
  deriving (Eq, Show)

parse1 :: String -> (Direction, Int)
parse1 line = (direction, read steps)
  where
    [directionStr, steps, _] = splitOn " " line
    direction = case directionStr of
      "U" -> Up
      "D" -> Down
      "L" -> Left
      "R" -> Right
      c -> error c

parse2 :: String -> (Direction, Int)
parse2 line = (direction, hexStringToInt (init colourAndDir))
  where
    colourAndDir = init $ drop 2 $ last $ splitOn " " line
    direction = case digitToInt (last colourAndDir) of
      0 -> Right
      1 -> Down
      2 -> Left
      3 -> Up
      x -> error ("Couldn't parse " ++ show x)

type Coords = (Int, Int)

findTrench :: [(Direction, Int)] -> [(Direction, Coords, Coords)]
findTrench = tail . reverse . foldl updateTrench [(Up, (0, 0), (0, 0))]
  where
    updateTrench trench (dir, steps) = (dir, start, end) : trench
      where
        (_, _, start@(r, c)) = head trench
        end = case dir of
          Up -> (r - steps, c)
          Down -> (r + steps, c)
          Left -> (r, c - steps)
          Right -> (r, c + steps)

data Wall = Vertical Int | Horizontal Int Int -- Horizontal should be ordered (Horizontal start end)
  deriving (Eq, Show)

instance Ord Wall where
  Vertical a <= Vertical b = a <= b
  Horizontal c1 _ <= Horizontal c2 _ = c1 <= c2
  Vertical a <= Horizontal c1 _ = a <= c1
  Horizontal c _ <= Vertical a = c <= a

interiorArea :: [(Direction, Coords, Coords)] -> Int
interiorArea trench = sum $ map rowArea [minimum allRows .. maximum allRows]
  where
    allRows = map (\(_, (r, _), _) -> r) trench

    rowArea row = fst $ foldl update (0, False) (zip (Nothing : wallsToConsider) wallsToConsider)
      where
        verticalWalls =
          map (\(_, (_, c), (_, _)) -> Vertical c) $
            filter
              (\(d, (r1, _), (r2, _)) -> d `elem` [Up, Down] && min r1 r2 < row && max r1 r2 > row)
              trench
        verticalWallsAtEnds =
          filter
            (\(d, (r1, _), (r2, _)) -> d `elem` [Up, Down] && (row `elem` [r1, r2]))
            trench
        horizontalWalls =
          map (\(_, (_, c1), (_, c2)) -> Horizontal (min c1 c2) (max c1 c2)) $
            filter
              (\(d, (r, _), _) -> d `elem` [Left, Right] && r == row)
              trench
        wallsToConsider = map Just . sort $ verticalWalls ++ horizontalWalls

        distToPrev prev colStart =
          case prev of
            Nothing -> 0
            Just (Vertical prevCol) -> colStart - prevCol - 1
            Just (Horizontal _ prevCol) -> colStart - prevCol - 1

        update (acc, insideTrench) (prev, Just (Vertical curCol)) =
          ( acc + 1 + (if insideTrench then distToPrev prev curCol else 0),
            not insideTrench
          )
        update (acc, insideTrench) (prev, Just (Horizontal colStart colEnd)) =
          ( acc + (colEnd - colStart + 1) + (if insideTrench then distToPrev prev colStart else 0),
            if d1 == d2 then not insideTrench else insideTrench
          )
          where
            [(d1, _, _), (d2, _, _)] =
              filter
                (\(_, start, end) -> start `elem` [(row, colStart), (row, colEnd)] || end `elem` [(row, colStart), (row, colEnd)])
                verticalWallsAtEnds
        update (acc, insideTrench) (prev, Nothing) = error ("Error in update: acc = " ++ show acc ++ ", insideTrench = " ++ show insideTrench ++ ", prev = " ++ show prev)

solve :: (String -> (Direction, Int)) -> String -> Int
solve parse = interiorArea . findTrench . map parse . lines

part1 :: String -> Int
part1 = solve parse1

part2 :: String -> Int
part2 = solve parse2
