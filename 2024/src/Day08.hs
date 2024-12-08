module Day08 (part1, part2) where

import Data.Set qualified as S
import Utils (groupBy)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

solve :: Bool -> String -> Int
solve constrainDist s = length . foldl1 S.union . map (antinodes . map fst) $ groupBy snd coords
  where
    antennaMap = lines s
    (rows, cols) = (length antennaMap, length (head antennaMap))
    coords = [((row, col), char) | row <- [0 .. rows - 1], col <- [0 .. cols - 1], let char = antennaMap !! row !! col, char /= '.']
    withinBounds (r, c) = r >= 0 && r < rows && c >= 0 && c < cols

    antinodes = foldl1 S.union . map (S.fromList . uncurry antinodes') . pairs
      where
        antinodes' (r1, c1) (r2, c2) = filter withinBounds [(r1 + n * dr, c1 + n * dc) | n <- distFromFirst]
          where
            (dr, dc) = (r2 - r1, c2 - c1)
            distFromFirst
              | constrainDist = [-1, 2]
              | otherwise = [-m .. m]
              where
                m = max rows cols

part1 :: String -> Int
part1 = solve True

part2 :: String -> Int
part2 = solve False
