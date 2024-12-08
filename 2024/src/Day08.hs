module Day08 (part1, part2) where

import Data.Set (Set)
import Data.Set qualified as S
import Utils (groupBy)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

part1 :: String -> Int
part1 s = length . foldl1 S.union . map (antinodes . map fst) $ groupBy snd coords
  where
    antennaMap = lines s
    (rows, cols) = (length antennaMap, length (head antennaMap))
    coords = [((row, col), char) | row <- [0 .. rows - 1], col <- [0 .. cols - 1], let char = antennaMap !! row !! col, char /= '.']

    antinodes :: [(Int, Int)] -> Set (Int, Int)
    antinodes = foldl1 S.union . map (S.fromList . uncurry antinodes') . pairs
      where
        withinBounds (r, c) = r >= 0 && r < rows && c >= 0 && c < cols
        antinodes' (r1, c1) (r2, c2) = filter withinBounds [(r1 - dr, c1 - dc), (r2 + dr, c2 + dc)]
          where
            (dr, dc) = (r2 - r1, c2 - c1)

part2 :: String -> Int
part2 = const 2
