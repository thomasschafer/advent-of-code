module Day12 (part1, part2) where

import Control.Arrow (Arrow (second, (&&&)))
import Data.Set (Set)
import Data.Set qualified as S
import Utils (mapTuple)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

plots :: [[Char]] -> [Set (Int, Int)]
plots plotMap = snd $ foldl update (S.empty, []) [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
  where
    (rows, cols) = (length plotMap, length (head plotMap))

    update (visited, acc) (r, c) = second (: acc) $ search (plotMap !! r !! c) visited (r, c)

    search expectedChar visited (r, c)
      | outOfBounds (r, c) || (r, c) `elem` visited || plotMap !! r !! c /= expectedChar = (visited, S.empty)
      | otherwise = foldl upd (S.insert (r, c) visited, S.singleton (r, c)) (neighbours (r, c))
      where
        upd (vis, plot) pos = second (plot `S.union`) $ search expectedChar vis pos

    outOfBounds (r, c) = r < 0 || r >= rows || c < 0 || c >= cols

perimeter :: Bool -> Set (Int, Int) -> Int
perimeter continueSides plot = sum . map (if continueSides then continuousBorders else allBorders) $ S.toList plot
  where
    p = (`elem` plot)
    allBorders = (4 -) . length . filter (`elem` plot) . neighbours
    -- Only count borders at anticlockwise end
    continuousBorders (r, c) = length $ filter id borders
      where
        borders =
          [ not (p (r + 1, c)) && not (p (r, c + 1) && not (p (r + 1, c + 1))),
            not (p (r - 1, c)) && not (p (r, c - 1) && not (p (r - 1, c - 1))),
            not (p (r, c + 1)) && not (p (r - 1, c) && not (p (r - 1, c + 1))),
            not (p (r, c - 1)) && not (p (r + 1, c) && not (p (r + 1, c - 1)))
          ]

solve :: Bool -> String -> Int
solve continousSides s = sum . map (uncurry (*) . (length &&& perimeter continousSides)) . plots $ lines s

part1, part2 :: String -> Int
(part1, part2) = mapTuple solve (False, True)
