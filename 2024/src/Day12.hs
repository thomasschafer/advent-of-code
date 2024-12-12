module Day12 (part1, part2) where

import Control.Arrow (Arrow (second))
import Data.Bifunctor (bimap)
import Data.Set qualified as S

part1 :: String -> Int
part1 s = sum . map (uncurry (*)) . snd $ foldl update (S.empty, []) [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
  where
    plotMap = lines s
    (rows, cols) = (length plotMap, length (head plotMap))

    update (visited, acc) (r, c)
      | (r, c) `elem` visited = (visited, acc) -- TODO: delete this??
      | otherwise = second (: acc) $ search (plotMap !! r !! c) visited (r, c)

    search expectedChar = search'
      where
        search' visited (r, c)
          | outOfBounds (r, c) || (r, c) `elem` visited || plotMap !! r !! c /= expectedChar = (visited, (0, 0))
          | otherwise = foldl upd (S.insert (r, c) visited, (1, 4 - length neighbors)) neighbors
          where
            neighbors = filter (\(r', c') -> not (outOfBounds (r', c')) && plotMap !! r' !! c' == expectedChar) [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
            upd (vis, (a, p)) (r', c') = second (bimap (+ a) (+ p)) $ search' vis (r', c')

    outOfBounds (r, c) = r < 0 || r >= rows || c < 0 || c >= cols

part2 :: String -> Int
part2 = const 2
