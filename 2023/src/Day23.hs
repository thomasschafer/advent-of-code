module Day23 (part1, part2) where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set qualified as S

longestHike :: [[Char]] -> Int
longestHike hikingMap = longestHike' S.empty 0 start
  where
    (rows, cols) = (length hikingMap, length $ head hikingMap)
    (startCol, endCol) = join (***) (fromJust . elemIndex '.') (head hikingMap, last hikingMap)
    (start, end) = ((0, startCol), (rows - 1, endCol))
    longestHike' seen acc cur@(r, c)
      | cur == end || null neighbors = acc
      | otherwise = maximum $ map (longestHike' (S.insert cur seen) (acc + 1)) neighbors
      where
        withinBounds (r', c') = r' >= 0 && r' < rows && c' >= 0 && c' < cols
        neighbors =
          filter
            ( \(r', c') ->
                withinBounds (r', c')
                  && hikingMap !! r' !! c' /= '#'
                  && (r', c') `notElem` seen
            )
            $ case hikingMap !! r !! c of
              '<' -> [(r, c - 1)]
              '>' -> [(r, c + 1)]
              'v' -> [(r + 1, c)]
              '^' -> [(r - 1, c)]
              _ -> [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

part1 :: String -> Int
part1 = longestHike . lines

part2 :: String -> Int
part2 = const 1