module Day23 (part1, part2) where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (elemIndex, find)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set qualified as S

type Coords = (Int, Int)

neighbors :: Coords -> [Coords]
neighbors (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

type MapData = ((Int, Int), (Coords, Coords))

mapData :: [[Char]] -> MapData
mapData hikingMap = ((rows, cols), (start, end))
  where
    (rows, cols) = (length hikingMap, length $ head hikingMap)
    (startCol, endCol) = join (***) (fromJust . elemIndex '.') (head hikingMap, last hikingMap)
    (start, end) = ((0, startCol), (rows - 1, endCol))

longestHike :: [[Char]] -> Int
longestHike hikingMap = fromJust $ longestHike' S.empty 0 start
  where
    ((rows, cols), (start, end)) = mapData hikingMap
    longestHike' seen acc cur@(r, c)
      | cur == end = Just acc
      | null neighborPaths = Nothing
      | otherwise = Just $ maximum neighborPaths
      where
        withinBounds (r', c') = r' >= 0 && r' < rows && c' >= 0 && c' < cols
        allNeighbors =
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
              _ -> neighbors (r, c)
        neighborPaths =
          mapMaybe (longestHike' (S.insert cur seen) (acc + 1)) $
            if end `elem` allNeighbors then [end] else allNeighbors

part1 :: String -> Int
part1 = longestHike . lines

compressMap :: [[Char]] -> (HashMap Coords [(Int, Coords)], MapData)
compressMap hikingMap =
  (foldl buildMap HM.empty [(r, c) | c <- [0 .. cols - 1], r <- [0 .. rows - 1]], md)
  where
    md@((rows, cols), (start, end)) = mapData hikingMap
    validCoords =
      S.fromList
        [ (r, c)
          | r <- [0 .. rows - 1],
            c <- [0 .. cols - 1],
            hikingMap !! r !! c /= '#'
        ]
    isGraphNode (r, c) =
      (r, c) `elem` [start, end]
        || ((r, c) `elem` validCoords)
          && length (filter (`elem` validCoords) (neighbors (r, c))) > 2

    buildMap curMap cur =
      if isGraphNode cur
        then HM.insert cur nodeNeighbors curMap
        else curMap
      where
        nodeNeighbors =
          mapMaybe
            (calcEdge (S.singleton cur) 1)
            (filter (`elem` validCoords) $ neighbors cur)

    calcEdge seen acc node =
      if isGraphNode node
        then Just (acc, node)
        else case find (\n -> n `notElem` seen && n `elem` validCoords) (neighbors node) of
          Just neigh -> calcEdge (S.insert node seen) (acc + 1) neigh
          Nothing -> Nothing

longestHikeCompressed :: (HashMap Coords [(Int, Coords)], MapData) -> Int
longestHikeCompressed (graph, (_, (start, end))) =
  fromJust $ longestHikeCompressed' S.empty 0 start
  where
    longestHikeCompressed' seen acc cur
      | cur == end = Just acc
      | null neighborPaths = Nothing
      | otherwise = Just $ maximum neighborPaths
      where
        neighborPaths =
          mapMaybe
            (\(weight, neigh) -> longestHikeCompressed' (S.insert cur seen) (acc + weight) neigh)
            . filter ((`notElem` seen) . snd)
            . fromJust
            $ HM.lookup cur graph

part2 :: String -> Int
part2 = longestHikeCompressed . compressMap . lines