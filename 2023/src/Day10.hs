module Day10 (part1, part2) where

import Data.List (elemIndex, findIndex, nub)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S

findLoopLength :: [[Char]] -> Int
findLoopLength pipeData = findLoopLength' 0 sCoords (S.fromList sCoords)
  where
    leftNodes = ['-', '7', 'J', 'S']
    rightNodes = ['-', 'F', 'L', 'S']
    upNodes = ['|', 'L', 'J', 'S']
    downNodes = ['|', 'F', '7', 'S']

    sRow = fromJust $ findIndex ('S' `elem`) pipeData
    sCol = fromJust $ elemIndex 'S' (pipeData !! sRow)
    rows = length pipeData
    cols = length $ head pipeData
    sCoords = [(sRow, sCol)]

    findLoopLength' :: Int -> [(Int, Int)] -> Set (Int, Int) -> Int
    findLoopLength' acc curNodes visited
      | null curNodes = error "Found empty curNodes"
      | length (nub newNodes) /= length newNodes = acc + 1 -- two paths have arrived at the same node, loop found
      | otherwise = findLoopLength' (acc + 1) newNodes updatedVisited
      where
        validNeighbors (r, c) =
          filter (not . (`elem` visited)) $
            [(r + 1, c) | cur `elem` downNodes, r + 1 < rows, (pipeData !! (r + 1) !! c) `elem` upNodes]
              ++ [(r - 1, c) | cur `elem` upNodes, r - 1 >= 0, (pipeData !! (r - 1) !! c) `elem` downNodes]
              ++ [(r, c + 1) | cur `elem` rightNodes, c + 1 < cols, (pipeData !! r !! (c + 1)) `elem` leftNodes]
              ++ [(r, c - 1) | cur `elem` leftNodes, c - 1 >= 0, (pipeData !! r !! (c - 1)) `elem` rightNodes]
          where
            cur = pipeData !! r !! c

        newNodes = concatMap validNeighbors curNodes
        updatedVisited = foldl (\vis (r, c) -> S.insert (r, c) vis) visited newNodes

part1 :: String -> Int
part1 = findLoopLength . lines

part2 :: String -> Int
part2 = const 1
