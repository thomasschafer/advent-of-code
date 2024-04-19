module Day20 (part1, part2) where

import Control.Monad (join)
import Data.List (find, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import Utils (quickTrace, setAt3d)

type Layout = [String]
type Tile = (Int, Layout)

parse :: String -> [Tile]
parse =
  map ((\t -> (parseId $ head t, tail t)) . splitOn "\n")
    . splitOn "\n\n"
 where
  parseId = read . init . last . words

rotate :: [[a]] -> [[a]]
rotate = transpose . reverse

orientationFuncs :: [[[a]] -> [[a]]]
orientationFuncs = rotations ++ map (. transpose) rotations
 where
  rotations = take 4 $ iterate (rotate .) id

orientations :: [[a]] -> [[[a]]]
orientations = flip map orientationFuncs . flip ($)

matchingOrientations :: (Int, Int) -> [[Maybe Tile]] -> Tile -> [Tile] -- returns matching tiles in correct orientation
matchingOrientations (r, c) solution = filter (matches . snd) . allOrientations
 where
  allOrientations (tileId, layout) = [(tileId, layout') | layout' <- orientations layout]

  matches layout = (r == 0 || last layoutAbove == head layout) && (c == 0 || map last layoutLeft == map head layout)
   where
    layoutAbove = snd . fromJust $ solution !! (r - 1) !! c
    layoutLeft = snd . fromJust $ solution !! r !! (c - 1)

findLayout :: [Tile] -> [[Tile]]
findLayout tiles = fromJust $ findLayout' (0, 0) (replicate sideLength $ replicate sideLength Nothing) tiles
 where
  sideLength = round . sqrt . fromIntegral $ length tiles

  findLayout' pos@(r, c) solution remainingTiles =
    if r >= sideLength
      then Just $ map (map fromJust) solution
      else
        join
          . find isJust
          . map search
          $ concatMap (matchingOrientations pos solution) remainingTiles
   where
    nextPos = if c == sideLength - 1 then (r + 1, 0) else (r, c + 1)

    search t = findLayout' nextPos updatedSolution remaining
     where
      updatedSolution = setAt3d pos (Just t) solution
      remaining = filter ((/= fst t) . fst) remainingTiles

part1 :: String -> Int
part1 = product . map fst . corners . findLayout . parse
 where
  corners = flip map [head . head, head . last, last . head, last . last] . flip ($)

combine :: [[Tile]] -> [String]
combine = combineLayouts . map (map $ trimLayout . snd)
 where
  trimLayout = map (init . tail) . init . tail
  combineLayouts :: [[Layout]] -> [String]
  combineLayouts = concatMap (\r -> [concat [l !! idx | l <- r] | idx <- [0 .. length (head r) - 1]])

removeSeaMonsters :: [String] -> [String]
removeSeaMonsters = id

seaRoughness :: [String] -> Int
seaRoughness = length . filter (== '#') . concat . removeSeaMonsters

part2 :: String -> Int
part2 = minimum . map seaRoughness . take 4 . iterate rotate . combine . findLayout . parse
