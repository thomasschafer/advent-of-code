module Day12 (part1, part2) where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Utils (toTuple)

type Present = [[Bool]]

type Region = ((Int, Int), [Int])

rotations :: [[a]] -> [[[a]]]
rotations = take 4 . iterate (map reverse . transpose)

parse :: String -> ([Present], [Region])
parse s = bimap (map parsePresent) (map parseRegion) . (init &&& last) . map lines $ splitOn "\n\n" s
  where
    parseRegion = bimap (toTuple . map read . splitOn "x") (map read . words) . toTuple . splitOn ": "
    parsePresent = map (map (== '#')) . tail

canFitPresents :: [Present] -> Region -> Bool
canFitPresents presents ((width, height), initialReq)
  | length (filter id . concat . concat $ initialPresents) > width * height = False
  | sum (map (\p -> length p * length (head p)) initialPresents) <= width * height = True
  | otherwise = go initialPresents initialGrid
  where
    initialPresents = map (presents !!) . concatMap (uncurry replicate) $ zip initialReq [0 ..]
    initialGrid = replicate height (replicate width False)

    go [] _ = True
    go (present : rest) grid = any (go rest) updatedGrids
      where
        perms = S.fromList $ rotations present ++ rotations (transpose present)
        updatedGrids = S.fromList $ concatMap (allPlacements grid) perms

    allPlacements grid present =
      mapMaybe
        place
        [ (i, j)
        | i <- [0 .. height - presentHeight],
          j <- [0 .. width - presentWidth],
          not (grid !! i !! j)
        ]
      where
        presentHeight = length present
        presentWidth = length $ head present

        place (i, j)
          | any (any (> 1)) combined = Nothing
          | otherwise = Just $ map (map toEnum) combined
          where
            combined =
              [ [ fromEnum (grid !! r !! c)
                    + fromEnum (safeLookup (r - i, c - j))
                | c <- [0 .. width - 1]
                ]
              | r <- [0 .. height - 1]
              ]

        safeLookup (i, j) = i >= 0 && i < presentHeight && j >= 0 && j < presentWidth && present !! i !! j

part1 :: String -> Int
part1 = length . uncurry (filter . canFitPresents) . parse

part2 :: String -> Int
part2 = const 2
