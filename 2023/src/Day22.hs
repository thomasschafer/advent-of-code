module Day22 (part1, part2) where

import Control.Arrow ((&&&))
import Data.Bifunctor (Bifunctor (second))
import Data.Function (on)
import Data.List (find, nub, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Utils (withIdx)

parseBrick :: String -> Brick
parseBrick = (\[c1, c2] -> (c1, c2)) . map ((\[x, y, z] -> (x, y, z)) . map read . splitOn ",") . splitOn "~"

lowestPoint :: Brick -> Int
lowestPoint ((_, _, z1), (_, _, z2)) = min z1 z2

highestPoint :: Brick -> Int
highestPoint ((_, _, z1), (_, _, z2)) = max z1 z2

type Brick = ((Int, Int, Int), (Int, Int, Int))

overlaps :: Brick -> Brick -> Bool
overlaps b1 b2 = (minX b1 <= maxX b2 && minX b2 <= maxX b1) && (minY b1 <= maxY b2 && minY b2 <= maxY b1)
  where
    minX ((x1, _, _), (x2, _, _)) = min x1 x2
    maxX ((x1, _, _), (x2, _, _)) = max x1 x2
    minY ((_, y1, _), (_, y2, _)) = min y1 y2
    maxY ((_, y1, _), (_, y2, _)) = max y1 y2

dropBricks :: [Brick] -> [Brick]
dropBricks = reverse . foldr updateBricks [] . sortBy (flip compare `on` lowestPoint)
  where
    updateBricks brick@((x1, y1, z1), (x2, y2, z2)) bricks =
      ((x1, y1, z1 - dropDist), (x2, y2, z2 - dropDist)) : bricks
      where
        bricksUnderneath = map highestPoint $ filter (overlaps brick) bricks
        dropDist =
          min z1 z2 - case bricksUnderneath of
            [] -> 1
            bs -> maximum bs + 1

numBricksSupportedBy :: [Brick] -> [(Int, [Int])]
numBricksSupportedBy dropped =
  map
    ( \(idx1, b1) ->
        ( idx1,
          mapMaybe
            (\(idx2, b2) -> if b1 `supports` b2 then Just idx2 else Nothing)
            (withIdx $ take idx1 dropped)
        )
    )
    (withIdx dropped)
  where
    b1 `supports` b2 = overlaps b1 b2 && lowestPoint b1 == highestPoint b2 + 1

cannotDisintegrate :: [Brick] -> [Int]
cannotDisintegrate = nub . concat . filter ((== 1) . length) . map snd . numBricksSupportedBy

part1 :: String -> Int
part1 = uncurry (-) . (length &&& (length . cannotDisintegrate)) . dropBricks . map parseBrick . lines

numBricksDrop :: [(Int, [Int])] -> Int -> Int
numBricksDrop supports bId = numBricksDrop' (mapMaybe mapBricks supports) (-1)
  where
    mapBricks (brick, supportedBy)
      | brick == bId = Just (brick, [])
      | null supportedBy = Nothing
      | otherwise = Just (brick, supportedBy)

numBricksDrop' :: [(Int, [Int])] -> Int -> Int
numBricksDrop' supports acc = case find (null . snd) supports of
  Nothing -> acc
  Just (brickId, _) ->
    numBricksDrop'
      (map (second (filter (/= brickId))) $ filter ((/= brickId) . fst) supports)
      (acc + 1)

part2 :: String -> Int
part2 brickData = sum $ map (numBricksDrop supports) toCheck
  where
    supports = numBricksSupportedBy . dropBricks . map parseBrick $ lines brickData
    toCheck = nub . concat . filter ((== 1) . length) $ map snd supports
