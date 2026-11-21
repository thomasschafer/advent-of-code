module Main where

import Control.Arrow (Arrow (first, second), (&&&))
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Foldable (maximumBy)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (mapAccumL)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

parse :: String -> (Int, Int)
parse = toTuple . map read . splitOn ", "
  where
    toTuple [a, b] = (a, b)

data Owner = Point (Int, Int) | Tied

largestFiniteTerritory :: [(Int, Int)] -> Int
largestFiniteTerritory points = go (HM.map Point initialOwners) (HM.map S.singleton initialOwners) S.empty
  where
    initialOwners = HM.fromList $ map (join (,)) points
    ((lx, gx), (ly, gy)) = join bimap (minimum &&& maximum) (unzip points)
    outsideBounds (x, y) = x < lx || x > gx || y < ly || y > gy

    go :: HashMap (Int, Int) Owner -> HashMap (Int, Int) (Set (Int, Int)) -> Set (Int, Int) -> Int
    go owners frontiers infinite
      | null frontiers = largestTerritory . HM.filter (`S.notMember` infinite) $ HM.mapMaybe fromPoint owners
      | otherwise = go newOwners newValidFrontiers newInfinite
      where
        fromPoint (Point p) = Just p
        fromPoint Tied = Nothing

        newFrontierClaims = HM.map expand frontiers
          where
            expand :: Set (Int, Int) -> Set (Int, Int)
            expand frontier = S.fromList [(x', y') | (x, y) <- S.toList frontier, (dx, dy) <- [(0, 1), (1, 0), (0, -1), (-1, 0)], let x' = x + dx, let y' = y + dy, not ((x', y') `HM.member` owners)]

        duplicateFrontiers = snd $ HM.foldr update (S.empty, S.empty) newFrontierClaims
          where
            update frontier (seen, duplicates) = (S.union frontier seen, S.union duplicates (S.intersection frontier seen))

        (newInfinite, newValidFrontiers) = second (HM.fromList . filter (not . null . snd)) . mapAccumL update infinite $ HM.toList newFrontierClaims
          where
            update :: Set (Int, Int) -> ((Int, Int), Set (Int, Int)) -> (Set (Int, Int), ((Int, Int), Set (Int, Int)))
            update inf (p, front) = (inf', (p, front'))
              where
                inf' = (if any outsideBounds front then S.insert p else id) inf
                front' = S.filter (\x -> not (outsideBounds x || S.member x duplicateFrontiers)) front

        newOwners =
          owners
            `HM.union` HM.fromList [(v, Point k) | (k, vs) <- HM.toList newValidFrontiers, v <- S.toList vs]
            `HM.union` (HM.fromList . map (,Tied) $ S.toList duplicateFrontiers)

largestTerritory :: HashMap (Int, Int) (Int, Int) -> Int
largestTerritory = maximum . map S.size . HM.elems . HM.fromListWith S.union . map (second S.singleton . swap) . HM.toList

part1 :: String -> Int
part1 = largestFiniteTerritory . map parse . lines

numPointsWithinDist :: Int -> [(Int, Int)] -> Int
numPointsWithinDist d points = length $ filter withinDist [(x, y) | x <- [lx .. gx], y <- [ly .. gy]]
  where
    ((lx, gx), (ly, gy)) = join bimap (minimum &&& maximum) (unzip points)
    -- technically there could be valid points outside of the bounding box but this should work on our input
    withinDist (x, y) = sum [abs (x - px) + abs (y - py) | (px, py) <- points] < d

part2 :: String -> Int
part2 = numPointsWithinDist 10000 . map parse . lines
