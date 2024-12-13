module Day13 (part1, part2) where

import Control.Arrow (Arrow (first))
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Utils (revTuple, to3Tuple, toTuple)

parse :: String -> ((Int, Int), (Int, Int), (Int, Int))
parse = to3Tuple . map (toTuple . map (read . drop 2) . splitOn ", " . last . splitOn ": ") . lines

subtractTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
subtractTuple (ax, ay) (bx, by) = (ax - bx, ay - by)

minTokensToWin :: ((Int, Int), (Int, Int), (Int, Int)) -> Maybe Int
minTokensToWin (a, b, p) = snd $ minTokensToWin' HM.empty p
  where
    minTokensToWin' cache prize@(prizeX, prizeY)
      | min prizeX prizeY < 0 = (cache, Nothing)
      | prizeX == 0 && prizeY == 0 = (cache, Just 0)
      | otherwise = case HM.lookup prize cache of
          Just res -> (cache, res)
          Nothing -> (HM.insert prize minCost updatedCache, minCost)
      where
        (costs, updatedCache) = foldl update ([], cache) [(a, 3), (b, 1)]
        minCost = if null costs then Nothing else Just (minimum costs)

        update (acc, cache) (move, cost) = first addIfJust . revTuple $ minTokensToWin' cache (subtractTuple prize move)
          where
            addIfJust = \case
              Nothing -> acc
              Just subCost -> (cost + subCost) : acc

part1 :: String -> Int
part1 = sum . mapMaybe (minTokensToWin . parse) . splitOn "\n\n"

part2 :: String -> Int
part2 = const 2
