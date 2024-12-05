module Day05 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (toTuple)

parse :: [String] -> [String] -> (HashMap Int (Set Int), [[Int]])
parse rules updates =
  ( foldl update HM.empty $ map (toTuple . map read . splitOn "|") rules,
    map (map read . splitOn ",") updates
  )
  where
    update acc (before, after) =
      HM.insert
        after
        (before `S.insert` fromMaybe S.empty (HM.lookup after acc))
        acc

satisfiesRules :: HashMap Int (Set Int) -> [Int] -> Bool
satisfiesRules rules = satisfiesRules' S.empty
  where
    satisfiesRules' _ [] = True
    satisfiesRules' notPermitted (update : rest)
      | update `elem` notPermitted = False
      | otherwise = satisfiesRules' (notPermitted `S.union` fromMaybe S.empty (HM.lookup update rules)) rest

part1 :: String -> Int
part1 s = sum . map middle $ filter (satisfiesRules rules) updates
  where
    middle xs = xs !! (length xs `div` 2)
    (rules, updates) = uncurry parse . toTuple . map lines $ splitOn "\n\n" s

part2 :: String -> Int
part2 = const 2
