module Day05 (part1, part2) where

import Control.Arrow (Arrow (first, second))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (toTuple, (...))

type Rules = HashMap Int (Set Int)

parse :: [String] -> [String] -> (Rules, [[Int]])
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

satisfiesRules :: Rules -> [Int] -> Bool
satisfiesRules rules = satisfiesRules' S.empty
  where
    satisfiesRules' _ [] = True
    satisfiesRules' notPermitted (update : rest)
      | update `elem` notPermitted = False
      | otherwise = satisfiesRules' (notPermitted `S.union` fromMaybe S.empty (HM.lookup update rules)) rest

stripRules :: Rules -> [Int] -> Rules
stripRules rules xs = flip addMissing xs $ foldl update HM.empty (HM.toList rules)
  where
    update acc (k, v)
      | k `elem` xs = HM.insert k (S.filter (`elem` xs) v) acc
      | otherwise = acc
    addMissing = foldl $ \acc x ->
      if x `HM.member` acc
        then acc
        else HM.insert x S.empty acc

fix :: Rules -> [Int] -> [Int]
fix = fix' ... stripRules
  where
    fix' relevantRules = if null relevantRules then [] else start ++ end
      where
        (forEnd, forStart) = partition relevantRules
        partition =
          foldl
            (flip (\(k, v) -> (if null v then first else second) (HM.insert k v)))
            (HM.empty, HM.empty)
            . HM.toList
        end = map fst $ HM.toList forEnd
        start = fix' . stripRules relevantRules $ HM.keys forStart

solve :: Bool -> String -> Int
solve fixUnordered s =
  sum
    . map (middle . (if fixUnordered then fix rules else id))
    $ filter ((if fixUnordered then not else id) . satisfiesRules rules) updates
  where
    middle xs = xs !! (length xs `div` 2)
    (rules, updates) = uncurry parse . toTuple . map lines $ splitOn "\n\n" s

part1 :: String -> Int
part1 = solve False

part2 :: String -> Int
part2 = solve True
