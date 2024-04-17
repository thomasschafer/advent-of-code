module Day19 (part1, part2) where

import Control.Arrow (Arrow (first))
import Data.Bifunctor (Bifunctor (bimap))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Utils (toTuple)

data Rule
  = Val Char
  | Other Int
  | List [Rule]
  | Or Rule Rule
  deriving (Show)

parse :: String -> (HashMap Int Rule, [String])
parse = first parseRulesMap . toTuple . map lines . splitOn "\n\n"
 where
  parseRulesMap = HM.fromList . map (bimap read (parseRules . words) . toTuple . splitOn ": ")

  parseRules rs
    | "|" `elem` rs = uncurry Or . toTuple . map parseRules $ splitOn ["|"] rs
    | length rs == 1 && length (head rs) == 3 && head (head rs) == '"' = Val $ head rs !! 1
    | otherwise = List $ map (Other . read) rs

satisfiesRule :: Rule -> String -> Bool
satisfiesRule = undefined

part1 :: String -> Int
part1 s = length $ filter (satisfiesRule . fromJust $ HM.lookup 0 rules) strs
 where
  (rules, strs) = parse s

part2 :: String -> Int
part2 = const 2
