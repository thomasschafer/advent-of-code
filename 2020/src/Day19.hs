module Day19 (part1, part2) where

import Control.Arrow (Arrow (first))
import Data.Bifunctor (Bifunctor (bimap))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Utils (toTuple, (...))

data Rule
  = Val Char
  | Other Int
  | List [Rule]
  | Or [Rule]
  deriving (Show)

parse :: String -> (HashMap Int Rule, [String])
parse = first parseRulesMap . toTuple . map lines . splitOn "\n\n"
 where
  parseRulesMap = HM.fromList . map (bimap read (parseRules . words) . toTuple . splitOn ": ")

  parseRules ['"' : c : ['"']] = Val c
  parseRules rs
    | "|" `elem` rs = Or . map parseRules $ splitOn ["|"] rs
    | otherwise = List $ map (Other . read) rs

satisfiesRule :: HashMap Int Rule -> Rule -> String -> Bool
satisfiesRule rulesMap = ([] `elem`) ... satisfiesRule'
 where
  -- return value of satisfiesRule' is a list of the results of successful matching
  satisfiesRule' (Val c) s = [tail s | not (null s) && head s == c]
  satisfiesRule' (Or rules) s = concatMap (`satisfiesRule'` s) rules
  satisfiesRule' (List []) s = [s]
  satisfiesRule' (List (rule : rules)) s =
    concatMap (satisfiesRule' $ List rules) $
      satisfiesRule' rule s
  satisfiesRule' (Other ruleNum) s = (`satisfiesRule'` s) . fromJust $ HM.lookup ruleNum rulesMap

part1 :: String -> Int
part1 s = length $ filter (satisfiesRule rulesMap . fromJust $ HM.lookup 0 rulesMap) strs
 where
  (rulesMap, strs) = parse s

part2 :: String -> Int
part2 = const 2
