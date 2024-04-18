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

parse :: [String] -> String -> (HashMap Int Rule, [String])
parse rulesToAdd = first (parseRulesMap . (++ rulesToAdd)) . toTuple . map lines . splitOn "\n\n"
 where
  parseRulesMap = HM.fromList . map (bimap read (parseRules . words) . toTuple . splitOn ": ")

  parseRules ['"' : c : ['"']] = Val c
  parseRules rs
    | "|" `elem` rs = Or . map parseRules $ splitOn ["|"] rs
    | otherwise = List $ map (Other . read) rs

satisfiesRule :: HashMap Int Rule -> Rule -> String -> Bool
satisfiesRule rulesMap = ([] `elem`) ... satisfiesRule'
 where
  -- The return value of satisfiesRule' is a list of the remaining strings after successfully matching some
  -- amount of the input. An empty list means matching was impossible.
  satisfiesRule' (Val c) s = [tail s | not (null s) && head s == c]
  satisfiesRule' (Or rules) s = concatMap (`satisfiesRule'` s) rules
  satisfiesRule' (List []) s = [s]
  satisfiesRule' (List (rule : rules)) s =
    concatMap (satisfiesRule' $ List rules) $
      satisfiesRule' rule s
  satisfiesRule' (Other ruleNum) s = (`satisfiesRule'` s) . fromJust $ HM.lookup ruleNum rulesMap

solve :: [String] -> String -> Int
solve rulesToAdd s = length $ filter (satisfiesRule rulesMap . fromJust $ HM.lookup 0 rulesMap) strs
 where
  (rulesMap, strs) = parse rulesToAdd s

part1 :: String -> Int
part1 = solve []

part2 :: String -> Int
part2 =
  solve
    [ "8: 42 | 42 8"
    , "11: 42 31 | 42 11 31"
    ]
