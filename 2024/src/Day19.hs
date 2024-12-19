module Day19 (part1, part2) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Utils (toTuple)

parse :: String -> ([String], [String])
parse = bimap (splitOn ", ") lines . toTuple . splitOn "\n\n"

part1 :: String -> Int
part1 s = length $ filter (isPossible towels) designs
  where
    (towels, designs) = parse s

isPossible :: [String] -> String -> Bool
isPossible _ [] = True
isPossible towels design =
  any
    (\towel -> towel `isPrefixOf` design && isPossible towels (drop (length towel) design))
    towels

part2 :: String -> Int
part2 = const 2
