module Day04 (part1, part2) where

import Data.List.Split (splitOn)

isValidPassport :: [(String, String)] -> Bool
isValidPassport fields = all (`elem` map fst fields) expected
  where
    expected = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part1 :: String -> Int
part1 = length . filter isValidPassport . map parsePassport . splitOn "\n\n"
  where
    parsePassport = map ((\[k, v] -> (k, v)) . splitOn ":") . words

part2 :: String -> Int
part2 = const 2