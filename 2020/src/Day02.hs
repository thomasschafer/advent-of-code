module Day02 (part1, part2) where

import Data.List.Split (splitOn)

type PasswordData = ((Int, Int), Char, String)

parseLine :: String -> PasswordData
parseLine s = ((x1, x2), c, password)
  where
    [countStr, [c, _], password] = words s
    [x1, x2] = map read $ splitOn "-" countStr

isValid1 :: PasswordData -> Bool
isValid1 ((minCount, maxCount), c, password) =
  count >= minCount && count <= maxCount
  where
    count = length $ filter (== c) password

solve :: (PasswordData -> Bool) -> String -> Int
solve p = length . filter p . map parseLine . lines

part1 :: String -> Int
part1 = solve isValid1

isValid2 :: PasswordData -> Bool
isValid2 ((idx1, idx2), c, password) =
  (== 1) . length $ filter (== c) [password !! (i - 1) | i <- [idx1, idx2]]

part2 :: String -> Int
part2 = solve isValid2
