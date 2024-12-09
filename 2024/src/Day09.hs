module Day09 (part1, part2) where

import Data.Maybe (catMaybes, isJust, mapMaybe)

parse :: String -> [Maybe Int]
parse = concatMap expand . zip [0 ..] . pairs . map (read . (: [])) . filter (/= '\n')
  where
    pairs [files] = [(files, 0)]
    pairs (files : freeSpace : rest) = (files, freeSpace) : pairs rest

    expand (idx, (files, freeSpace)) = replicate files (Just idx) ++ replicate freeSpace Nothing

moveFiles' :: [Maybe Int] -> [Int] -> [Maybe Int]
moveFiles' [] _ = []
moveFiles' (Nothing : rest) (end : endRest) = Just end : moveFiles' rest endRest
moveFiles' (Just x : rest) ends = Just x : moveFiles' rest ends

moveFiles :: [Maybe Int] -> [Maybe Int]
moveFiles xs = moveFiles' (take n xs) (reverse . catMaybes $ drop n xs)
  where
    n = length $ filter isJust xs

checksum :: [Maybe Int] -> Int
checksum = sum . mapMaybe (\(idx, x) -> (idx *) <$> x) . zip [0 ..]

part1 :: String -> Int
part1 = checksum . moveFiles . parse

part2 :: String -> Int
part2 = const 2
