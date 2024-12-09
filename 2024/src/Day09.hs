module Day09 (part1, part2) where

import Data.Maybe (catMaybes, isJust, mapMaybe)

parse :: String -> [(Int, Maybe Int)]
parse = concatMap f . zip [0 ..] . pairs . map (read . (: [])) . filter (/= '\n')
  where
    f (idx, (files, freeSpace)) = [(files, Just idx), (freeSpace, Nothing)]
    pairs [files] = [(files, 0)]
    pairs (files : freeSpace : rest) = (files, freeSpace) : pairs rest

moveFilesFrag :: [Maybe Int] -> [Maybe Int]
moveFilesFrag files = moveFilesFrag' (take n files) (reverse . catMaybes $ drop n files)
  where
    n = length $ filter isJust files

    moveFilesFrag' [] _ = []
    moveFilesFrag' (Nothing : rest) (end : endRest) = Just end : moveFilesFrag' rest endRest
    moveFilesFrag' (Just x : rest) ends = Just x : moveFilesFrag' rest ends

pullFileForward :: [(Int, Maybe Int)] -> (Int, Int) -> [(Int, Maybe Int)]
pullFileForward files (count, fileId) = update' files
  where
    update' [] = []
    update' (cur@(c, Nothing) : rest)
      | c >= count = ((count, Just fileId) :) . ((c - count, Nothing) :) . flip map rest $ \case
          (c', Just f) | f == fileId -> (c', Nothing)
          x -> x
      | otherwise = cur : update' rest
    update' (cur@(_, Just f) : rest)
      | f == fileId = cur : rest
      | otherwise = cur : update' rest

moveFilesWhole :: [(Int, Maybe Int)] -> [(Int, Maybe Int)]
moveFilesWhole files = foldl pullFileForward files . reverse . flip mapMaybe files $ \case
  (_, Nothing) -> Nothing
  (count, Just fileId) -> Just (count, fileId)

flatten :: [(Int, Maybe Int)] -> [Maybe Int]
flatten = concatMap (uncurry replicate)

checksum :: [Maybe Int] -> Int
checksum = sum . mapMaybe (\(idx, x) -> (idx *) <$> x) . zip [0 ..]

part1, part2 :: String -> Int
part1 = checksum . moveFilesFrag . flatten . parse
part2 = checksum . flatten . moveFilesWhole . parse
