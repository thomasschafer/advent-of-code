module Day05 (day05Main) where

import Data.List.Split (splitOn)

type Mapping = ((Int, Int), Int) -- ((sourceStart, sourceEnd), amountToUpdate)

parseSeedsAndMappings :: String -> ([Int], [[Mapping]])
parseSeedsAndMappings seedData = (seeds, mappings)
  where
    seedLine : rest = splitOn "\n\n" seedData
    seeds = map read . splitOn " " . (!! 1) $ splitOn ": " seedLine :: [Int]
    toMapping :: [Int] -> Mapping
    toMapping [destRangeStart, sourceRangeStart, rangeLength] =
      ((sourceRangeStart, sourceRangeStart + rangeLength - 1), destRangeStart - sourceRangeStart)
    mappings = map (map (toMapping . map read . splitOn " ") . tail . lines) rest :: [[Mapping]]

applyMappings :: Int -> [Mapping] -> Int
applyMappings num mappings =
  case filter mappingContainsNum mappings of
    [((sourceStart, sourceEnd), amountToUpdate)] -> num + amountToUpdate
    [] -> num
    m -> error $ "Expected to find either 0 or 1 mappings, instead found " ++ show m
  where
    mappingContainsNum ((sourceStart, sourceEnd), _) = num >= sourceStart && num <= sourceEnd

applyAllMappings :: [Int] -> [[Mapping]] -> [Int]
applyAllMappings = foldl (\nums mappings -> map (`applyMappings` mappings) nums)

part1 :: String -> Int
part1 = minimum . uncurry applyAllMappings . parseSeedsAndMappings

day05Main :: IO ()
day05Main = do
  testData <- readFile "data/day_5_test.txt"
  realData <- readFile "data/day_5.txt"
  print $ part1 testData
  print $ part1 realData
