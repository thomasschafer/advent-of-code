module Day05 (part1, part2) where

import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Utils (withIdx)

data Mapping = Mapping {sourceStart :: Int, sourceEnd :: Int, amountToUpdate :: Int}

parseSeedsAndMappings :: String -> ([Int], [[Mapping]])
parseSeedsAndMappings seedData = (seeds, mappings)
  where
    seedLine : rest = splitOn "\n\n" seedData
    seeds = map read . splitOn " " . (!! 1) $ splitOn ": " seedLine
    toMapping [destRangeStart, sourceStart, rangeLength] =
      Mapping
        { sourceStart,
          sourceEnd = sourceStart + rangeLength - 1,
          amountToUpdate = destRangeStart - sourceStart
        }
    mappings = map (map (toMapping . map read . splitOn " ") . tail . lines) rest

filterAndSortMappings :: (Int, Int) -> [Mapping] -> [Mapping]
filterAndSortMappings (num, len) mappings = sortBy (compare `on` sourceStart) filteredMappings
  where
    mappingIntersects Mapping {sourceStart, sourceEnd} = num <= sourceEnd && sourceStart <= num + len - 1
    filteredMappings = filter mappingIntersects mappings

applyMappingsToRange :: [Mapping] -> (Int, Int) -> [(Int, Int)]
applyMappingsToRange mappings (rangeStart, len) = updatedRanges
  where
    orderedMappings = filterAndSortMappings (rangeStart, len) mappings
    maybeMappingsStart = if null orderedMappings then Nothing else Just ((sourceStart . head) orderedMappings)
    rangeEnd = rangeStart + len - 1
    maybeMappingsEnd = if null orderedMappings then Nothing else Just ((sourceEnd . last) orderedMappings)

    addMappings idx = cur : ([mappingToAdd | sourceEnd cur < sourceStart next - 1])
      where
        cur = orderedMappings !! idx
        next = orderedMappings !! (idx + 1)
        mappingToAdd = Mapping {sourceStart = sourceEnd cur + 1, sourceEnd = sourceStart next - 1, amountToUpdate = 0}

    trim Mapping {sourceStart, sourceEnd, amountToUpdate} =
      Mapping
        { sourceStart = max sourceStart rangeStart,
          sourceEnd = min sourceEnd rangeEnd,
          amountToUpdate
        }

    allMappings = map trim $
      case (maybeMappingsStart, maybeMappingsEnd) of
        (Just mappingsStart, Just mappingsEnd) ->
          [Mapping {sourceStart = rangeStart, sourceEnd = mappingsStart - 1, amountToUpdate = 0} | mappingsStart > rangeStart]
            ++ concatMap addMappings [0 .. length orderedMappings - 2]
            ++ [last orderedMappings]
            ++ [Mapping {sourceStart = mappingsEnd + 1, sourceEnd = rangeEnd, amountToUpdate = 0} | rangeEnd > mappingsEnd]
        _ -> [Mapping {sourceStart = rangeStart, sourceEnd = rangeEnd, amountToUpdate = 0}]

    updatedRanges = map (\Mapping {sourceStart, sourceEnd, amountToUpdate} -> (sourceStart + amountToUpdate, sourceEnd - sourceStart + 1)) allMappings

solve :: ([Int] -> [(Int, Int)]) -> String -> Int
solve seedsToRanges seedData = fst . minimum $ foldl update seedRanges allMappings
  where
    (seeds, allMappings) = parseSeedsAndMappings seedData
    seedRanges = seedsToRanges seeds
    update ranges m = concatMap (applyMappingsToRange m) ranges

part1 :: String -> Int
part1 = solve (map (,1))

part2 :: String -> Int
part2 = solve (foldl toSeedRanges [] . withIdx)
  where
    toSeedRanges acc (idx, num) = if odd idx then init acc ++ [(fst (last acc), num)] else acc ++ [(num, -1)]
