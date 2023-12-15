module Day15 (part1, part2) where

import Data.Char (ord)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (findIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Utils (withIdx)

hash :: String -> Int
hash = foldl (\acc c -> ((acc + ord c) * 17) `mod` 256) 0

part1 :: String -> Int
part1 = sum . map hash . splitOn ","

data LensOperation = Add String Int | Remove String

parse :: String -> LensOperation
parse s = if last s == '-' then Remove (init s) else Add label (read focalLength)
  where
    [label, focalLength] = splitOn "=" s

data Lens = Lens {label :: String, focalLength :: Int}

applyOperation :: HashMap Int [Lens] -> LensOperation -> HashMap Int [Lens]
applyOperation lensesMap op = HM.insert boxNum (updateBox curBox op) lensesMap
  where
    boxNum = hash (case op of Add l _ -> l; Remove l -> l)
    curBox = fromMaybe [] (HM.lookup boxNum lensesMap)

updateBox :: [Lens] -> LensOperation -> [Lens]
updateBox lenses (Remove lensLabel) = filter ((/= lensLabel) . label) lenses
updateBox lenses (Add lensLabel lensFocalLength) = case findIndex ((== lensLabel) . label) lenses of
  Just idx -> take idx lenses ++ [newLens] ++ drop (idx + 1) lenses
  Nothing -> lenses ++ [newLens]
  where
    newLens = Lens {label = lensLabel, focalLength = lensFocalLength}

part2 :: String -> Int
part2 = sum . concatMap focusingPower . HM.toList . foldl applyOperation HM.empty . map parse . splitOn ","
  where
    focusingPower (boxNum, lenses) = map (\(idx, l) -> (boxNum + 1) * (idx + 1) * focalLength l) (withIdx lenses)
