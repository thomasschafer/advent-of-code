module Day5 (day5Main) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Utils (splitBy)

type Coords = (Int, Int)

parseCoords :: String -> ((Coords, Coords) -> Bool) -> [(Coords, Coords)]
parseCoords ventData shouldInclude = mapMaybe lineToCoords $ lines ventData
  where
    wordToCoords :: String -> Coords
    wordToCoords word = (x, y)
      where
        numStrings = splitBy ',' word
        x = read $ head numStrings
        y = read $ last numStrings

    lineToCoords :: String -> Maybe (Coords, Coords)
    lineToCoords line = if shouldInclude coords then Just coords else Nothing
      where
        allWords = words line
        coords = (wordToCoords $ head allWords, wordToCoords $ last allWords)

solvePart1 :: String -> Int
solvePart1 ventData = foldl (\acc v -> if v >= 2 then acc + 1 else acc) 0 ventCounts
  where
    coords = parseCoords ventData (\(c1, c2) -> fst c1 == fst c2 || snd c1 == snd c2)

    updateVents :: (Coords, Coords) -> HashMap Coords Int -> HashMap Coords Int
    updateVents ((x1, y1), (x2, y2)) numVents =
      if x1 == x2
        then
          let start = min y1 y2
              end = max y1 y2
              updateFn acc y = HashMap.insert key nextVal acc
                where
                  key = (x1, y)
                  nextVal = fromMaybe 0 (HashMap.lookup key acc) + 1
           in foldl updateFn numVents [start .. end]
        else
          let start = min x1 x2
              end = max x1 x2
              updateFn acc x = HashMap.insert key nextVal acc
                where
                  key = (x, y1)
                  nextVal = fromMaybe 0 (HashMap.lookup key acc) + 1
           in foldl updateFn numVents [start .. end]

    calcVentCounts :: [(Coords, Coords)] -> HashMap Coords Int -> HashMap Coords Int
    calcVentCounts [] numVents = numVents
    calcVentCounts (coordPair : coordPairs) numVents =
      let newNumVents = updateVents coordPair numVents
       in calcVentCounts coordPairs newNumVents

    ventCounts = calcVentCounts coords HashMap.empty

solvePart2 :: String -> Int
solvePart2 ventData = foldl (\acc v -> if v >= 2 then acc + 1 else acc) 0 ventCounts
  where
    coords = parseCoords ventData (\(c1, c2) -> fst c1 == fst c2 || snd c1 == snd c2)

    updateVents :: (Coords, Coords) -> HashMap Coords Int -> HashMap Coords Int
    updateVents ((x1, y1), (x2, y2)) numVents =
      if x1 == x2
        then
          let start = min y1 y2
              end = max y1 y2
              updateFn acc y = HashMap.insert key nextVal acc
                where
                  key = (x1, y)
                  nextVal = fromMaybe 0 (HashMap.lookup key acc) + 1
           in foldl updateFn numVents [start .. end]
        else
          let start = min x1 x2
              end = max x1 x2
              updateFn acc x = HashMap.insert key nextVal acc
                where
                  key = (x, y1)
                  nextVal = fromMaybe 0 (HashMap.lookup key acc) + 1
           in foldl updateFn numVents [start .. end]

    calcVentCounts :: [(Coords, Coords)] -> HashMap Coords Int -> HashMap Coords Int
    calcVentCounts [] numVents = numVents
    calcVentCounts (coordPair : coordPairs) numVents =
      let newNumVents = updateVents coordPair numVents
       in calcVentCounts coordPairs newNumVents

    ventCounts = calcVentCounts coords HashMap.empty

day5Main :: IO ()
day5Main = do
  testData <- readFile "data/day_5_test.txt"
  print $ solvePart1 testData
  realData <- readFile "data/day_5.txt"
  print $ solvePart1 realData
