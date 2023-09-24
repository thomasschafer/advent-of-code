module Day5 (day5Main) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
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

numVentCounts :: ((Coords, Coords) -> Bool) -> String -> Int
numVentCounts shouldInclude ventData = foldl (\acc v -> if v >= 2 then acc + 1 else acc) 0 ventCounts
  where
    updateVents :: HashMap Coords Int -> (Coords, Coords) -> HashMap Coords Int
    updateVents numVents ((x1, y1), (x2, y2)) = newVents
      where
        direction = (signum (x2 - x1), signum (y2 - y1))
        numSteps = max (abs (x2 - x1)) (abs (y2 - y1))

        updateFn :: (HashMap Coords Int, Coords) -> Int -> (HashMap Coords Int, Coords)
        updateFn (vents, curKey) _ = (HashMap.insert curKey updatedVal vents, nextKey)
          where
            (xCur, yCur) = curKey
            updatedVal = fromMaybe 0 (HashMap.lookup curKey vents) + 1
            xNext = xCur + fst direction
            yNext = yCur + snd direction
            nextKey = (xNext, yNext)

        (newVents, _) = foldl updateFn (numVents, (x1, y1)) [0 .. numSteps]

    coords = parseCoords ventData shouldInclude
    ventCounts = foldl updateVents HashMap.empty coords

solvePart1 :: String -> Int
solvePart1 = numVentCounts (\(c1, c2) -> fst c1 == fst c2 || snd c1 == snd c2)

solvePart2 :: String -> Int
solvePart2 = numVentCounts (const True)

day5Main :: IO ()
day5Main = do
  testData <- readFile "data/day_5_test.txt"
  realData <- readFile "data/day_5.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
  print $ solvePart2 testData
  print $ solvePart2 realData
