module Day02 (day02Main) where

import Data.Char (isSpace)
import Utils (quickTrace, splitBy)

data CubeSet = CubeSet {red :: Int, green :: Int, blue :: Int}

addCubes :: CubeSet -> String -> CubeSet
addCubes acc cubeString =
  case colour of
    "red" -> acc {red = red acc + read num}
    "green" -> acc {green = green acc + read num}
    "blue" -> acc {blue = blue acc + read num}
    _ -> error ("Unexpected colour " ++ colour)
  where
    [num, colour] = splitBy ' ' cubeString

data GameResult = GameResult {gameId :: Int, isPossible :: Bool}

part1 :: String -> Int
part1 = sum . map gameId . filter isPossible . map parseLine . lines
  where
    maxCubes = CubeSet {red = 12, green = 13, blue = 14}

    parseLine :: String -> GameResult
    parseLine line = GameResult {gameId, isPossible = all isGamePossible cubeSets}
      where
        [start, end] = splitBy ':' line
        gameId = read . last $ splitBy ' ' start :: Int
        toCubeSet = foldl addCubes (CubeSet {red = 0, green = 0, blue = 0})
        cubeSets = map (toCubeSet . map (dropWhile isSpace) . splitBy ',') $ splitBy ';' end
        isGamePossible c = red c <= red maxCubes && blue c <= blue maxCubes && green c <= green maxCubes

day02Main :: IO ()
day02Main = do
  testData <- readFile "data/day_2_test.txt"
  realData <- readFile "data/day_2.txt"
  print $ part1 testData
  print $ part1 realData
