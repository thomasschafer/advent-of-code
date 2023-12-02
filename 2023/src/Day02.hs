module Day02 (day02Main) where

import Data.Char (isSpace)
import Utils (splitBy)

data CubeCounts = CubeCounts {red :: Int, green :: Int, blue :: Int}

addCubes :: CubeCounts -> String -> CubeCounts
addCubes acc cubeString =
  case colour of
    "red" -> acc {red = red acc + read num}
    "green" -> acc {green = green acc + read num}
    "blue" -> acc {blue = blue acc + read num}
    _ -> error ("Unexpected colour " ++ colour)
  where
    [num, colour] = splitBy ' ' cubeString

parseGameRound :: String -> (Int, [CubeCounts])
parseGameRound line = (gameId, gameCubeCounts)
  where
    [start, end] = splitBy ':' line
    gameId = read . last $ splitBy ' ' start :: Int
    toCubeCounts = foldl addCubes (CubeCounts {red = 0, green = 0, blue = 0})
    gameCubeCounts = map (toCubeCounts . map (dropWhile isSpace) . splitBy ',') $ splitBy ';' end

data GameResult = GameResult {gameId :: Int, isPossible :: Bool}

parseGameResult :: CubeCounts -> String -> GameResult
parseGameResult maxCubes line = GameResult {gameId, isPossible = all isGamePossible gameCubeCounts}
  where
    (gameId, gameCubeCounts) = parseGameRound line
    isGamePossible counts = all (\f -> f counts <= f maxCubes) [red, blue, green]

part1 :: String -> Int
part1 = sum . map gameId . filter isPossible . map (parseGameResult maxCubes) . lines
  where
    maxCubes = CubeCounts {red = 12, green = 13, blue = 14}

parseCubePower :: String -> Int
parseCubePower line = numRed * numGreen * numBlue
  where
    gameCubeCounts = snd $ parseGameRound line
    updateMaxCubes acc next =
      CubeCounts {red = calcMax red, green = calcMax green, blue = calcMax blue}
      where
        calcMax colour = maximum (map colour [acc, next])
    CubeCounts {red = numRed, green = numGreen, blue = numBlue} =
      foldl updateMaxCubes (CubeCounts {red = 0, green = 0, blue = 0}) gameCubeCounts

part2 :: String -> Int
part2 = sum . map parseCubePower . lines

day02Main :: IO ()
day02Main = do
  testData <- readFile "data/day_2_test.txt"
  realData <- readFile "data/day_2.txt"
  print $ part1 testData
  print $ part1 realData
  print $ part2 testData
  print $ part2 realData
