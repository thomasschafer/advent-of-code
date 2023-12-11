module Day17 (day17Main) where

import Data.List.Split (splitOn)

hitsTarget :: ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int) -> Bool
hitsTarget ((x1, x2), (y1, y2)) (vx, vy) (x, y) =
  (x <= x2 && y >= y1)
    && ( withinTarget x y
           || hitsTarget
             ((x1, x2), (y1, y2))
             (if vx > 0 then vx - 1 else if vx < 0 then vx + 1 else 0, vy - 1)
             (x + vx, y + vy)
       )
  where
    withinTarget x y = x1 <= x && x <= x2 && y1 <= y && y <= y2

velocitiesThatHitTarget :: String -> [(Int, Int)]
velocitiesThatHitTarget s =
  filter
    (\(vx, vy) -> hitsTarget ((x1, x2), (y1, y2)) (vx, vy) (0, 0))
    velocitiesToTest
  where
    [[x1, x2], [y1, y2]] = parseCoords s
    velocitiesToTest =
      [ (vx, vy)
        | vy <- reverse [-maxVy .. maxVy],
          vx <- (if x2 < 0 then reverse [x2 .. 0] else [0 .. x2])
      ]
    maxVy = abs x2 + abs y2

parseCoords :: String -> [[Int]]
parseCoords = map (map (read . filter (/= ',')) . splitOn ".." . last . splitOn "=") . drop 2 . words

solvePart1 :: String -> Int
solvePart1 = maxHeightReached . snd . head . velocitiesThatHitTarget
  where
    maxHeightReached vy = vy * (vy + 1) `div` 2

solvePart2 :: String -> Int
solvePart2 = length . velocitiesThatHitTarget

day17Main :: IO ()
day17Main = do
  testData <- readFile "data/day_17_test.txt"
  realData <- readFile "data/day_17.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
  print $ solvePart2 testData
  print $ solvePart2 realData
