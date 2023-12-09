module Day17 (day17Main) where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Utils (quickTrace)

parseCoords :: String -> [[Int]]
parseCoords = map (map (read . filter (/= ',')) . splitOn ".." . last . splitOn "=") . drop 2 . words

solvePart1 :: String -> Int
solvePart1 s = maxHeightReached . fromJust $ find hitsTarget (reverse [-maxVy .. maxVy]) -- todo
  where
    [[x1, x2], [y1, y2]] = parseCoords s
    maxVy = abs x2 + abs y2

    maxHeightReached vy = vy * (vy + 1) `div` 2

    hitsTarget vy = any (\vx -> hitsTarget' vx vy 0 0) (if x2 < 0 then reverse [x2 .. 0] else [0 .. x2])
      where
        withinTarget x y = x1 <= x && x <= x2 && y1 <= y && y <= y2
        hitsTarget' vx vy x y =
          (x <= x2 && y >= y1)
            && ( withinTarget x y
                   || hitsTarget'
                     (if vx > 0 then vx - 1 else if vx < 0 then vx + 1 else 0)
                     (vy - 1)
                     (x + vx)
                     (y + vy)
               )

day17Main :: IO ()
day17Main = do
  testData <- readFile "data/day_17_test.txt"
  realData <- readFile "data/day_17.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
