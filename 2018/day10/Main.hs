module Main where

import Control.Arrow ((&&&), (***))
import Control.Monad (join)
import Data.Char (isDigit)
import Data.List (intercalate, maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Set qualified as S
import Debug.Trace (traceM, traceShowId)

main :: IO ()
main = do
  input <- getContents
  traceM . display $ part1 input
  print "ZAEZRLZG"

display :: [Point] -> String
display = intercalate "\n" . toGrid
  where
    toGrid :: [Point] -> [[Char]]
    toGrid ps = [[if (x, y) `S.member` s then '#' else ' ' | x <- [lx .. gx]] | y <- [ly .. gy]]
      where
        ((lx, gx), (ly, gy)) = boxBounds ps
        s = S.fromList $ map (\(Point p _) -> p) ps

boxBounds :: [Point] -> ((Int, Int), (Int, Int))
boxBounds = join (***) ((minimum &&& maximum)) . unzip . map (x &&& y)
  where
    x (Point (px, _) _) = px
    y (Point (_, py) _) = py

boxSize :: [Point] -> Int
boxSize = uncurry (*) . join (***) (uncurry $ flip (-)) . boxBounds

-- When you start part 2, uncomment the next line.
-- print (part2 input)

data Point = Point (Int, Int) (Int, Int)

step :: Point -> Point
step (Point (px, py) (vx, vy)) = Point (px + vx, py + vy) (vx, vy)

parse :: String -> Point
parse = toPoint . map read . words . map keepDigits
  where
    keepDigits c
      | isDigit c || c == '-' = c
      | otherwise = ' '

    toPoint [px, py, vx, vy] = Point (px, py) (vx, vy)

part1 :: String -> [Point]
part1 = snd . localMinBy fst . map (boxSize &&& id) . iterate (map step) . map parse . lines
  where
    localMinBy f (x1 : x2 : x3 : rest)
      | f x1 > f x2 && f x2 < f x3 = x2
      | otherwise = localMinBy f (x2 : x3 : rest)

part2 :: String -> Int
part2 = undefined
