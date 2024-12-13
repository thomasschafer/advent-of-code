module Day13 (part1, part2) where

import Control.Monad
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Utils (mapTuple, to3Tuple, toTuple)

parse :: Int -> String -> ((Int, Int), (Int, Int), (Int, Int))
parse toAdd =
  (\(a, b, c) -> (a, b, mapTuple (+ toAdd) c))
    . to3Tuple
    . map (toTuple . map (read . drop 2) . splitOn ", " . last . splitOn ": ")
    . lines

-- Algebra:
-- (ax, ay)*af + (bx, by)*bf = (px, py)
-- ax*af + bx*bf = px
-- ay*af + by*bf = py
-- bf = (px - ax*af)/bx -- (1)
-- bf = (py - ay*af)/by
-- by(px - ax*af) = bx(py - ay*af)
-- bypx - ax*af*by = bx*py - ay*bx*af
-- ay*bx*af - ax*by*af = bx*py - by*px
-- af(ay*bx - ax*by) = bx*py - by*px
-- af = (bx*py - by*px)/(ay*bx - ax*by)
-- -- Plug this into (1) to get bf
minTokensToWin :: ((Int, Int), (Int, Int), (Int, Int)) -> Maybe Int
minTokensToWin ((ax, ay), (bx, by), (px, py)) = do
  let num = bx * py - by * px
  let den = ay * bx - ax * by
  guard $ num `mod` den == 0
  let aFactor = num `div` den
  let bFactor = (py - ay * aFactor) `div` by
  return $ 3 * aFactor + bFactor

solve :: Int -> String -> Int
solve toAdd = sum . mapMaybe (minTokensToWin . parse toAdd) . splitOn "\n\n"

part1, part2 :: String -> Int
(part1, part2) = mapTuple solve (0, 10000000000000)
