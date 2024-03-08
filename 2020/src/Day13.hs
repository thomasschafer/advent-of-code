module Day13 (part1, part2) where

import Control.Arrow (Arrow ((&&&)))
import Data.List.Split (splitOn)

earliestBusToAirport :: Int -> [Int] -> Int
earliestBusToAirport departureTime = uncurry (*) . minimum . map (departsIn &&& id)
  where
    departsIn busId = busId - departureTime `mod` busId

part1 :: String -> Int
part1 =
  uncurry earliestBusToAirport
    . (\[[a], b] -> (a, b))
    . map (map read . filter (/= "x") . splitOn ",")
    . lines

part2 :: String -> Int
part2 = const 1
