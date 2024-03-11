module Day13 (part1, part2) where

import Control.Arrow (Arrow (second, (&&&)))
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

egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b

modInv :: Int -> Int -> Int
modInv a b = case egcd a b of
  (x, y)
    | a * x + b * y == 1 -> x
    | otherwise -> error $ "No modular inverse for " ++ show a ++ " and " ++ show b

-- Chinese remainder theorem solver from https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell
crtSolve :: [(Int, Int)] -> Int
crtSolve schedules =
  (`mod` allProd)
    . sum
    . zipWith (*) crtModulii
    . zipWith (*) residues
    $ zipWith modInv crtModulii modulii
  where
    residues = map (negate . fst) schedules
    modulii = map snd schedules
    allProd = product modulii
    crtModulii = map (allProd `div`) modulii

part2 :: String -> Int
part2 = crtSolve . parseTimes . last . lines
  where
    parseTimes =
      map (second read)
        . filter ((/= "x") . snd)
        . zip [0 ..]
        . splitOn ","
