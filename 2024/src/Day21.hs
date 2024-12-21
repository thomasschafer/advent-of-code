module Day21 (part1, part2) where

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Utils (positionOf)

keypad, dirpad :: [[Char]]
keypad = ["789", "456", "123", " 0A"]
dirpad = [" ^A", "<v>"]

data PadType = Keypad | Dirpad
  deriving (Eq)

toKeyPressOptions :: PadType -> (Int, Int) -> (Int, Int) -> [[Char]]
toKeyPressOptions padType (r1, c1) (r2, c2) = nub $ catMaybes [vh, hv]
  where
    (dr, dc) = (r2 - r1, c2 - c1)
    vertical = replicate (abs dr) (if dr > 0 then 'v' else '^')
    horizontal = replicate (abs dc) (if dc > 0 then '>' else '<')
    vh = do
      guard . not $ case padType of
        Keypad -> c1 == 0 && r2 == 3
        Dirpad -> c1 == 0 && r2 == 0
      return . concat $ [vertical, horizontal, "A"]
    hv = do
      guard . not $ case padType of
        Keypad -> r1 == 3 && c2 == 0
        Dirpad -> r1 == 0 && c2 == 0
      return . concat $ [horizontal, vertical, "A"]

choices :: [[[Char]]] -> [[Char]]
choices [] = [[]]
choices (initial : rest) = concatMap (\i -> map (i ++) (choices rest)) initial

moves :: PadType -> [Char] -> [[Char]]
moves padType chars = choices options
  where
    pad = case padType of Keypad -> keypad; Dirpad -> dirpad
    positions = map (flip positionOf pad . (==)) ('A' : chars)
    options = zipWith (toKeyPressOptions padType) positions $ tail positions

complexity :: [Char] -> Int
complexity chars = minimum (map length mvs) * read (takeWhile isDigit chars)
  where
    mvs = moves Keypad chars >>= moves Dirpad >>= moves Dirpad

part1 :: String -> Int
part1 = sum . map complexity . lines

-- TODO
part2 :: String -> Int
part2 = const 2
