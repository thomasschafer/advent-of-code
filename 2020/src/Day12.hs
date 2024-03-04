module Day12 (part1, part2) where

import Control.Arrow ((***))
import Control.Monad (join)
import Prelude hiding (Left, Right)

data Instruction
  = North Int
  | South Int
  | East Int
  | West Int
  | Left Int
  | Right Int
  | Forward Int
  deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction (instrType : num) = instruction $ read num
  where
    instruction = case instrType of
      'N' -> North
      'S' -> South
      'E' -> East
      'W' -> West
      'L' -> Left
      'R' -> Right
      'F' -> Forward

updatePos :: ((Float, Float), Float) -> Instruction -> ((Float, Float), Float)
updatePos ((north, east), angle) instr = case instr of
  South dist -> ((north - fromIntegral dist, east), angle)
  North dist -> ((north + fromIntegral dist, east), angle)
  East dist -> ((north, east + fromIntegral dist), angle)
  West dist -> ((north, east - fromIntegral dist), angle)
  Left delta -> ((north, east), angle - fromIntegral delta)
  Right delta -> ((north, east), angle + fromIntegral delta)
  Forward dist -> ((north + dn, east + de), angle)
    where
      radians = angle * pi / 180
      dn = fromIntegral dist * cos radians
      de = fromIntegral dist * sin radians

manhattanDist :: (Num a) => (a, a) -> a
manhattanDist = uncurry (+) . join (***) abs

part1 :: String -> Int
part1 = round . manhattanDist . fst . foldl updatePos ((0, 0), 90) . map parseInstruction . lines

part2 :: String -> Int
part2 = const 2
