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

manhattanDist :: (Num a) => (a, a) -> a
manhattanDist = uncurry (+) . join (***) abs

solve :: (RealFrac a, Integral c) => (((a, a), b) -> Instruction -> ((a, a), b)) -> ((a, a), b) -> String -> c
solve update initial = round . manhattanDist . fst . foldl update initial . map parseInstruction . lines

updatePos1 :: ((Float, Float), Float) -> Instruction -> ((Float, Float), Float)
updatePos1 ((north, east), angle) instr = case instr of
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

part1 :: String -> Int
part1 = solve updatePos1 ((0, 0), 90)

updateWaypoint :: Int -> (Float, Float) -> (Float, Float)
updateWaypoint angleClockwise (north, east) =
  ( north * cos radians - east * sin radians,
    east * cos radians + north * sin radians
  )
  where
    radians = fromIntegral angleClockwise * pi / 180

updatePos2 :: ((Float, Float), (Float, Float)) -> Instruction -> ((Float, Float), (Float, Float))
updatePos2 (ship@(sn, se), waypoint@(wn, we)) instr = case instr of
  South dist -> (ship, (wn - fromIntegral dist, we))
  North dist -> (ship, (wn + fromIntegral dist, we))
  East dist -> (ship, (wn, we + fromIntegral dist))
  West dist -> (ship, (wn, we - fromIntegral dist))
  Left angle -> (ship, updateWaypoint (-angle) waypoint)
  Right angle -> (ship, updateWaypoint angle waypoint)
  Forward dist -> ((sn + fromIntegral dist * wn, se + fromIntegral dist * we), waypoint)

part2 :: String -> Int
part2 = solve updatePos2 ((0, 0), (1, 10))
