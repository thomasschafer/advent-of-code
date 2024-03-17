module Day15 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)

buildInitialState :: [(Int, Int)] -> (HashMap Int Int, (Int, Int))
buildInitialState [] = error "Expected non-empty input list"
buildInitialState (turn : turns) =
  foldl
    (\(acc, (prevTurn, prevNum)) (turnNum, num) -> (HM.insert prevNum prevTurn acc, (turnNum, num)))
    (HM.empty, turn)
    turns

getFromTurn :: Int -> (HashMap Int Int, (Int, Int)) -> Int
getFromTurn turnToReach = getFromTurn'
  where
    getFromTurn' (mapping, (lastTurn, lastNum))
      | lastTurn == turnToReach = lastNum
      | otherwise = getFromTurn' (HM.insert lastNum lastTurn mapping, (lastTurn + 1, turnsAgo))
      where
        turnsAgo = case HM.lookup lastNum mapping of
          Just turn -> lastTurn - turn
          Nothing -> 0

solve :: Int -> String -> Int
solve n = getFromTurn n . buildInitialState . zip [1 ..] . parse
  where
    parse = map read . splitOn ","

part1 :: String -> Int
part1 = solve 2020

part2 :: String -> Int
part2 = solve 30000000
