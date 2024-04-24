module Day22 (part1, part2) where

import Data.List.Split (splitOn)
import Data.Set qualified as S
import Utils (toTuple)

type GameState = ([Int], [Int])

parse :: String -> GameState
parse = toTuple . map (map read . tail . lines) . splitOn "\n\n"

score :: [Int] -> Int
score = sum . zipWith (*) [1 ..] . reverse

playNonRec :: GameState -> [Int]
playNonRec ([], cards) = cards
playNonRec (cards, []) = cards
playNonRec (c1 : c1s, c2 : c2s)
  | c1 > c2 = playNonRec (c1s ++ [c1, c2], c2s)
  | c2 > c1 = playNonRec (c1s, c2s ++ [c2, c1])
  | otherwise = error $ "Found cards of equal value: " ++ show c1

playRec :: GameState -> GameState
playRec = playRec' S.empty
 where
  playRec' _ game@([], _) = game
  playRec' _ game@(_, []) = game
  playRec' seen game@(p1@(c1 : c1s), c2 : c2s)
    | game `S.member` seen = (p1, [])
    | (c1 <= length c1s) && (c2 <= length c2s) =
        case playRec' S.empty (take c1 c1s, take c2 c2s) of
          (_, []) -> playRec' updatedSeen (c1s ++ [c1, c2], c2s)
          ([], _) -> playRec' updatedSeen (c1s, c2s ++ [c2, c1])
          _ -> error "Found two empty stacks in game state"
    | c1 > c2 = playRec' updatedSeen (c1s ++ [c1, c2], c2s)
    | c2 > c1 = playRec' updatedSeen (c1s, c2s ++ [c2, c1])
    | otherwise = error $ "Found cards of equal value: " ++ show c1
   where
    updatedSeen = S.insert game seen

part1 :: String -> Int
part1 = score . playNonRec . parse

part2 :: String -> Int
part2 = score . winner . playRec . parse
 where
  winner ([], cards) = cards
  winner (cards, []) = cards
  winner _ = error "Found two empty stacks"
