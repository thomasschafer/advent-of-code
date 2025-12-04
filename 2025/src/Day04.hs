module Day04 (part1, part2) where

import Control.Monad (join)
import Data.Set (Set)
import Data.Set qualified as S

parse :: String -> Set (Int, Int)
parse = S.fromList . toIndices . lines
  where
    toIndices rolls =
      [ (r, c)
      | r <- [0 .. length rolls - 1],
        c <- [0 .. length (head rolls) - 1],
        rolls !! r !! c == '@'
      ]

accessibleRolls :: Set (Int, Int) -> Set (Int, Int)
accessibleRolls = join $ S.filter . isAccessible

isAccessible :: Set (Int, Int) -> (Int, Int) -> Bool
isAccessible rolls (r, c) = length neighbouringRolls < 4
  where
    neighbouringRolls =
      [ ()
      | i <- [-1 .. 1],
        j <- [-1 .. 1],
        let pos = (r + i, c + j),
        pos /= (r, c) && pos `S.member` rolls
      ]

part1 :: String -> Int
part1 = length . accessibleRolls . parse

repeatRemoveRolls :: Int -> Set (Int, Int) -> Int
repeatRemoveRolls acc rolls
  | S.null toRemove = acc
  | otherwise = repeatRemoveRolls (acc + S.size toRemove) $ S.filter (`S.notMember` toRemove) rolls
  where
    toRemove = accessibleRolls rolls

part2 :: String -> Int
part2 = repeatRemoveRolls 0 . parse
