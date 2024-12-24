module Day24 (part1, part2) where

import Control.Arrow (Arrow (first, second))
import Data.Bifunctor (bimap)
import Data.Bits (Bits (xor))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Utils (mapTuple, to3Tuple, toTuple)

data Op = And | Or | Xor
  deriving (Show)

perform :: Op -> Bool -> Bool -> Bool
perform = \case
  And -> (&&)
  Or -> (||)
  Xor -> xor

type Operation = (String, Op, String)

parse :: String -> (HashMap String Bool, [(Operation, String)])
parse = bimap (HM.fromList . map parseMapping) (map parseOperation) . toTuple . map lines . splitOn "\n\n"
  where
    parseMapping = second (== "1") . toTuple . splitOn ": "
    parseOperation = first (toOp . to3Tuple . words) . toTuple . splitOn " -> "

    toOp (s1, opStr, s2) = (s1, op, s2)
      where
        op = case opStr of
          "AND" -> And
          "OR" -> Or
          "XOR" -> Xor

simulate :: HashMap String Bool -> [(Operation, String)] -> HashMap String Bool
simulate curState ops = foldl (flip search) curState zOps
  where
    revOpsMap = HM.fromList $ map swap ops
    zOps = filter ((== 'z') . head) $ map snd ops

    search :: String -> HashMap String Bool -> HashMap String Bool
    search wire state = case HM.lookup wire state of
      Just _ -> state
      Nothing -> HM.insert wire result newState
        where
          (a, op, b) = fromJust $ HM.lookup wire revOpsMap
          newState = search b $ search a state
          (resA, resB) = mapTuple (fromJust . flip HM.lookup newState) (a, b)
          result = perform op resA resB

zNumber :: HashMap String Bool -> Int
zNumber = fromBinary . map (first $ read . tail) . filter ((== 'z') . head . fst) . HM.toList
  where
    fromBinary = sum . map (\(digitPos, isOn) -> if isOn then 2 ^ digitPos else 0)

part1 :: String -> Int
part1 = zNumber . uncurry simulate . parse

-- TODO
part2 :: String -> Int
part2 = const 2
