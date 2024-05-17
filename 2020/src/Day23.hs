module Day23 (part1, part2) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MV
import Utils (toInt)

-- The head of the returned vector will be the current element.
-- For all other elements, value n being an index m means that n is immediately clockwise of m
toMVec :: [Int] -> ST s (MVector s Int)
toMVec cups = do
  let pairs = zip (last cups : init cups) cups
  v <- V.thaw $ V.fromList [0 .. length cups]
  forM_ pairs $ uncurry (MV.write v)
  MV.write v 0 (head cups)
  return v

fromLinked :: [Int] -> [Int]
fromLinked (current : cups) = take (length cups) $ display' current
 where
  display' idx = idx : display' (cups !! (idx - 1))

move :: Int -> MVector s Int -> ST s (MVector s Int)
move mvLen mv = do
  current <- MV.read mv 0
  c1 <- MV.read mv current
  c2 <- MV.read mv c1
  c3 <- MV.read mv c2
  let selection = [c1, c2, c3]
  let destination =
        case maximum [current' | i <- [1 .. 4], let current' = current - i, current' `notElem` selection] of
          0 -> maximum $ filter (`notElem` selection) [mvLen - 3 .. mvLen - 1]
          c -> c
  c3Next <- MV.read mv c3
  MV.write mv current c3Next -- current -> c3 next
  destNext <- MV.read mv destination
  MV.write mv c3 destNext -- c3 -> destination next
  MV.write mv destination c1 -- destination -> c1
  MV.write mv 0 c3Next -- head -> current next
  return mv

applyMoveNTimes :: Int -> MV.MVector s Int -> ST s (MV.MVector s Int)
applyMoveNTimes k mv = applyMoveNTimes' k mv
 where
  mvLen = MV.length mv
  applyMoveNTimes' 0 mvec = return mvec
  applyMoveNTimes' n mvec = move mvLen mvec >>= applyMoveNTimes (n - 1)

orderAfter1 :: [Int] -> [Int]
orderAfter1 nums = drop (idx1 + 1) nums ++ take (idx1 - 1) nums
 where
  idx1 = fromJust $ elemIndex 1 nums

solve :: ([Int] -> [Int]) -> Int -> String -> [Int]
solve transform numMoves cupStr = runST $ do
  cups <- toMVec . transform $ map toInt cupStr
  cupsMoved <- applyMoveNTimes numMoves cups
  orderAfter1 . fromLinked . V.toList <$> V.freeze cupsMoved

part1 :: String -> String
part1 = concatMap show . solve id 100

part2 :: String -> Int
part2 = productOfFirst2 . solve extend 10000000
 where
  productOfFirst2 = (\(a : b : _) -> a * b)
  extend nums = nums ++ [length nums + 1 .. 1000000]
