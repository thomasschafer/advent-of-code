module Day23 (part1, part2) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MV
import Utils (toInt)

-- move :: [Int] -> [Int]
-- move (current : cups) = tail newOrder ++ [head newOrder]
--  where
--   threeCups = take 3 cups
--   rest = drop 3 cups
--   destination = case filter (< current) rest of
--     [] -> maximum rest
--     cs -> maximum cs
--   insertionIdx = fromJust (elemIndex destination rest) + 1
--   newOrder = current : take insertionIdx rest ++ threeCups ++ drop insertionIdx rest
-- move cups = error $ "Not enough cups: " ++ show cups
--
-- orderAfter1 :: [Int] -> [Int]
-- orderAfter1 cups = drop (idx + 1) cups ++ take idx cups
--  where
--   idx = fromJust $ elemIndex 1 cups
--
-- solve :: Int -> Int -> String -> [Int]
-- solve repetitions expandedLength cupStr = orderAfter1 . (!! repetitions) $ iterate move cupsExpanded
--  where
--   cups = map toInt cupStr
--   cupsExpanded = cups ++ [(maximum cups + 1) .. expandedLength]
--
-- part1 :: String -> String
-- part1 cups = concatMap show $ solve 100 (length cups) cups
--
-- part2 :: String -> Int
-- part2 = uncurry (*) . toTuple . take 2 . join solve (10 ^ 6)

-- The head of the returned vector will be the current element.
-- For all other elements, value n being an index m means that n is immediately clockwise of m
toMVec :: String -> ST s (MVector s Int)
toMVec cupStr = do
  let cups = map toInt cupStr
  let pairs = zip (last cups : init cups) cups
  v <- V.thaw $ V.fromList [0 .. length cups]
  forM_ pairs $ uncurry (MV.write v)
  MV.write v 0 (head cups)
  return v

move :: MVector s Int -> ST s (MVector s Int)
move mv = do
  current <- MV.read mv 0
  c1 <- MV.read mv current
  c2 <- MV.read mv c1
  c3 <- MV.read mv c2
  let selection = [c1, c2, c3]
  let destination =
        case maximum [current' | i <- [1 .. 4], let current' = current - i, current' `notElem` selection] of
          0 -> MV.length mv - 1
          c -> c
  c3Next <- MV.read mv c3
  MV.write mv current c3Next -- current -> c3 next
  destNext <- MV.read mv destination
  MV.write mv c3 destNext -- c3 -> destination next
  MV.write mv destination c1 -- destination -> c1
  MV.write mv 0 c3Next -- head -> current next
  return mv

applyMoveNTimes :: Int -> MV.MVector s Int -> ST s (MV.MVector s Int)
applyMoveNTimes 0 mvec = return mvec
applyMoveNTimes n mvec = move mvec >>= applyMoveNTimes (n - 1)

orderAfter1 :: MVector s Int -> ST s [Int]
orderAfter1 mv = return []

part1 :: String -> [Int]
part1 cupStr = runST $ do
  cups <- toMVec cupStr
  cups' <- applyMoveNTimes 2 cups
  -- orderAfter1 . V.toList <$> V.freeze cups'
  V.toList <$> V.freeze cups'

part2 :: String -> Int
part2 = const 1 -- TODO
