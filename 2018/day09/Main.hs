module Main where

import Control.Arrow (second, (***))
import Control.Monad (join)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

parse :: String -> (Int, Int)
parse s = case words s of
  [numPlayers, "players;", "last", "marble", "is", "worth", lastMarblePoints, "points"] ->
    join (***) read (numPlayers, lastMarblePoints)

shiftCurPtrAntiCw :: Seq Int -> Seq Int
shiftCurPtrAntiCw Seq.Empty = Seq.Empty
shiftCurPtrAntiCw (rest :|> x) = x :<| rest

solve :: Int -> Int -> Int
solve numPlayers lastMarble = go 1 HM.empty (Seq.singleton 0)
  where
    -- left of Seq is cur and then immediately clockwise of cur, right is anticlockwise
    go :: Int -> HashMap Int Int -> Seq Int -> Int
    go turn scores marbles
      | turn > lastMarble = maximum $ HM.elems scores
      | turn `mod` 23 == 0 =
          let scores' = HM.insertWith (+) player (turn + sevenBack) scores
              (marbles' :|> sevenBack) = iterate shiftCurPtrAntiCw marbles !! 6
           in go (turn + 1) scores' marbles'
      | otherwise =
          let marbles' = case marbles of
                (cur :<| r :<| rs) -> (turn :<| rs) :|> cur :|> r
                _ -> turn :<| marbles
           in go (turn + 1) scores marbles'
      where
        player = turn `mod` numPlayers

part1 :: String -> Int
part1 = uncurry solve . parse

part2 :: String -> Int
part2 = uncurry solve . second (*100) . parse
