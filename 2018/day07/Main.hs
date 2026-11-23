module Main where

import Control.Arrow (Arrow (second), first, (***))
import Control.Monad (join)
import Data.Char (ord)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (partition, sort, sortBy)
import Data.List qualified as L
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

parse :: String -> (Char, Char)
parse s = case words s of
  ["Step", [from], "must", "be", "finished", "before", "step", [to], "can", "begin."] ->
    (from, to)
  _ -> error $ "Invalid format" ++ s

depMap :: [(Char, Char)] -> HashMap Char (Set Char)
depMap = HM.fromListWith S.union . concatMap (\(a, b) -> [(a, S.empty), (b, S.singleton a)])

toTime :: Char -> Int -> Int
toTime c delay = ord c - ord 'A' + 1 + delay

stepOrderParallel :: Int -> Int -> [(Char, Char)] -> (String, Int)
stepOrderParallel numWorkers delay graphTuples = go ([], 0) (depMap graphTuples) []
  where
    --  workers must be passed in sorted and at least one must be ready i.e. 0 time left on current job
    go :: ([Char], Int) -> HashMap Char (Set Char) -> [(Int, Char)] -> (String, Int)
    go (acc, time) deps workers =
      let ready = sort . map fst . HM.toList $ HM.filter S.null deps
       in case (workers, ready) of
            -- a worker and job are availble: don't continue any existing work, just allocate that new work
            (_, nextJob : _)
              | length workers < numWorkers ->
                  go (acc, time) (HM.delete nextJob deps) (L.insert (toTime nextJob delay, nextJob) workers)
            -- advance time until first available job is complete
            ((t, _) : _, _) ->
              let (finished, unfinished) = span ((== t) . fst) workers
                  cs = S.fromList $ map snd finished
               in go (reverse (map snd finished) ++ acc, t) (HM.map (`S.difference` cs) deps) unfinished
            -- nothing left to do
            ([], _) | HM.null deps -> (reverse acc, time)

part1 :: String -> String
part1 = fst . stepOrderParallel 1 0 . map parse . lines

part2 :: String -> Int
part2 = snd . stepOrderParallel 5 60 . map parse . lines
