module Main where

import Control.Arrow (Arrow (second), first, (***))
import Control.Monad (join)
import Data.Char (ord)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (partition, sort)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)
import Debug.Trace (traceShowId)

-- TODO: remove anf uncomment part 2 below
samplePt1 =
  "Step C must be finished before step A can begin.\n"
    ++ "Step C must be finished before step F can begin.\n"
    ++ "Step A must be finished before step B can begin.\n"
    ++ "Step A must be finished before step D can begin.\n"
    ++ "Step B must be finished before step E can begin.\n"
    ++ "Step D must be finished before step E can begin.\n"
    ++ "Step F must be finished before step E can begin."

main :: IO ()
main = do
  input <- getContents
  print (part1 samplePt1)
  -- print $ tst samplePt1
  print (part2 input)

parse :: String -> (Char, Char)
parse s = case words s of
  ["Step", from, "must", "be", "finished", "before", "step", to, "can", "begin."] ->
    join (***) asChar (from, to)
  _ -> error $ "Invalid format" ++ s
  where
    asChar [c] = c

depMap :: [(Char, Char)] -> HashMap Char (Set Char)
depMap graphTuples = HM.union initial . HM.fromListWith S.union $ map (second S.singleton . swap) graphTuples
  where
    (from, to) = join (***) S.fromList $ unzip graphTuples
    initial = HM.fromList . map ((,S.empty)) . S.toList $ S.filter (`S.notMember` to) from

toTime :: Char -> Int -> Int
toTime c delay = ord c - ord 'A' + 1 + delay

stepOrderParallel :: Int -> Int -> [(Char, Char)] -> (String, Int)
stepOrderParallel numWorkers delay graphTuples = go ([], 0) (depMap graphTuples) [(0, Nothing) | _ <- [1 .. numWorkers]]
  where
    --  workers must be passed in sorted and at least one must be ready i.e. 0 time left on current job
    go :: ([Char], Int) -> HashMap Char (Set Char) -> [(Int, Maybe Char)] -> (String, Int)
    go (acc, time) deps workers
      | HM.null deps = (reverse acc ++ mapMaybe snd workers, time + fst (last workers))
      | otherwise =
          let ready = map fst . HM.toList $ HM.filter S.null deps
              (busyWorkers, idleWorkers) = traceShowId $ partition (isJust . snd) workers
           in case (idleWorkers, ready) of
                ((0, Nothing) : restIdleWorkers, nextJob : _) ->
                  -- a worker and job are available: don't continue any existing work, just allocate that new work
                  go (acc, time) (HM.delete nextJob deps) (sort $ (toTime nextJob delay, Just nextJob) : restIdleWorkers ++ busyWorkers)
                -- otherwise, advance time until first available job is complete
                _ ->
                  let ((t, Just c) : restBusyWorkers) = busyWorkers
                   in go (c : acc, time + t) (HM.map (S.delete c) deps) (map (first $ max 0 . subtract t) $ (0, Nothing) : idleWorkers ++ restBusyWorkers)

part1 :: String -> String
part1 = fst . stepOrderParallel 1 0 . map parse . lines

part2 :: String -> Int
part2 = snd . stepOrderParallel 5 60 . map parse . lines

tst :: String -> Int
tst = snd . stepOrderParallel 2 0 . map parse . lines
