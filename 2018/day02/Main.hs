module Main where

import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)

import qualified Data.Set as S
import Data.Set (Set)

import Debug.Trace (traceShowId)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  putStrLn (part2 input)

counts :: Int -> [Set Int] -> Int
counts n = length . filter (S.member n)

part1 :: String -> Int
part1 s = counts 2 ws * counts 3 ws
  where 
    ws = map (S.fromList . HM.elems . toCharCounts HM.empty) $ lines s
    
    toCharCounts h [] = h
    toCharCounts h (x:xs) = toCharCounts h' xs
      where
        h' = HM.insert x (1 + HM.findWithDefault 0 x h) h

part2 :: String -> String
part2 = filter (/= '-') . findDuplicate S.empty . concatMap (traceShowId . blankOnePerms []) . traceShowId . lines
  where
    blankOnePerms :: [Char] -> [Char] -> [[Char]]
    blankOnePerms prefix [x] = [prefix++"-"]
    blankOnePerms prefix (x:xs) = (prefix++"-"++xs):(blankOnePerms (prefix++[x]) xs)

    findDuplicate :: (Ord a) => Set a -> [a] -> a
    findDuplicate seen (x:xs)
      | x `S.member` seen = x
      | otherwise = findDuplicate (S.insert x seen) xs
