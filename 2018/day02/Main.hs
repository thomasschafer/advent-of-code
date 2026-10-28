module Main where

import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)

import qualified Data.Set as S
import Data.Set (Set)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  -- When you start part 2, uncomment the next line.
  -- print (part2 input)

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

part2 :: String -> Int
part2 = undefined