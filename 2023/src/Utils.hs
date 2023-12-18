module Utils (freqCounts, hexStringToInt, splitBy, quickTrace, withIdx) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Debug.Trace

freqCounts :: (Hashable a) => [a] -> HashMap a Int
freqCounts = foldl update HM.empty
  where
    update countsMap n = HM.insert n (1 + fromMaybe 0 (HM.lookup n countsMap)) countsMap

splitBy :: Char -> String -> [String]
splitBy splitChar str =
  let splitFn = (== splitChar)
   in case dropWhile splitFn str of
        "" -> []
        str' -> word : splitBy splitChar remainder
          where
            (word, remainder) = break splitFn str'

withIdx :: [b] -> [(Int, b)]
withIdx l = zip [0 .. length l] l

quickTrace :: (Show a) => [Char] -> a -> a
quickTrace name value = trace (name ++ " " ++ show value) value

hexStringToInt :: String -> Int
hexStringToInt = foldl (\acc x -> acc * 16 + toNum x) 0
  where
    toNum 'a' = 10
    toNum 'b' = 11
    toNum 'c' = 12
    toNum 'd' = 13
    toNum 'e' = 14
    toNum 'f' = 15
    toNum x = read [x]
