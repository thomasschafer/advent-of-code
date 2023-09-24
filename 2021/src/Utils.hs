module Utils (splitBy, quickTrace) where

import Debug.Trace

splitBy :: Char -> String -> [String]
splitBy splitChar str =
  let splitFn = (== splitChar)
   in case dropWhile splitFn str of
        "" -> []
        str' -> word : splitBy splitChar remainder
          where
            (word, remainder) = break splitFn str'

quickTrace :: (Show a) => [Char] -> a -> a
quickTrace name value = trace (name ++ " " ++ show value) value
