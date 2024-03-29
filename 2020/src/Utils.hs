module Utils (lpad, mapTuple, quickTrace, withIdx) where

import Control.Arrow ((***))
import Control.Monad (join)
import Debug.Trace

withIdx :: [b] -> [(Int, b)]
withIdx l = zip [0 .. length l] l

quickTrace :: (Show a) => [Char] -> a -> a
quickTrace name value = trace (name ++ " " ++ show value) value

lpad :: a -> Int -> [a] -> [a]
lpad val n xs = replicate (n - length ys) val ++ ys
  where
    ys = take n xs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)
