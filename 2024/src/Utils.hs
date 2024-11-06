module Utils ((...), lpad, mapTuple, quickTrace, setAt2d, setAt3d, toInt, toTuple, updateAt, withIdx) where

import Control.Arrow ((***))
import Control.Monad (join)
import Debug.Trace (trace)

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

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

infixr 8 ...
(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
f ... g = (f .) . g

setAt2d :: Int -> a -> [a] -> [a]
setAt2d idx x xs = take idx xs ++ [x] ++ drop (idx + 1) xs

setAt3d :: (Int, Int) -> a -> [[a]] -> [[a]]
setAt3d (r, c) x matrix = setAt2d r (setAt2d c x $ matrix !! r) matrix

toInt :: Char -> Int
toInt = read . (: [])

updateAt :: Int -> a -> [a] -> [a]
updateAt idx newVal xs = take idx xs ++ [newVal] ++ drop (idx + 1) xs
