-- Helpers your solutions can share: `import Aoc` to use them, and add your own.
module Aoc
  ( ints
  , grid
  , neighbours4
  , neighbours8
  , withIdx
  , withIndices2d
  , quickTrace
  , pad
  , lpad
  , rpad
  , mapTuple
  , toTuple
  , to3Tuple
  , toList
  , revTuple
  , (...)
  , setAt
  , setAt2d
  , toInt
  , binaryStringToInt
  , hexStringToInt
  , freqCounts
  , safeTail
  , groupBy
  , positionsOf
  , positionOf
  , allEqual
  , chunksOf
  , stripPrefix
  , splitBy
  , splitWhenIncl
  ) where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.Char (digitToInt, isDigit)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

-- | Every integer in the text, including negative ones: ints "x=-3, y=12" == [-3, 12].
ints :: String -> [Int]
ints [] = []
ints ('-' : c : rest) | isDigit c = let (ds, rest') = span isDigit (c : rest) in negate (read ds) : ints rest'
ints (c : rest) | isDigit c = let (ds, rest') = span isDigit (c : rest) in read ds : ints rest'
ints (_ : rest) = ints rest

-- | A grid of characters by (row, column), from lines of text.
grid :: String -> M.Map (Int, Int) Char
grid text = M.fromList [((r, c), ch) | (r, line) <- zip [0 ..] (lines text), (c, ch) <- zip [0 ..] line]

-- | The positions above, below, left and right.
neighbours4 :: (Int, Int) -> [(Int, Int)]
neighbours4 (r, c) = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- | The positions around, diagonals included.
neighbours8 :: (Int, Int) -> [(Int, Int)]
neighbours8 (r, c) = [(r + dr, c + dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0)]

-- | Each element with its index: withIdx "ab" == [(0, 'a'), (1, 'b')].
withIdx :: [a] -> [(Int, a)]
withIdx = zip [0 ..]

-- | Each element of a list of rows with its (row, column).
withIndices2d :: [[a]] -> [((Int, Int), a)]
withIndices2d xs = do
  (i, row) <- zip [0 ..] xs
  (j, x) <- zip [0 ..] row
  pure ((i, j), x)

-- | The value, printed to stderr with a name first, for debugging.
quickTrace :: (Show a) => String -> a -> a
quickTrace name value = trace (name ++ " " ++ show value) value

-- | The first n elements, padded to n with val at the start (if start) or the end.
pad :: Bool -> a -> Int -> [a] -> [a]
pad start val n xs = combine (replicate (n - length ys) val, ys)
  where
    ys = take n xs
    combine = uncurry $ (if start then id else flip) (++)

lpad, rpad :: a -> Int -> [a] -> [a]
lpad = pad True
rpad = pad False

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)
toTuple xs = error ("toTuple: expected 2 elements, got " ++ show (length xs))

to3Tuple :: [a] -> (a, a, a)
to3Tuple [x, y, z] = (x, y, z)
to3Tuple xs = error ("to3Tuple: expected 3 elements, got " ++ show (length xs))

toList :: (a, a) -> [a]
toList (a, b) = [a, b]

revTuple :: (a, b) -> (b, a)
revTuple (x, y) = (y, x)

infixr 8 ...

-- | Composes with a function of two arguments: (f ... g) x y == f (g x y).
(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
f ... g = (f .) . g

-- | The list with the element at idx replaced.
setAt :: Int -> a -> [a] -> [a]
setAt idx x xs = take idx xs ++ [x] ++ drop (idx + 1) xs

-- | The rows with the element at (row, column) replaced.
setAt2d :: (Int, Int) -> a -> [[a]] -> [[a]]
setAt2d (r, c) x matrix = setAt r (setAt c x $ matrix !! r) matrix

-- | A digit's value: toInt '7' == 7.
toInt :: Char -> Int
toInt = digitToInt

binaryStringToInt :: String -> Int
binaryStringToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

-- | A hexadecimal number's value, in lower or upper case.
hexStringToInt :: String -> Int
hexStringToInt = foldl (\acc x -> acc * 16 + digitToInt x) 0

-- | How many times each element appears.
freqCounts :: (Hashable a) => [a] -> HashMap a Int
freqCounts = foldl (\acc x -> HM.insertWith (+) x 1 acc) HM.empty

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs

-- | The elements grouped by key, in no particular order.
groupBy :: (Hashable b) => (a -> b) -> [a] -> [[a]]
groupBy f = map snd . HM.toList . foldl update HM.empty
  where
    update acc x = let key = f x in HM.insert key (x : fromMaybe [] (HM.lookup key acc)) acc

-- | The (row, column) of each element for which p holds.
positionsOf :: (a -> Bool) -> [[a]] -> [(Int, Int)]
positionsOf p rows = [(r, c) | (r, row) <- zip [0 ..] rows, (c, x) <- zip [0 ..] row, p x]

-- | The (row, column) of the one element for which p holds.
positionOf :: (a -> Bool) -> [[a]] -> (Int, Int)
positionOf p rows = case positionsOf p rows of
  [x] -> x
  xs -> error ("positionOf: expected 1 match, got " ++ show (length xs))

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

-- | The list in chunks of n, which must divide its length.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | length xs < n = error "chunksOf: the list's length isn't a multiple of n"
  | otherwise = take n xs : chunksOf n (drop n xs)

-- | The list without any copies of y at its start.
stripPrefix :: (Eq a) => a -> [a] -> [a]
stripPrefix y = dropWhile (== y)

-- | The parts between each splitChar, without empty ones: splitBy ',' "a,,b" == ["a", "b"].
splitBy :: Char -> String -> [String]
splitBy splitChar str = case dropWhile (== splitChar) str of
  "" -> []
  str' -> let (word, rest) = break (== splitChar) str' in word : splitBy splitChar rest

-- | The list split before each element for which f holds, keeping that element.
splitWhenIncl :: (a -> Bool) -> [a] -> [[a]]
splitWhenIncl f = uncurry (:) . foldr step ([], [])
  where
    step x (cur, acc)
      | f x = ([x], cur : acc)
      | otherwise = (x : cur, acc)
