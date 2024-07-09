module Day03 (part1, part2) where

import Control.Arrow ((&&&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Set qualified as S
import Utils (toTuple)
import Prelude hiding (Left, Right)

data Move a = Left a | Right a | Up a | Down a
  deriving (Show)

instance Functor Move where
  fmap f (Left x) = Left $ f x
  fmap f (Right x) = Right $ f x
  fmap f (Up x) = Up $ f x
  fmap f (Down x) = Down $ f x

parse :: String -> [Move Int]
parse = map toMove . splitOn ","
  where
    toMove (c : num) =
      case c of
        'L' -> Left
        'R' -> Right
        'U' -> Up
        'D' -> Down
        $ read num

start :: (Int, Int)
start = (0, 0)

toStepFn :: Move Int -> (Int, Int) -> Int -> (Int, Int)
toStepFn (Left _) (x, y) k' = (x - k', y)
toStepFn (Right _) (x, y) k' = (x + k', y)
toStepFn (Up _) (x, y) k' = (x, y + k')
toStepFn (Down _) (x, y) k' = (x, y - k')

unwrap :: Move Int -> Int
unwrap (Left k) = k
unwrap (Right k) = k
unwrap (Up k) = k
unwrap (Down k) = k

toPathMap :: [Move Int] -> HashMap (Int, Int) Int
toPathMap = (\(m, _, _) -> m) . foldl update (HM.fromList [(start, 0)], start, 0)
  where
    update (path, pos, step) move =
      ( foldr insertIfNotPresent path [(stepFn pos k, step + k) | k <- [1 .. numSteps]],
        stepFn pos numSteps,
        step + numSteps
      )
      where
        (stepFn, numSteps) = (toStepFn &&& unwrap) move
        insertIfNotPresent (k, v) m
          | isJust (HM.lookup k m) = m
          | otherwise = HM.insert k v m

dist :: (Int, Int) -> Int
dist (x, y) = abs x + abs y

part1 :: String -> Int
part1 =
  minimum
    . S.map dist
    . S.delete (0, 0)
    . uncurry S.intersection
    . toTuple
    . map (S.fromList . HM.keys . toPathMap . parse)
    . lines

part2 :: String -> Int
part2 =
  minimum
    . HM.delete (0, 0)
    . uncurry (HM.intersectionWith (+))
    . toTuple
    . map (toPathMap . parse)
    . lines
