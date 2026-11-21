module Main where

import Control.Arrow (Arrow (second), (***))
import Control.Monad (join)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)

-- When you start part 2, uncomment the next line.
-- print (part2 input)

parse :: String -> (String, String)
parse s = case words s of
  ["Step", from, "must", "be", "finished", "before", "step", to, "can", "begin."] ->
    (from, to)
  _ -> error $ "Invalid format" ++ s

stepOrder :: [(String, String)] -> String
stepOrder graphTuples = go [] deps
  where
    initial = HM.fromList . map ((,S.empty)) . S.toList $ S.filter (`S.notMember` to) from
      where
        (from, to) = join (***) S.fromList $ unzip graphTuples

    deps = HM.union initial . HM.fromListWith S.union $ map (second S.singleton . swap) graphTuples

    go :: [String] -> HashMap String (Set String) -> String
    go acc d = case popNextEmptyDeps d of
      Nothing -> concat $ reverse acc
      Just (next, d') -> go (next : acc) d'

popNextEmptyDeps :: HashMap String (Set String) -> Maybe (String, HashMap String (Set String))
popNextEmptyDeps deps
  | HM.null deps = Nothing
  | otherwise = Just $ (next, HM.map (S.delete next) $ HM.delete next deps)
  where
    next = head . sort . HM.keys $ HM.filter S.null deps

part1 :: String -> String
part1 = stepOrder . map parse . lines

part2 :: String -> Int
part2 = undefined
